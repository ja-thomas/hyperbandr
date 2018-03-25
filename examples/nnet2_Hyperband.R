#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mxnet") 
library("mlr") # you might need to install mxnet branch of mlr: devtools::install_github("mlr-org/mlr", ref = "mxnet")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("data.table")


####################################
## define the problem to optimize ##
####################################

# read mini_mnist (1/10 of actual mnist for faster evaluation, evenly distributed classes)
train = fread("mnist/train.csv", header = TRUE)
test = fread("mnist/test.csv", header = TRUE)

# Some operations to normalize features
mnist = as.data.frame(rbind(train, test))
mnist = mnist[sample(nrow(mnist)), ]
mnist[, 2:785] = lapply(mnist[, 2:785], function(x) x/255) 
rm(train)
rm(test)

# Generate train and test split
train.set = sample(nrow(mnist), size = (2/3)*nrow(mnist))
test.set = setdiff(1:nrow(mnist), train.set)

# mini-mnist has 10 classes
problem = makeClassifTask(data = mnist, target = "label")

# each class has 600 samples
print(problem)


#######################################
## define functions to use hyperband ##
#######################################

# config space
configSpace = makeParamSet(
  makeNumericParam(id = "learning.rate", lower = 0.01, upper = 0.3),
  makeNumericParam(id = "momentum", lower = 0.5, upper = 0.99),
  makeIntegerParam(id = "num.layer1", lower = 1L, upper = 100L),
  makeIntegerParam(id = "num.layer2", lower = 1L, upper = 100L),
  makeIntegerParam(id = "num.layer3", lower = 1L, upper = 100L),
  makeDiscreteParam(id = "act1", c("tanh", "relu", "sigmoid")),
  makeDiscreteParam(id = "act2", c("tanh", "relu", "sigmoid")),
  makeDiscreteParam(id = "act3", c("tanh", "relu", "sigmoid")),
  makeLogicalParam(id = "dropout.global"),
  makeNumericParam(id = "dropout.input", lower = 0.2, upper = 0.8),
  makeLogicalParam(id = "batch.normalization"))
  
# sample fun
sample.fun = function(par.set, n.configs, ...) {
  lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
}

# init fun
init.fun = function(r, config) {
  lrn = makeLearner("classif.mxff",
    layers = 3, conv.layer1 = TRUE, conv.layer2 = TRUE, conv.layer3 = TRUE,
    conv.kernel1 = c(3,3), conv.stride1 = c(1,1), pool.kernel1 = c(2,2), pool.stride1 = c(2,2),
    conv.kernel2 = c(3,3), conv.stride2 = c(1,1), pool.kernel2 = c(2,2), pool.stride2 = c(2,2),
    conv.kernel3 = c(3,3), conv.stride3 = c(1,1), pool.kernel3 = c(2,2), pool.stride3 = c(2,2),
    conv.data.shape = c(sqrt(problem$task.desc$n.feat[[1]]), sqrt(problem$task.desc$n.feat[[1]])),
    begin.round = 1, num.round = r, par.vals = config)
  mod = train(learner = lrn, task = problem, subset = train.set)
  return(mod)
}

# train fun
train.fun = function(mod, budget) {
  lrn = makeLearner("classif.mxff", par.vals = mod$learner$par.vals)
  lrn = setHyperPars(lrn,
    symbol = mod$learner.model$symbol,
    arg.params = mod$learner.model$arg.params,
    aux.params = mod$learner.model$aux.params,
    begin.round = mod$learner$par.vals$begin.round + mod$learner$par.vals$num.round,
    num.round = budget)
  mod = train(learner = lrn, task = problem, subset = train.set)
  return(mod)
}

# performance fun
performance.fun = function(model) {
  pred = predict(model, task = problem, subset = test.set)
  performance(pred, measures = acc)
}

mod1 = init.fun(r = 1, config = sample.fun(par.set = configSpace, n.configs = 1)[[1]])
mod1
mod2 = train.fun(mod = mod1, budget = 3)
mod2
performance.fun(mod1)
performance.fun(mod2)

#######################################
############# applications ############
#######################################

#### make neural net algorithm object ####
obj = algorithm$new(
  id = "nnet",
  configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
  initial.budget = 1,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)

# we can inspect model of our algorithm object
obj$model
# the data matrix shows us the hyperparameters, the current budget and the performance
obj$algorithm.result$data.matrix
# if we are only interested in the performance, we can also call the getPerformance method
obj$getPerformance()
# we can continue training our object for one iteration by calling
obj$continue(1)
# inspect of the data matrix has changed
obj$algorithm.result$data.matrix
# continue training for 18 iterations to obtain a total of 20 iterations
invisible(capture.output(replicate(18, obj$continue(1))))
# inspect model the model again
obj$model
# inspect the data matrix again
obj$algorithm.result$data.matrix
# we can immediately visualize the performance function
obj$visPerformance()

###### make neural net bracket object #####
brack = bracket$new(
  max.perf = TRUE,
  max.ressources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "nnet",
  par.set = configSpace,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)

# the data matrix shows us the hyperparameters, the current budget and the performance
brack$bracket.storage$data.matrix
# run the bracket
brack$run()
# inspect the data matrix again
brack$bracket.storage$data.matrix
# visualize the the bracket
brack$visPerformances()
# access the performance of the best model
brack$getPerformances()

########### call hyperband ############ 
hyperhyper = hyperband(
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = TRUE,
  id = "nnet", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# visualize the brackets and get the best performance of each bracket
hyperVis(hyperhyper)
lapply(hyperhyper, function(x) x$getPerformances())

