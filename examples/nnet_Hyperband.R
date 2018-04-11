#######################################
############## packages ###############
#######################################

# main packages
library("devtools")
library("mlr") # as of april 2018, you need to install the mxnet branch of mlr: devtools::install_github("mlr-org/mlr", ref = "mxnet")
library("mxnet") 
# helper packages
library("ggplot2")
library("data.table")
library("dplyr")
library("gridExtra")


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
val.set = sample(setdiff(1:nrow(mnist), train.set), 0.5 * (dim(mnist)[[1]] - length(train.set)))
test.set = setdiff(1:nrow(mnist), c(train.set, val.set))

# mini-mnist has 10 classes
task = makeClassifTask(data = mnist, target = "label")

# define the problem
problem = list(data = task, train = train.set, val = val.set, test = test.set)

# each class has 600 samples
print(problem)


#######################################
## define functions to use hyperband ##
#######################################

# config space
configSpace = makeParamSet(
  makeDiscreteParam(id = "optimizer", values = c("sgd", "rmsprop", "adam", "adagrad")),
  makeNumericParam(id = "learning.rate", lower = 0.001, upper = 0.1),
  makeNumericParam(id = "wd", lower = 0, upper = 0.01),
  makeNumericParam(id = "dropout.input", lower = 0, upper = 0.6),
  makeNumericParam(id = "dropout.layer1", lower = 0, upper = 0.6),
  makeNumericParam(id = "dropout.layer2", lower = 0, upper = 0.6),
  makeNumericParam(id = "dropout.layer3", lower = 0, upper = 0.6),
  makeLogicalParam(id = "batch.normalization1"),
  makeLogicalParam(id = "batch.normalization2"),
  makeLogicalParam(id = "batch.normalization3"))

# sample fun
sample.fun = function(par.set, n.configs, ...) {
  lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
}

# init fun
init.fun = function(r, config, problem) {
  lrn = makeLearner("classif.mxff",
    # LeNet architecture: http://deeplearning.net/tutorial/lenet.html 
    layers = 3, 
    conv.layer1 = TRUE, conv.layer2 = TRUE,
    conv.data.shape = c(28, 28),
    num.layer1 = 8, num.layer2 = 16, num.layer3 = 120,
    conv.kernel1 = c(3,3), conv.stride1 = c(1,1), pool.kernel1 = c(2,2), pool.stride1 = c(2,2),
    conv.kernel2 = c(3,3), conv.stride2 = c(1,1), pool.kernel2 = c(2,2), pool.stride2 = c(2,2),           
    array.batch.size = 200,
    begin.round = 1, num.round = r, 
    ctx = mx.gpu(),
    par.vals = config)
  mod = train(learner = lrn, task = problem$data, subset = problem$train)
  return(mod)
}

# train fun
train.fun = function(mod, budget, problem) {
  lrn = makeLearner("classif.mxff", par.vals = mod$learner$par.vals)
  lrn = setHyperPars(lrn,
    symbol = mod$learner.model$symbol,
    arg.params = mod$learner.model$arg.params,
    aux.params = mod$learner.model$aux.params,
    begin.round = mod$learner$par.vals$begin.round + mod$learner$par.vals$num.round,
    num.round = budget,
    ctx = mx.gpu())
  mod = train(learner = lrn, task = problem$data, subset = problem$train)
  return(mod)
}

# performance fun
performance.fun = function(model, problem) {
  pred = predict(model, task = problem$data, subset = problem$val)
  performance(pred, measures = acc)
}


#######################################
############# applications ############
#######################################

#### make neural net algorithm object ####
obj = algorithm$new(
  problem = problem,
  id = "cnn",
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
obj$continue(4)
# inspect of the data matrix has changed
obj$algorithm.result$data.matrix
# continue training for 18 iterations to obtain a total of 20 iterations
invisible(capture.output(replicate(3, obj$continue(5))))
# inspect model the model again
obj$model
# inspect the data matrix again
obj$algorithm.result$data.matrix
# we can immediately visualize the performance function
obj$visPerformance()

###### make neural net bracket object #####
brack = bracket$new(
  problem = problem,
  max.perf = TRUE,
  max.resources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "cnn",
  par.set = configSpace,
  sample.fun = sample.fun,
  init.fun = init.fun,
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

########### call hyperband ################
hyperhyper = hyperband(
  problem = problem, 
  max.resources = 81, 
  prop.discard = 3,  
  max.perf = TRUE,
  id = "cnn", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  init.fun = init.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# visualize the brackets
hyperVis(hyperhyper, perfLimits = c(0, 1))
# get the best performance of each bracket
max(unlist(lapply(hyperhyper, function(x) x$getPerformances())))
# get the architecture of the best bracket
best.mod = which.max(unlist(lapply(hyperhyper, function(x) x$getPerformances())))
hyperhyper[[best.mod]]$models[[1]]$model

# check the performance of the best bracket on the test set
performance(predict(object = hyperhyper[[best.mod]]$models[[1]]$model, 
                    task = problem$data, 
                    subset = problem$test), 
            measures = acc)

