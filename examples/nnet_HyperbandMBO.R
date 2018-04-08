#######################################
############## packages ###############
#######################################

setwd("C:/Users/Niklas/hyperbandr")
library("devtools")
load_all()
library("mxnet") 
library("mlr") # you might need to install mxnet branch of mlr: devtools::install_github("mlr-org/mlr", ref = "mxnet")
library("mlrMBO")
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
sample.fun.mbo = function(par.set, n.configs, hyper.storage) {
  # sample from configSpace
  if (dim(hyper.storage)[[1]] == 0) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(hyper.storage)
    designMBO = data.frame(designMBO[, max(y), by = names(configSpace$pars)])
    colnames(designMBO) = colnames(hyper.storage)[-(length(configSpace$pars) + 1)]
    opt.state = initSMBO(
      par.set = configSpace, 
      design = designMBO,
      control = ctrl, 
      minimize = FALSE, 
      noisy = FALSE)
    prop = proposePoints(opt.state)
    propPoints = prop$prop.points
    rownames(propPoints) = c()
    propPoints = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
    return(propPoints)
  }
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

########### call hyperband ################
hyperhyperMBO = hyperband(
  problem = problem, 
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = TRUE,
  id = "nnet", 
  par.set = configSpace, 
  sample.fun =  sample.fun.mbo,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# visualize the brackets
hyperVis(hyperhyperMBO)
# get the best performance of each bracket
max(unlist(lapply(hyperhyperMBO, function(x) x$getPerformances())))
# get the architecture of the best bracket
best.mod = which.max(unlist(lapply(hyperhyperMBO, function(x) x$getPerformances())))
hyperhyperMBO[[best.mod]]$models[[1]]$model

# check the performance of the best bracket on the test set
performance(predict(object = hyperhyperMBO[[best.mod]]$models[[1]]$model, 
                    task = problem$data, 
                    subset = problem$test), 
            measures = acc)

