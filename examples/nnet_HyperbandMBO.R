#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mxnet") 
library("mlr") # you might need to install mxnet branch of mlr: devtools::install_github("mlr-org/mlr", ref = "mxnet")
library("mlrMBO")
library("randomForest") # for MBO
library("ggplot2")
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
  makeNumericParam(id = "learning.rate", lower = 0.01, upper = 0.5),
  makeNumericParam(id = "momentum", lower = 0.1, upper = 0.99),
  makeLogicalParam(id = "dropout.global"),
  makeNumericParam(id = "dropout.input", lower = 0.2, upper = 0.8),
  makeLogicalParam(id = "batch.normalization"))

# sample fun 
sample.fun = function(par.set, n.configs, bracket.storage) {
  # sample from configSpace
  if (dim(bracket.storage)[[1]] == 0) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(bracket.storage)
    designMBO = data.frame(designMBO[, mean(y), by = names(configSpace$pars)])
    colnames(designMBO) = colnames(bracket.storage)[-6]
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
init.fun = function(r, config) {
  lrn = makeLearner("classif.mxff", 
          layers = 2, num.layer1 = 16, num.layer2 = 32,
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

#######################################
############# applications ############
#######################################

########### call hyperband ############ 
hyperhyperMBO = hyperband(
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = TRUE,
  id = "nnet", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# visualize the brackets and get the best performance of each bracket
hyperVis(hyperhyperMBO)
lapply(hyperhyperMBO, function(x) x$getPerformances())

