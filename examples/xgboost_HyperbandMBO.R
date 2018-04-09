#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
library("mlrMBO")
library("rgenoud")
library("xgboost")
library("ggplot2")
library("gridExtra")
library("data.table")
library("dplyr")


#######################################
#### define the problem to optimize ###
#######################################

data(agaricus.train)
data(agaricus.test)
dtrain = xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
dtest = xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)

problem = list(train = dtrain, val = dtest)
rm(dtrain)
rm(dtest) 

 
#######################################
## define functions for hyperbandMBO ##
#######################################

configSpace = makeParamSet(
  makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
  makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6))

# sample fun 
sample.fun = function(par.set, n.configs, hyper.storage) {
  # sample from configSpace
  if (dim(hyper.storage)[[1]] == 0) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(hyper.storage)
    designMBO = data.frame(designMBO[, mean(y), by = names(configSpace$pars)])
    colnames(designMBO) = colnames(hyper.storage)[-(length(configSpace$pars) + 1)]
    opt.state = initSMBO(
      par.set = configSpace, 
      design = designMBO,
      control = ctrl,
      minimize = TRUE, 
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
  watchlist = list(eval = problem$val, train = problem$train)
  capture.output({mod = xgb.train(config, problem$train, nrounds = r, watchlist, verbose = 1)})
  return(mod)
}

# train fun
train.fun = function(mod, budget, problem) {
  watchlist = list(eval = problem$val, train = problem$train)
  capture.output({mod = xgb.train(xgb_model = mod, 
    nrounds = budget, params = mod$params, problem$train, watchlist, verbose = 1)})
  return(mod)
}

# performance fun
performance.fun = function(model, problem) {
  tail(model$evaluation_log$eval_rmse, n = 1)
}


#######################################
############# applications ############
#######################################

########### call hyperband ############
hyperhyperMBO = hyperband(
  problem = problem,
  max.resources = 81, 
  prop.discard = 3,  
  max.perf = FALSE,
  id = "xgboost_MBO", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# get performance arbitrary bracket
hyperVis(hyperhyperMBO)
lapply(hyperhyperMBO, function(x) x$getPerformances())

