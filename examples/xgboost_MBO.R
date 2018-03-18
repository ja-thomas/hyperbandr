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
library("data.table")


#######################################
#### define the problem to optimize ###
#######################################

data(agaricus.train)
data(agaricus.test)
dtrain = xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
dtest = xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)

 
#######################################
## define functions for hyperbandMBO ##
#######################################

configSpace = makeParamSet(
  makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
  makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6))

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
    designMBO = data.frame(designMBO[, mean(y), by = c("max_depth", "colsample_bytree", "subsample")])
    colnames(designMBO) = colnames(bracket.storage)[-4]
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
init.fun = function(r, config, ...) {
  # watchlist for lazy performance evaluation (ha-ha)
  watchlist = list(eval = dtest, train = dtrain)
  # compute the actual xgboost model
  capture.output({mod = xgb.train(config, dtrain, nrounds = r, watchlist, verbose = 1)})
  return(mod)
}

# train fun
train.fun = function(mod, budget, ...) {
  watchlist = list(eval = dtest, train = dtrain)
  capture.output({mod = xgb.train(xgb_model = mod, 
    nrounds = budget, params = mod$params, dtrain, watchlist, verbose = 1)})
  return(mod)
}

# performance fun
performance.fun = function(model, ...) {
  tail(model$evaluation_log$eval_rmse, n = 1)
}


#######################################
############# applications ############
#######################################

## call hyperband
hyperhyper = hyperband3(
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = FALSE,
  id = "xgboost", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# get performance arbitrary bracket
hyperhyper[[1]]$getPerformances()
hyperhyper[[2]]$getPerformances()
hyperhyper[[3]]$getPerformances()
hyperhyper[[4]]$getPerformances()
hyperhyper[[5]]$getPerformances()

