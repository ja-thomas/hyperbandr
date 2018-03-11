#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
library("mlrMBO")
library("xgboost")
library("ggplot2")


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

# config space
# configSpace = makeParamSet(
#   makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
#   makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
#   makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6),
#   makeNumericParam("nrounds", lower = 1, upper = 1))

configSpace = makeParamSet(
  makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
  makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6))

# sample fun
sample.fun = function(par.set, n.configs, ...) {
  # sample from configSpace
  if (!exists("bracket.storage4", envir = .GlobalEnv)) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = bracket.storage4,
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
performance.fun = function(model) {
  tail(model$evaluation_log$eval_rmse, n = 1)
}


#######################################
############# applications ############
#######################################

## make xgboost algorithm object
obj = algorithm$new(
  id = "xgboost",
  configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
  initial.budget = 1,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)

# inspect model
obj$model
# inspect performance
obj$getPerformance()
# verify iterations
obj$model$niter
# continue training for 10 iterations
obj$continue(10)
# verify iterations again
obj$model$niter
# inspect performance again
obj$getPerformance()


## make xgboost bracket object
brack = bracket$new(
  max.perf = FALSE,
  max.ressources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "xgboost",
  par.set = configSpace,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)

# inspect configurations
brack$configurations
# run the bracket
brack$run()
# inspect the performance of the best model
brack$getPerformances()


## call hyperband
hyperhyper = hyperband(
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = FALSE,
  export.bracket.storage = TRUE,
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

