#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
library("xgboost")
library("ggplot2")
library("gridExtra")
library("dplyr")


#######################################
#### define the problem to optimize ###
#######################################

data(agaricus.train)
data(agaricus.test)
train.set = xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
test.set = xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)

problem = list(train = train.set, val = test.set)
rm(train.set)
rm(test.set) 

#######################################
## define functions to use hyperband ##
#######################################

# config space
configSpace = makeParamSet(
  makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
  makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6))

# sample fun
sample.fun = function(par.set, n.configs, ...) {
  lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
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

#### make xgboost algorithm object ####
obj = algorithm$new(
  problem = problem,
  id = "xgboost",
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

###### make xgboost bracket object #####
brack = bracket$new(
  problem = problem,
  max.perf = FALSE,
  max.resources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "xgboost",
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
  problem = problem,
  max.resources = 81, 
  prop.discard = 3,  
  max.perf = FALSE,
  id = "xgboost", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# visualize the brackets and get the best performance of each bracket
hyperVis(hyperhyper)
lapply(hyperhyper, function(x) x$getPerformances())

