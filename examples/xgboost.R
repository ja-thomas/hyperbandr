#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
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
## define functions to use hyperband ##
#######################################

# config space
configSpace = makeParamSet(
  makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
  makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6))

# sample fun
sample.fun = function(par.set, n.configs) {
  lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
}

# init fun 
init.fun = function(r, config) {
  watchlist = list(eval = dtest, train = dtrain)
  capture.output({mod = xgb.train(config, dtrain, nrounds = r, watchlist, verbose = 1)})
  return(mod)
}

# train fun
train.fun = function(mod, budget) {
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
  max.perf = FALSE, 
  max.ressources = 81, 
  prop.discard = 3, 
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


## make benchmark experiment
benchmarkThis = function(howManyIt, precision) {
  results = data.frame(matrix(ncol = 5, nrow = howManyIt))
  for (i in 1:howManyIt) {
    catf("Iteration %i", i)
    hyperhyper = hyperband(
      max.perf = FALSE, 
      max.ressources = 81, 
      prop.discard = 3, 
      id = "xgboost", 
      par.set = configSpace, 
      sample.fun =  sample.fun,
      train.fun = train.fun, 
      performance.fun = performance.fun)
    results[i, 1] = round(hyperhyper[[1]]$getPerformances(), digits = precision)
    results[i, 2] = round(hyperhyper[[2]]$getPerformances(), digits = precision)
    results[i, 3] = round(hyperhyper[[3]]$getPerformances(), digits = precision)
    results[i, 4] = round(hyperhyper[[4]]$getPerformances(), digits = precision)
    results[i, 5] = round(hyperhyper[[5]]$getPerformances(), digits = precision)
  }
  return(results)
}

# make 100 iterations
xgboostBenchmark = benchmarkThis(10, precision = 6)

# visualize the results
ggplot(stack(xgboostBenchmark), aes(x = ind, y = values, fill = ind)) + 
  scale_x_discrete(labels=c("bracket 1", "bracket 2", "bracket 3", "bracket 4", "bracket 5")) + 
  theme(legend.position = "none") + labs(x = "", y = "performance") + 
  geom_boxplot()

