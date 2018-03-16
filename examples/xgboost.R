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
# 
obj$algorithm.result$data.matrix
# inspect performance
obj$getPerformance()
# verify iterations
obj$model$niter
# continue training for 10 iterations
for (i in rep(1, 20)) {
  obj$continue(i)
}
# inspect model
obj$model
# 
obj$algorithm.result$data.matrix
# verify iterations again
obj$model$niter
# inspect performance again
obj$getPerformance()
# plot it
ggplot(data = obj$algorithm.result$data.matrix, aes(x = `current budget`, y = y, colour = "midnightblue")) +
  scale_y_continuous(name = "MSE", limits = c(0, 0.4)) + 
  scale_x_continuous(labels = function (x) floor(x), name = "epochs") + 
  labs(colour = "") +
  geom_line() +
  theme(legend.position="none")


## make xgboost bracket object
brack = bracket2$new(
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
# 
brack$bracket.storage$data.matrix
# run the bracket
brack$run()
#
brack$bracket.storage$data.matrix
# inspect the performance of the best model
brack$getPerformances()
# plot it
plotThisBracket = data.table(brack$bracket.storage$data.matrix)
data.frame(plotThisBracket[, mean(y), by = c("max_depth", "colsample_bytree", "subsample")])
ggplot(data = brack$bracket.storage$data.matrix, aes(x = `current budget`, y = y, colour = "midnightblue")) +
  scale_y_continuous(name = "MSE", limits = c(0, 0.4)) + 
  scale_x_continuous(labels = function (x) floor(x), name = "epochs") + 
  labs(colour = "") +
  geom_line() +
  theme(legend.position="none")


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

bla = data.table(hyperhyper[[2]]$data.matrix)
data.frame(bla[, mean(y), by = c("max_depth", "colsample_bytree", "subsample")])

hyperhyper = hyperband3$new(
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = FALSE,
  id = "xgboost", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun
)

# get performance arbitrary bracket
hyperhyper[[1]]$getPerformances()
hyperhyper[[2]]$getPerformances()
hyperhyper[[3]]$getPerformances()
hyperhyper[[4]]$getPerformances()
hyperhyper[[5]]$getPerformances()

