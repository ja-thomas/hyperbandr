### simple test script for the hyperband package
library("devtools")
library("R6")
library("mlr")
library("mxnet")
library("reshape2")
library("BBmisc")
library("smoof")
library("ggplot2")
library("data.table")
load_all()

####################################
## define the problem to optimize ##
####################################

# we choose the 2 dimensional branin function 
problem = makeBraninFunction()

# the branin function has 3 global minima
opt = data.table(x1 = getGlobalOptimum(problem)$param$x1, x2 = getGlobalOptimum(problem)$param$x2)
(vis = autoplot(problem) + geom_point(data = opt, aes(x = x1, y = x2), shape = 4, colour = "red", size = 5))
print(problem)

# smoof functions contain a param.set describing types and bounds of the function parameters
(param.set = getParamSet(problem))

#######################################
## define functions to use hyperband ##
#######################################

# config space
configSpace = makeParamSet(
    makeNumericParam(id = "x1", lower = -5, upper = 10.1))

# sample fun
sample.fun = function(par.set, n.configs) {
  sampleValues(par = par.set, n = n.configs)
}

config = sample.fun(configSpace, 1)
# init fun
init.fun = function(r, config) {
  x1 = unname(unlist(config))
  x2 = runif(1, 0, 15)
  mod = c(x1, x2)
  return(mod)
}

mod = init.fun(config = config)
#
train.fun = function(mod, budget) {
  for(i in seq_len(budget)) {
    mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
    if(performance.fun(mod.new) < performance.fun(mod))
      mod = mod.new
  }
  return(mod)
}

# thus, we also need a function to evaluate the performance
performance.fun = function(model) {
  problem(c(model[[1]], model[[2]]))
}

#
obj = algorithm$new(
  id = "branin",
  configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
  initial.budget = 0,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun
)

obj$configuration
obj$model
obj$getPerformance()

brack = bracket$new(
  max.perf = TRUE,
  max.ressources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "branin",
  par.set = configSpace,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun
)

#####################################################################
## time to inspect some of the abilities of the hyperbandr package ##
#####################################################################

# we can add the current configuration to our plot
(vis = vis + geom_point(aes(x = obj$configuration, y = obj$model), shape = 4, colour = "blue", size = 5))

# to evaluate the model, we call the getPerformance function
obj$getPerformance()

# now its time to retrain the model for some iterations
obj$continue(budget = 10)
obj$getPerformance()

# let us inspect where the model has moved to 
(vis = vis + geom_point(aes(x = obj$configuration, y = obj$model), shape = 4, colour = "blue", size = 5))

# we continue for another 1000 iterations
obj$continue(budget = 1000)
obj$getPerformance()
(vis = vis + geom_point(aes(x = obj$configuration, y = obj$model), shape = 4, colour = "blue", size = 5))




hyperhyper = hyperband(
  # hyperband
  max.perf = FALSE, 
  max.ressources = 81, 
  prop.discard = 3, 
  # new param
  #bracket.winner = TRUE,
  id = "neural_net", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  #init.fun = init.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun
)

# for plotting
results = data.frame(matrix(ncol = 10, nrow = 10))
colnames(results) = c("b1: x1", "b1: x2", "b2: x1", "b2: x2", "b3: x1", 
                      "b3: x2", "b4: x1", "b4: x2", "b5: x1", "b5: x2")
results[1, 1] = round(hyperhyper[[1]]$models[[1]]$model[[1]], digits = 2)
results[1, 2] = round(hyperhyper[[1]]$models[[1]]$model[[2]], digits = 2)
results[1, 3] = round(hyperhyper[[2]]$models[[1]]$model[[1]], digits = 2)
results[1, 4] = round(hyperhyper[[2]]$models[[1]]$model[[2]], digits = 2)
results[1, 5] = round(hyperhyper[[3]]$models[[1]]$model[[1]], digits = 2)
results[1, 6] = round(hyperhyper[[3]]$models[[1]]$model[[2]], digits = 2)
results[1, 7] = round(hyperhyper[[4]]$models[[1]]$model[[1]], digits = 2)
results[1, 8] = round(hyperhyper[[4]]$models[[1]]$model[[2]], digits = 2)
results[1, 9] = round(hyperhyper[[5]]$models[[1]]$model[[1]], digits = 2)
results[1, 10] = round(hyperhyper[[5]]$models[[1]]$model[[2]], digits = 2)

vis = vis + geom_point(data = results(x=quantile(TestDf$Values, percentiles),
              y=percentiles), aes(x=x, y=y))

(vis = vis + geom_point(aes(x = hyperhyper[[1]]$models[[1]]$model[1], 
                            y = hyperhyper[[1]]$models[[1]]$model[2]), 
                        shape = 4, colour = "blue", size = 5)
  + geom_point(aes(x = hyperhyper[[2]]$models[[1]]$model[1],
                   y = hyperhyper[[2]]$models[[1]]$model[2]),
               shape = 4, colour = "blue", size = 5) 
  + geom_point(aes(x = hyperhyper[[3]]$models[[1]]$model[1],
                   y = hyperhyper[[3]]$models[[1]]$model[2]),
               shape = 4, colour = "blue", size = 5)
  + geom_point(aes(x = hyperhyper[[4]]$models[[1]]$model[1],
                   y = hyperhyper[[4]]$models[[1]]$model[2]),
               shape = 4, colour = "blue", size = 5)
  + geom_point(aes(x = hyperhyper[[5]]$models[[1]]$model[1],
                   y = hyperhyper[[5]]$models[[1]]$model[2]),
               shape = 4, colour = "blue", size = 5))

# benchmark:

benchmarkThis = function(howManyIt = 10L) {
  results = data.frame(matrix(ncol = 5, nrow = howManyIt))
  #colnames(results) = c("bracket 1", "bracket 2", "bracket 3", "bracket 4", "bracket 5")
  for (i in 1:howManyIt) {
    catf("Iteration %i", i)
    hyperhyper = hyperband(
      max.perf = FALSE, 
      max.ressources = 81, 
      prop.discard = 3, 
      id = "branin", 
      par.set = configSpace, 
      sample.fun =  sample.fun,
      train.fun = train.fun, 
      performance.fun = performance.fun)
    results[i, 1] = round(hyperhyper[[1]]$getPerformances(), digits = 2)
    results[i, 2] = round(hyperhyper[[2]]$getPerformances(), digits = 2)
    results[i, 3] = round(hyperhyper[[3]]$getPerformances(), digits = 2)
    results[i, 4] = round(hyperhyper[[4]]$getPerformances(), digits = 2)
    results[i, 5] = round(hyperhyper[[5]]$getPerformances(), digits = 2)
  }
  return(results)
}
myBraninBenchmark = benchmarkThis(100)

ggplot(stack(myBraninBenchmark), aes(x = ind, y = values, fill = ind)) + 
  scale_x_discrete(labels=c("bracket 1","bracket 2","bracket 3","bracket 4", "bracket 5")) + 
  theme(legend.position = "none") + labs(x = "", y = "performance") + 
  geom_boxplot()

# other
hyperhyper[[1]]
hyperhyper[[1]]$models
hyperhyper[[1]]$models[[1]]$configuration
hyperhyper[[1]]$models[[1]]$model
hyperhyper[[1]]$models[[1]]$getPerformance()







