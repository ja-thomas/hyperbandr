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

# we choose x1 as our hyperparameter, ranging from -5 to 10.15
# in hyperband-language: the x1 values are our configuration
config = runif(1, -5, 10.15)

# define the init.fun to initialize the model, e.g. the current value of x2
init.fun = function(r, config) {
  runif(1, 0, 15)
}

# define the train.fun
# we sample from a normal distribution and add the value to our current x2
# we keep the new model if the performance improves
train.fun = function(mod, budget) {
  for(i in seq_len(budget)) {
    mod.new = mod + rnorm(1, sd = 3)
    if(performance.fun(mod.new) < performance.fun(mod))
      mod = mod.new
  }
  return(mod)
}

# thus, we also need a function to evaluate the performance
performance.fun = function(model) {
  problem(c(config, model))
}

# another function to sample configurations
sample.fun = function(par.set, n.configs) {
  runif(n = n.configs, -5, 10)
}

#####################################################################
## time to inspect some of the abilities of the hyperbandr package ##
#####################################################################

# with the "new-method" we create objects of the class algortihms, 
# which is basically one randomly sampled and initialized configuration 
obj = algorithms$new(
  id = "branin",
  configuration = config,
  initial.budget = 0,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun
)

# inspecting the object tells us about the properties of our object
obj

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







brack = bracket$new(
  id = "bla",
  par.set = NA,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun,
  s = 4,
  B = 405,
  max.ressources = 81, 
  prop.discard = 3 
)

length(brack$models)
brack$run()
length(brack$models)
bla = brack$models
brack$filterTopKModels(1)

brack$models[[1]]$current.budget

lapply(brack$models, function(x) x$getPerformance())

hyperhyper = hyperband(
  # hyperband
  max.ressources = 81, 
  prop.discard = 3, 
  # new param
  bracket.winner = TRUE,
  # obj
  #configuration = config, 
  #initial.budget = 0, 
  #init.fun = init.fun,
  # bracket
  id = "test", 
  par.set = NA, 
  sample.fun =  sample.fun, 
  train.fun = train.fun, 
  performance.fun = performance.fun
)











