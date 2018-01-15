library(R6)
library(smoof)
library(devtools)
load_all()

## define the problem to optimize
# conveniently choose a 2 dimensional function "makeBraninFunction"
problem = makeBraninFunction()

# the "makeBraninFunction" has 3 global minima
print(problem)

## smoof functions contain a param.set describing types and bounds of the function parameters
# for the "makeBraninFunction" we have x1 as a hyperparameter, ranging from -5 to 10.15
param.set = getParamSet(problem)

## Thus, for a fix value of x1, we would like to optimize x2
# for the BraninFunction, that is basically the "Wertebreich" along the x2 axis
plot(problem)

# in hyperband-language: the x1 value is our configuration (e.g. a hyperparameter)!
config = runif(1, -5, 10.15)

# define the init.fun to initialize the model (the current value of x1)
init.fun = function(r, config) {
  runif(1, 0, 15)
}

# define the train.fun
train.fun = function(mod, budget) {
  for(i in seq_len(budget)) {
    mod.new = mod + rnorm(1, sd = 3)
    if(performance.fun(mod.new) < performance.fun(mod))
      mod = mod.new
  }
  return(mod)
}

# define the performance.fun
performance.fun = function(model) {
  problem(c(config, model))
}

# with the "new-method" of the factory (this is a default method of each R6 class),
# we create objects of the class. Just call $new() to access the method.

obj = algorithms$new(
  id = "test",
  configuration = config,
  initial.budget = 0,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun
)

obj
obj$configuration
obj$current.budget
obj$id
obj$model
obj$getPerformance()
obj$continue(budget = 1)
obj$model
obj$getPerformance()
obj$continue(budget = 10)
obj$getPerformance()

# another function to sample configurations
sample.fun = function(par.set, n.configs) {
  runif(n = n.configs, -5, 10)
}

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











