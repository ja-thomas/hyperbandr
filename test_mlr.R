### simple test script for the hyperband package
library("devtools")
library("R6")
library("mlr")
library("mxnet")
library("reshape2")
library("BBmisc")
load_all()

## define the problem
# mini-mnist has 10 classes
problem = makeClassifTask(data = mnist, target = "label")

# each class has 600 samples
print(problem)

# in hyperband-language: the x1 value is our configuration (e.g. a hyperparameter)!
config = lapply(
  sampleValue(makeParamSet(
    makeNumericParam(id = "learning.rate", lower = 0.05, upper = 0.3),
    makeNumericParam(id = "momentum", lower = 0.7, upper = 0.99),
    makeIntegerParam(id = "layers", lower = 1L, upper = 1L),
    makeIntegerParam(id = "num.layer1", lower = 1L, upper = 64L),
    makeDiscreteParam(id = "act1", c("tanh", "relu", "sigmoid")))), 
  function(x) x[!is.na(x)]
)

# define the init.fun to initialize the model
init.fun = function(r, config) {
  lrn = makeLearner("classif.mxff", begin.round = 1, num.round = 1, par.vals = config)
  mod = train(learner = lrn, task = problem, subset = train.set)
  return(mod)
}

# define the train.fun, for mxnet: basically retrain function
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

# define the performance.fun
performance.fun = function(model) {
  pred = predict(model, task = problem, subset = test.set)
  performance(pred, measures = acc)
}

# with the "new-method" of the factory (this is a default method of each R6 class), 
# we create objects of the class. Just call $new() to access the method.

obj = hyperbandr:::algorithms$new(
  id = "neural_net",
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
  lapply(
    sampleValues(
      makeParamSet(
        makeNumericParam(id = "learning.rate", lower = 0.05, upper = 0.3),
        makeNumericParam(id = "momentum", lower = 0.7, upper = 0.99),
        makeIntegerParam(id = "layers", lower = 1L, upper = 1L),
        makeIntegerParam(id = "num.layer1", lower = 1L, upper = 64L),
        makeDiscreteParam(id = "act1", c("tanh", "relu", "sigmoid"))),
      n = n.configs),
    function(x) x[!is.na(x)]
  )
}

brack = hyperbandr:::bracket$new(
  id = "bla",
  par.set = NA,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun,
  n.configs = 10,
  max.budget = 100,
  nu = 3
)

brack$models[[1]]$current.budget

lapply(brack$models, function(x) x$getPerformance())

