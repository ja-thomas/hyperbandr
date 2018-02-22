### simple test script for the hyperband package
library("devtools")
library("R6")
library("mlr")
library("mxnet")
library("reshape2")
library("BBmisc")
library("data.table")
load_all()

## define the problem

# read mini_mnist (1/10 of actual mnist for faster evaluation, evenly distributed classes)
train = fread("mnist/train.csv", header = TRUE)
test = fread("mnist/test.csv", header = TRUE)
# Some operations to normalize features
mnist = as.data.frame(rbind(train, test))
mnist = mnist[sample(nrow(mnist)), ]
mnist[, 2:785] = lapply(mnist[, 2:785], function(x) x/255) 
rm(train)
rm(test)
# Generate train and test split
train.set = sample(nrow(mnist), size = (2/3)*nrow(mnist))
test.set = setdiff(1:nrow(mnist), train.set)

# mini-mnist has 10 classes
problem = makeClassifTask(data = mnist, target = "label")

# each class has 600 samples
print(problem)

# config space
configSpace = makeParamSet(
    makeNumericParam(id = "learning.rate", lower = 0.05, upper = 0.3),
    makeNumericParam(id = "momentum", lower = 0.7, upper = 0.99),
    makeIntegerParam(id = "layers", lower = 1L, upper = 1L),
    makeIntegerParam(id = "num.layer1", lower = 1L, upper = 8L),
    makeDiscreteParam(id = "act1", c("tanh", "relu", "sigmoid")))

# sample fun
sample.fun = function(par.set, n.configs) {
  lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
}

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

confs = sample.fun(n.configs = 10)

mod1 = init.fun(r, config = confs[[2]])
mod1
mod1_perf = performance.fun(mod1)
mod1_perf

mod2 = train.fun(mod1, budget = 10)
mod2
mod2_perf = performance.fun(mod2)
mod2_perf

# with the "new-method" of the factory (this is a default method of each R6 class), 
# we create objects of the class. Just call $new() to access the method.

obj = algorithm$new(
  id = "neural_net",
  configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
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
obj$current.budget
obj$model
obj$getPerformance()
obj$continue(budget = 10)
obj$current.budget
obj$getPerformance()

brack = bracket$new(
  max.perf = TRUE,
  max.ressources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "neural_net",
  par.set = configSpace,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun
)

length(brack$models)
brack$models

brack$run()
brack$getPerformances()


hyperhyper = hyperband(
  # hyperband
  max.perf = TRUE, 
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

hyperhyper[[1]]$getPerformances()
hyperhyper[[2]]$getPerformances()
hyperhyper[[3]]$getPerformances()
hyperhyper[[4]]$getPerformances()
hyperhyper[[5]]$getPerformances()








