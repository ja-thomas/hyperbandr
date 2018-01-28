### Script to recreate the results from the hyperband paper

## Packages
library("devtools")
library("R6")
library("mlr")
library("mxnet")
library("reshape2")
library("BBmisc")
library("data.table")
load_all()

## Define the problem
# Read mini_mnist (1/10 of actual mnist for faster evaluation, evenly distributed classes)
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

# Mini-mnist has 10 classes
problem = makeClassifTask(data = mnist, target = "label")

# Each class has 600 samples
print(problem)

# Define the configuration space: see Table 2 in https://arxiv.org/pdf/1603.06560.pdf 
config = makeParamSet(
  makeNumericParam(id = "learning.rate", lower = 1e-3, upper = 1e-1),
  makeIntegerParam(id = "array.batch.size", lower = 1e1, upper = 1e3),
  makeIntegerParam(id = "num.layer1", lower = 5, upper = 30),
  makeIntegerParam(id = "num.layer2", lower = 10L, upper = 60L))

# Define a function to sample configurations from the configuration space
sample.fun = function(par.set, n.configs) {
  lapply(
    sampleValues(
      par = par.set,
      n = n.configs),
    function(x) x[!is.na(x)]
  )
}
  
# Define the init.fun to initialize each model. We set all parameters of the LeNet architecture, 
# in particular those we would like to optimize by par.vals.
init.fun = function(r, config) {
  lrn = makeLearner("classif.mxff", 
    num.layer3 = 500,
    act1 = "tanh", act2 = "tanh", act3 = "tanh", act.out = "softmax",
    conv.layer1 = TRUE, conv.layer2 = TRUE, conv.data.shape = c(28,28),
    conv.kernel1 = c(5,5), conv.stride1 = c(1,1), pool.kernel1 = c(2,2), pool.stride1 = c(2,2),
    conv.kernel2 = c(5,5), conv.stride2 = c(1,1), pool.kernel2 = c(2,2), pool.stride2 = c(2,2),
    begin.round = 1,
    num.round = 1, 
    par.vals = config
  )
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
  pred = predict(object = model, task = problem, subset = test.set)
  performance(pred, measures = acc)
}

# test functions
mod1 = init.fun(r = 1, config = sampleValue(config))
mod1
mod1_perf = performance.fun(mod1)
mod1_perf

mod2 = train.fun(mod1, budget = 5)
mod2
mod2_perf = performance.fun(mod2)
mod2_perf


obj = hyperbandr:::algorithms$new(
  id = "LeNet",
  configuration = sampleValue(config),
  initial.budget = 0,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun
)

brack = bracket$new(
  id = "LeNet",
  par.set = config,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun,
  s = 4,
  B = 405,
  max.ressources = 81,
  prop.discard = 3,
  max.perf = TRUE
)

hyperhyper = hyperband(
  # hyperband
  max.ressources = 81, 
  prop.discard = 3, 
  # new param
  bracket.winner = TRUE,
  max.perf = TRUE, 
  # obj
  #configuration = config, 
  #initial.budget = 0, 
  #init.fun = init.fun,
  # bracket
  id = "LeNet", 
  par.set = config, 
  sample.fun =  sample.fun, 
  train.fun = train.fun, 
  performance.fun = performance.fun
)
