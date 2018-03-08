#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mxnet") 
library("mlr") # you might need to install mxnet branch of mlr: devtools::install_github("mlr-org/mlr", ref = "mxnet")
library("ggplot2")
library("data.table")


####################################
## define the problem to optimize ##
####################################

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

#######################################
## define functions to use hyperband ##
#######################################

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

# init fun
init.fun = function(r, config) {
  lrn = makeLearner("classif.mxff", begin.round = 1, num.round = r, par.vals = config)
  mod = train(learner = lrn, task = problem, subset = train.set)
  return(mod)
}

# train fun
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

# performance fun
performance.fun = function(model) {
  pred = predict(model, task = problem, subset = test.set)
  performance(pred, measures = acc)
}

#######################################
############# applications ############
#######################################

## make neural net algorithm object
obj = algorithm$new(
  id = "neural_net",
  configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
  initial.budget = 0,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)

# inspect model
obj$model
# inspect performance
obj$getPerformance()
# continue training for 10 iterations
obj$continue(10)
# inspect performance again
obj$getPerformance()


## make neural net bracket object
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
hyperhyper[[3]]$getPerformances()


## make benchmark experiment 
benchmarkThis = function(howManyIt, precision) {
  results = data.frame(matrix(ncol = 5, nrow = howManyIt))
  for (i in 1:howManyIt) {
    catf("Iteration %i", i)
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
    results[i, 1] = round(hyperhyper[[1]]$getPerformances(), digits = precision)
    results[i, 2] = round(hyperhyper[[2]]$getPerformances(), digits = precision)
    results[i, 3] = round(hyperhyper[[3]]$getPerformances(), digits = precision)
    results[i, 4] = round(hyperhyper[[4]]$getPerformances(), digits = precision)
    results[i, 5] = round(hyperhyper[[5]]$getPerformances(), digits = precision)
  }
  return(results)
}

# make 10 iterations (depending on your hardware this might take some time)
myNeuralNetBenchmark = benchmarkThis(10, precision = 6)

# visualize the results
ggplot(stack(myNeuralNetBenchmark), aes(x = ind, y = values, fill = ind)) + 
  scale_x_discrete(labels=c("bracket 1", "bracket 2", "bracket 3", "bracket 4", "bracket 5")) + 
  theme(legend.position = "none") + labs(x = "", y = "performance") + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_boxplot()

