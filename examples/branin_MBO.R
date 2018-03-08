#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
library("smoof")
library("ggplot2")
library("data.table")

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
  if (!exists("bracket.storage4", envir = .GlobalEnv)) {
    sampleValues(par = par.set, n = n.configs)
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = bracket.storage4, 
      control = ctrl,
      minimize = TRUE, 
      noisy = TRUE)
    prop = proposePoints(opt.state)
    propPoints = prop$prop.points
    rownames(propPoints) = c()
    propPoints = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
    return(propPoints)    
  }
}

# init fun
init.fun = function(r, config) {
  x1 = unname(unlist(config))
  x2 = runif(1, 0, 15)
  mod = c(x1, x2)
  return(mod)
}

# train fun
train.fun = function(mod, budget) {
  for(i in seq_len(budget)) {
    mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
    if(performance.fun(mod.new) < performance.fun(mod))
      mod = mod.new
  }
  return(mod)
}

# performance fun
performance.fun = function(model) {
  problem(c(model[[1]], model[[2]]))
}


#######################################
############# applications ############
#######################################

## make branin algorithm object
obj = algorithm$new(
  id = "branin",
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


## make branin bracket object
brack = bracket$new(
  max.perf = FALSE,
  max.ressources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "branin",
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
hyperhyper[[1]]$getPerformances()
hyperhyper[[2]]$getPerformances()
hyperhyper[[3]]$getPerformances()
hyperhyper[[4]]$getPerformances()
hyperhyper[[5]]$getPerformances()

# visualize results of all brackets
(vis = vis + geom_point(aes(x = hyperhyper[[1]]$models[[1]]$model[1], 
                            y = hyperhyper[[1]]$models[[1]]$model[2]), 
                        shape = 4, colour = "green", size = 5)
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

# make 100 iterations
braninMBOBenchmark = benchmarkThis(10, precision = 6)

# visualize the results
ggplot(stack(braninMBOBenchmark), aes(x = ind, y = values, fill = ind)) + 
  scale_x_discrete(labels=c("bracket 1", "bracket 2", "bracket 3", "bracket 4", "bracket 5")) + 
  theme(legend.position = "none") + labs(x = "", y = "performance") + 
  geom_boxplot()

