#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
library("mlrMBO")
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
(vis = autoplot(problem) + geom_point(data = opt, aes(x = x1, y = x2), shape = 20, colour = "red", size = 5))
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
sample.fun = function(par.set, n.configs, ...) {
  # sample from configSpace
  if (!exists("bracket.storage4", envir = .GlobalEnv)) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(bracket.storage4)
    designMBO = data.frame(designMBO[, mean(y), by = x1])
    colnames(designMBO) = colnames(bracket.storage4)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = designMBO,
      control = ctrl,
      minimize = TRUE, 
      noisy = FALSE)
    prop = proposePoints(opt.state)
    propPoints = prop$prop.points
    rownames(propPoints) = c()
    propPoints = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
    return(propPoints)
  }
}

# sample fun 2
sample.fun = function(par.set, n.configs, ...) {
  # sample from configSpace
  if (!exists("bracket.storage4", envir = .GlobalEnv)) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else if (!exists("bracket.storage3", envir = .GlobalEnv)) {
  # make MBO from dataBase  
    catf("Proposing points")
    bracket.storage = bracket.storage4
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(bracket.storage)
    designMBO = data.frame(designMBO[, mean(y), by = x1])
    colnames(designMBO) = colnames(bracket.storage)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = designMBO,
      control = ctrl,
      minimize = TRUE, 
      noisy = FALSE)
    prop = proposePoints(opt.state)
    propPoints = prop$prop.points
    rownames(propPoints) = c()
    propPoints = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
    return(propPoints)
  } else if (!exists("bracket.storage2", envir = .GlobalEnv)) {
  # make MBO from dataBase
    catf("Proposing points")
    bracket.storage = rbind(bracket.storage4, bracket.storage3)
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(bracket.storage)
    designMBO = data.frame(designMBO[, mean(y), by = x1])
    colnames(designMBO) = colnames(bracket.storage)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = designMBO,
      control = ctrl,
      minimize = TRUE, 
      noisy = FALSE)
    prop = proposePoints(opt.state)
    propPoints = prop$prop.points
    rownames(propPoints) = c()
    propPoints = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
    return(propPoints)
  } else if (!exists("bracket.storage1", envir = .GlobalEnv)) {
  # make MBO from dataBase  
    catf("Proposing points")
    bracket.storage = rbind(bracket.storage4, bracket.storage3, bracket.storage2)
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(bracket.storage)
    designMBO = data.frame(designMBO[, mean(y), by = x1]) # als getter in klasse BracketStorage
    colnames(designMBO) = colnames(bracket.storage)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = designMBO,
      control = ctrl,
      minimize = TRUE, 
      noisy = FALSE)
    prop = proposePoints(opt.state)
    propPoints = prop$prop.points
    rownames(propPoints) = c()
    propPoints = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
    return(propPoints)
  } else if (!exists("bracket.storage0", envir = .GlobalEnv)) {
  # make MBO from dataBase  
    catf("Proposing points")
    bracket.storage = rbind(bracket.storage4, bracket.storage3, bracket.storage2, bracket.storage1)
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(bracket.storage)
    designMBO = data.frame(designMBO[, mean(y), by = x1])
    colnames(designMBO) = colnames(bracket.storage)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = designMBO,
      control = ctrl,
      minimize = TRUE, 
      noisy = FALSE)
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
  id = "branin", 
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
results = data.frame(matrix(nrow = 5, ncol = 2))
for(i in 1:5) {
  results[i, 1] = hyperhyper[[i]]$models[[1]]$model[1]
  for(j in 1:5) {
    results[j, 2] = hyperhyper[[j]]$models[[1]]$model[2]
  }
}
rownames(results) = c("bracket 1", "bracket 2", "bracket 3", "bracket 4", "bracket 5")
colnames(results) = c("x1", "x2")

(vis = vis + 
  geom_point(data = results, mapping = aes(x = x1, y = x2), shape = 3, size = 3) + 
  geom_text_repel(data = results,
                  mapping = aes(x = x1, y = x2, color = factor(x1)),
                  label = rownames(results),
                  max.iter = 10000,
                  force = 3,
                  size = 4,
                  box.padding = unit(5, "lines")) + 
  theme_bw() + 
  theme(legend.position = "none")) + 
  scale_x_continuous(name = "configuration x1") +
  scale_y_continuous(name = "hyperparameter x2")

