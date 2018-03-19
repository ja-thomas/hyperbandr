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
sample.fun = function(par.set, n.configs, bracket.storage) {
  # sample from configSpace
  if (dim(bracket.storage)[[1]] == 0) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(bracket.storage)
    designMBO = data.frame(designMBO[, mean(y), by = names(configSpace$pars)])
    colnames(designMBO) = colnames(bracket.storage)[-2]
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

########### call hyperband ############ 
hyperhyperMBO = hyperband(
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = FALSE,
  id = "branin_MBO", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# get performance arbitrary bracket
lapply(hyperhyperMBO, function(x) x$visPerformances())
lapply(hyperhyperMBO, function(x) x$getPerformances())

# visualize results of all brackets
results = data.frame(matrix(nrow = 5, ncol = 2))
for(i in 1:5) {
  results[i, 1] = hyperhyperMBO[[i]]$models[[1]]$model[1]
  for(j in 1:5) {
    results[j, 2] = hyperhyperMBO[[j]]$models[[1]]$model[2]
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

