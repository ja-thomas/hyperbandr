#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
library("mlrMBO")
library("rgenoud")
library("smoof")
library("ggplot2")
library("data.table")
library("dplyr")
library("gridExtra")
library("ggrepel")


####################################
## define the problem to optimize ##
####################################

# we choose the 2 dimensional branin function 
braninProb = makeBraninFunction()

# the branin function has 3 global minima
opt = data.table(x1 = getGlobalOptimum(braninProb)$param$x1, x2 = getGlobalOptimum(braninProb)$param$x2)
(vis = autoplot(braninProb) + geom_point(data = opt, aes(x = x1, y = x2), shape = 20, colour = "red", size = 5))
print(braninProb)

# smoof functions contain a param.set describing types and bounds of the function parameters
(param.set = getParamSet(braninProb))


#######################################
## define functions to use hyperband ##
#######################################

# config space
configSpace = makeParamSet(
    makeNumericParam(id = "x1", lower = -5, upper = 10.1))

# sample fun 
sample.fun = function(par.set, n.configs, hyper.storage) {
  # sample from configSpace
  if (dim(hyper.storage)[[1]] == 0) {
    lapply(sampleValues(par = par.set, n = n.configs), function(x) x[!is.na(x)])
  } else {
  # make MBO from dataBase  
    catf("Proposing points")
    ctrl = makeMBOControl(propose.points = n.configs)
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    designMBO = data.table(hyper.storage)
    designMBO = data.frame(designMBO[, mean(y), by = names(configSpace$pars)])
    colnames(designMBO) = colnames(hyper.storage)[-(length(configSpace$pars) + 1)]
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
init.fun = function(r, config, problem) {
  x1 = unname(unlist(config))
  x2 = runif(1, 0, 15)
  mod = c(x1, x2)
  return(mod)
}

# train fun
train.fun = function(mod, budget, problem) {
  for(i in seq_len(budget)) {
    mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
    if(performance.fun(mod.new) < performance.fun(mod))
      mod = mod.new
  }
  return(mod)
}

# performance fun
performance.fun = function(model, problem) {
  braninProb(c(model[[1]], model[[2]]))
}


#######################################
############# applications ############
#######################################

########### call hyperband ############ 
hyperhyperMBO = hyperband(
  problem = braninProb,
  max.ressources = 81, 
  prop.discard = 3,  
  max.perf = FALSE,
  id = "branin_MBO",
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# visualize the brackets and get the best performance of each bracket
hyperVis(hyperhyperMBO)
lapply(hyperhyperMBO, function(x) x$getPerformances())

# visualize the final results of all brackets
results = lapply(hyperhyperMBO, function(x) x$models[[1]]$model)
data = data.frame(matrix(unlist(results), ncol = 2, byrow = TRUE))
rownames(data) = c("bracket 1", "bracket 2", "bracket 3", "bracket 4", "bracket 5")
colnames(data) = c("x1", "x2")

(vis = vis + 
  geom_point(data = data, mapping = aes(x = x1, y = x2), shape = 3, size = 3) + 
  geom_text_repel(data = data,
                  mapping = aes(x = x1, y = x2, color = factor(x1)),
                  label = rownames(data),
                  max.iter = 10000,
                  force = 3,
                  size = 4,
                  box.padding = unit(5, "lines")) + 
  theme_bw() + 
  theme(legend.position = "none")) + 
  scale_x_continuous(name = "configuration x1") +
  scale_y_continuous(name = "hyperparameter x2")

