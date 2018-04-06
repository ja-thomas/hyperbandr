#######################################
############## packages ###############
#######################################

library("devtools")
load_all()
library("mlr")
library("smoof")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("data.table")
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
sample.fun = function(par.set, n.configs, ...) {
  sampleValues(par = par.set, n = n.configs)
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

#### make branin algorithm object ####
obj = algorithm$new(
  problem = braninProb,
  id = "branin",
  configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
  initial.budget = 1,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)

# we can inspect model of our algorithm object
obj$model
# the data matrix shows us the hyperparameters, the current budget and the performance
obj$algorithm.result$data.matrix
# if we are only interested in the performance, we can also call the getPerformance method
obj$getPerformance()
# we can continue training our object for one iteration by calling
obj$continue(1)
# inspect of the data matrix has changed
obj$algorithm.result$data.matrix
# continue training for 18 iterations to obtain a total of 20 iterations
invisible(capture.output(replicate(18, obj$continue(1))))
# inspect model the model again
obj$model
# inspect the data matrix again
obj$algorithm.result$data.matrix
# we can immediately visualize the performance function
obj$visPerformance()

###### make branin bracket object #####
brack = bracket$new(
  problem = braninProb, 
  max.perf = FALSE,
  max.ressources = 200,
  prop.discard = 4,
  s = 4,
  B = (4 + 1)*200,
  id = "branin",
  par.set = configSpace,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)

# the data matrix shows us the hyperparameters, the current budget and the performance
brack$bracket.storage$data.matrix
# run the bracket
brack$run()
# inspect the data matrix again
brack$bracket.storage$data.matrix
# visualize the the bracket
brack$visPerformances()
# access the performance of the best model
brack$getPerformances()

########### call hyperband ############ 
hyperhyper = hyperband(
  problem = braninProb,
  max.ressources = 500, 
  prop.discard = 3,  
  max.perf = FALSE,
  id = "branin", 
  par.set = configSpace, 
  sample.fun =  sample.fun,
  train.fun = train.fun, 
  performance.fun = performance.fun)

# visualize the brackets and get the best performance of each bracket
hyperVis(hyperhyper)
lapply(hyperhyper, function(x) x$getPerformances())

# visualize the final results of all brackets
results = lapply(hyperhyper, function(x) x$models[[1]]$model)
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

