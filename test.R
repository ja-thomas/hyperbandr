load_all()
# we need some packages
library("ggplot2")
library("smoof")
library("data.table")
library("dplyr")
# simple example for the branin function, a minimization problem
problem = makeBraninFunction()
opt = data.table(x1 = getGlobalOptimum(problem)$param$x1, x2 = getGlobalOptimum(problem)$param$x2)
# the three red dots are global minima
autoplot(problem) +
  geom_point(data = opt, aes(x = x1, y = x2), shape = 20, colour = "red", size = 5)
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
  problem(c(model[[1]], model[[2]]))
}

########### call hyperband ############
hyperhyper = hyperband(
  problem = problem,
  max.resources = 81,
  prop.discard = 3,
  max.perf = FALSE,
  id = "branin",
  init.fun = init.fun,
  par.set = configSpace,
  sample.fun =  sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)
# get performance and visualize brackets
lapply(hyperhyper, function(x) x$visPerformances())
lapply(hyperhyper, function(x) x$getPerformances())
