# we choose the 2 dimensional branin function
braninProb = smoof::makeBraninFunction()
# the branin function has 3 global minima
param.set = getParamSet(braninProb)
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
