load_all()
library(R6)
library(smoof)

init.fun = function(r, config) {
  runif(1, 0, 15)
}

performance.fun = function(model) {
  problem(c(config, model))
}

train.fun = function(mod, budget) {
  for(i in seq_len(budget)) {
    mod.new = mod + rnorm(1, sd = 3)
    if(performance.fun(mod.new) < performance.fun(mod))
      mod = mod.new
  }
  return(mod)
}

sample.fun = function(par.set, n.configs) {
  runif(n.configs, -5, 10)
}


problem = makeBraninFunction()
param.set = getParamSet(problem)
config = runif(1, -5, 10)


obj = hyperbandr:::algorithms$new(
  id = "test",
  configuration = config,
  init.fun = init.fun,
  train.fun = train.fun,
  initial.budget = 0,
  performance.fun = performance.fun
)


brack = hyperbandr:::bracket$new(
  id = "bla",
  par.set = NA,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun,
  n.configs = 10,
  max.budget = 100,
  nu = 3)
brack$models[[1]]$current.budget

lapply(brack$models, function(x) x$getPerformance())
