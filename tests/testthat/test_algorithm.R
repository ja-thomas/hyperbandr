context("Algorithm")

test_that("test if object algorithm works", {
  problem = smoof::makeBraninFunction()
  configSpace = ParamHelpers::makeParamSet(
    ParamHelpers::makeNumericParam(id = "x1", lower = -5, upper = 10.1))
  sample.fun = function(par.set, n.configs) {
    ParamHelpers::sampleValues(par = par.set, n = n.configs)
  }
  init.fun = function(r, config) {
    x1 = unname(unlist(config))
    x2 = runif(1, 0, 15)
    mod = c(x1, x2)
    return(mod)
  }
  train.fun = function(mod, budget) {
    for(i in seq_len(budget)) {
      mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
      if(performance.fun(mod.new) < performance.fun(mod))
        mod = mod.new
    }
    return(mod)
  }
  performance.fun = function(model) {
    problem(c(model[[1]], model[[2]]))
  }
  obj = algorithm$new(
    id = "branin",
    configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
    initial.budget = 0,
    init.fun = init.fun,
    train.fun = train.fun,
    performance.fun = performance.fun)
  
  # check if obj is of class R6/Algorithm
  expect_true(is.R6(obj))
  expect_class(obj, "Algorithm")
  
  # check if budget is 10 if obj is trained for 10 iterations
  obj$continue(10)
  expect_integerish(obj$current.budget, lower = 10, upper = 10)
  # check if configuration are between -5 and 10.1
  expect_numeric(obj$configuration$x1, lower = -5, upper = 10.1)
  # check if initialized models are between 0 and 15
  expect_numeric(obj$model[[2]], lower = 0, upper = 15)
  # check if performance is equal or greater than 0
  expect_numeric(obj$getPerformance(), lower = 0)

  ## recheck everything after 100 it  
  # check if budget is 10 if obj is trained for 10 iterations
  obj$continue(100)
  expect_integerish(obj$current.budget, lower = 110, upper = 110)
  # check if configuration are between -5 and 10.1
  expect_numeric(obj$configuration$x1, lower = -5, upper = 10.1)
  # check if initialized models are between 0 and 15
  expect_numeric(obj$model[[2]], lower = 0, upper = 15)
  # check if performance is equal or greater than 0
  expect_numeric(obj$getPerformance(), lower = 0)  
  
  ## recheck everything after 1000 it  
  # check if budget is 10 if obj is trained for 10 iterations
  obj$continue(1000)
  expect_integerish(obj$current.budget, lower = 1110, upper = 1110)
  # check if configuration are between -5 and 10.1
  expect_numeric(obj$configuration$x1, lower = -5, upper = 10.1)
  # check if initialized models are between 0 and 15
  expect_numeric(obj$model[[2]], lower = 0, upper = 15)
  # check if performance is equal or greater than 0
  expect_numeric(obj$getPerformance(), lower = 0) 
})

