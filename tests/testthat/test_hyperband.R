context("Hyperband")

test_that("test if object hyperband works", {
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
  hyperhyper = hyperband(
    max.perf = FALSE, 
    max.ressources = 81, 
    prop.discard = 3, 
    id = "neural_net", 
    par.set = configSpace, 
    sample.fun =  sample.fun,
    train.fun = train.fun, 
    performance.fun = performance.fun)
  
  # check if hyperhyper is of type list
  expect_true(is.list(hyperhyper))
  
  # check if we obtain 5 brackets
  expect_integerish(length(hyperhyper), lower = 5, upper = 5)
  
  # check if all brackets have one model left
  expect_integerish(length(hyperhyper[[1]]$models), lower = 1, upper = 1)
  expect_integerish(length(hyperhyper[[2]]$models), lower = 1, upper = 1)
  expect_integerish(length(hyperhyper[[3]]$models), lower = 1, upper = 1)
  expect_integerish(length(hyperhyper[[4]]$models), lower = 1, upper = 1)
  expect_integerish(length(hyperhyper[[5]]$models), lower = 1, upper = 1)
  
  # check if all list elements are of class R6/Bracket
  expect_true(is.R6(hyperhyper[[1]]))
  expect_class(hyperhyper[[1]], "Bracket")
  expect_true(is.R6(hyperhyper[[2]]))
  expect_class(hyperhyper[[2]], "Bracket")
  expect_true(is.R6(hyperhyper[[3]]))
  expect_class(hyperhyper[[3]], "Bracket")
  expect_true(is.R6(hyperhyper[[4]]))
  expect_class(hyperhyper[[4]], "Bracket")
  expect_true(is.R6(hyperhyper[[5]]))
  expect_class(hyperhyper[[5]], "Bracket")
})

