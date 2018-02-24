context("Bracket")

test_that("test if object bracket works", {
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
  brack = bracket$new(
    max.perf = TRUE,
    max.ressources = 81,
    prop.discard = 3,
    s = 4,
    B = (4 + 1)*81,
    id = "branin",
    par.set = configSpace,
    sample.fun = sample.fun,
    train.fun = train.fun,
    performance.fun = performance.fun)
  
  # check if brack is of class R6/Bracket
  expect_true(is.R6(brack))
  expect_class(brack, "Bracket")
  
  # check if number of models generated are correct
  expect_integerish(length(brack$models), lower = 81, upper = 81)
  # check if configurations are a list and configurations are lists as well
  expect_list(brack$configurations, type = "list")
  # check if configuration are between -5 and 10.1
  expect_numeric(unlist(brack$configurations), lower = -5, upper = 10.1)
  # check if performances are equal or greater than 0
  expect_numeric(brack$getPerformances(), lower = 0)

  ## recheck everything $run() 
  # check if budget is 10 if obj is trained for 10 iterations
  brack$run()
  # check if number of models generated are correct
  expect_integerish(length(brack$models), lower = 1, upper = 1)
  # check if configurations are a list and configurations are lists as well
  expect_list(brack$configurations, type = "list")
  # check if configuration are between -5 and 10.1
  expect_numeric(unlist(brack$configurations), lower = -5, upper = 10.1)
  # check if performances are equal or greater than 0
  expect_numeric(brack$getPerformances(), lower = 0)
})

