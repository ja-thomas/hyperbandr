context("Bracket")

testBrack = function(brack) {
  # check if brack is of class R6/Bracket
  expect_true(is.R6(brack))
  expect_class(brack, "Bracket")

  # check if number of models generated are correct
  expect_equal(length(brack$models), 81)
  # check if configurations are a list and configurations are lists as well
  expect_list(brack$configurations, type = "list")
  # check if configuration are between -5 and 10.1
  expect_numeric(unlist(brack$configurations), lower = -5, upper = 10.1)
  # check if performances are equal or greater than 0
  expect_numeric(brack$getPerformances(), lower = 0)

  ## recheck everything $run()
  # check if budget is 10 if obj is trained for 10 iterations
  capture_output(brack$run())
  # check if number of models generated are correct
  expect_equal(length(brack$models), 1)
  # check if configurations are a list and configurations are lists as well
  expect_list(brack$configurations, type = "list")
  # check if configuration are between -5 and 10.1
  expect_numeric(unlist(brack$configurations), lower = -5, upper = 10.1)
  # check if performances are equal or greater than 0
  expect_numeric(brack$getPerformances(), lower = 0)
}

test_that("test if object bracket works", {
  testBrack(bracket$new(
    problem = braninProb,
    max.perf = FALSE,
    max.resources = 81,
    prop.discard = 3,
    s = 4,
    B = (4 + 1)*81,
    id = "branin",
    par.set = configSpace,
    sample.fun = sample.fun,
    init.fun = init.fun,
    train.fun = train.fun,
    performance.fun = performance.fun))

  #default sample function
  testBrack(bracket$new(
    problem = braninProb,
    max.perf = FALSE,
    max.resources = 81,
    prop.discard = 3,
    s = 4,
    B = (4 + 1)*81,
    id = "branin",
    par.set = configSpace,
    init.fun = init.fun,
    train.fun = train.fun,
    performance.fun = performance.fun))

})

