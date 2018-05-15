context("Algorithm")

test_that("test if object algorithm works", {
  obj = algorithm$new(
    problem = braninProb,
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
  expect_equal(obj$current.budget, 10)
  # check if configuration are between -5 and 10.1
  expect_numeric(obj$configuration$x1, lower = -5, upper = 10)
  # check if initialized models are between 0 and 15
  expect_numeric(obj$model[[2]], lower = 0, upper = 15)
  # check if performance is equal or greater than 0
  expect_numeric(obj$getPerformance(), lower = 0)

  ## recheck everything after 100 it
  # check if budget is 10 if obj is trained for 10 iterations
  obj$continue(100)
  expect_equal(obj$current.budget, 110)
  # check if configuration are between -5 and 10.1
  expect_numeric(obj$configuration$x1, lower = -5, upper = 10)
  # check if initialized models are between 0 and 15
  expect_numeric(obj$model[[2]], lower = 0, upper = 15)
  # check if performance is equal or greater than 0
  expect_numeric(obj$getPerformance(), lower = 0)
})

