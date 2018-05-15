context("Hyperband")

test_that("test if object hyperband works", {

  capture_output({hyperhyper = hyperband(
    problem = braninProb,
    max.resources = 81,
    prop.discard = 3,
    max.perf = FALSE,
    id = "branin",
    par.set = configSpace,
    sample.fun =  sample.fun,
    init.fun = init.fun,
    train.fun = train.fun,
    performance.fun = performance.fun)})


  # check if hyperhyper is of type list
  expect_true(is.list(hyperhyper))

  # check if we obtain 5 brackets
  expect_integerish(length(hyperhyper), lower = 5, upper = 5)

  # check if all brackets have one model left
  lapply(hyperhyper, function(x) {
    expect_equal(length(x$models), 1)
    expect_true(is.R6(x))
    expect_class(x, "Bracket")
  })

})

