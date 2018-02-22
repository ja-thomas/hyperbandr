#############################################################################
### R6 class to create algorithm objects
#
## An algorithm object is a generic R6 implementation to optimize 
## problems with anytime algorithms (e.g. neural networks)
#
## inputs:
#
# id: unique id of the algorithm object
# configuration: parameter configuration to use for the algorithm object
# initial.budget: the budget to use for the initialization of the algorithm object
# init.fun: a function initializing the algorithm object
# train.fun: a function training the algorithm object
# performance.fun: a function evaluating the algorithm object
#
## methods:
#
# continue: a function applying train.fun to retrain the model for <budget> iterations
# getPerformance: a function applying performance.fun to evaluate the current model

algorithm = R6Class("Algorithm",
  public = list(
    id = NULL,
    configuration = NULL,
    current.budget = NULL,
    model = NULL,
    train.fun = NULL,
    performance.fun = NULL,
    initialize = function(id, configuration, initial.budget, init.fun, train.fun, performance.fun) {
      ##FIXME: argchecks
      self$id = id
      self$configuration = configuration
      self$current.budget = initial.budget
      self$model = init.fun(initial.budget, configuration)
      self$train.fun = train.fun
      self$performance.fun = performance.fun
    },
    continue = function(budget) {
      ##FIXME: argchecks
      self$model = self$train.fun(self$model, budget)
      self$current.budget = self$current.budget + budget
      invisible(NULL)
    },
    getPerformance = function() {
      self$performance.fun(self$model)
    }
  )
)
