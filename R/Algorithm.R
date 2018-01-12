#############################################################################
### R6 class to create algorithm objects.
#
## inputs:
#
# id: unique id of each algorithm object
# configuration: parameter configuration of each algorithm object
# initial.budget: the available budget to train the algorithm object
# init.fun:
# train.fun:
# performance.fun:

algorithms = R6Class("Algorithm", # the name of the class
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
