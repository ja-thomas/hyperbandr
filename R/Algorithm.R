algorithms = R6Class("Algorithm",
  public = list(
    id = NULL,
    configuration = NULL,
    model = NULL,
    train.fun = NULL,
    current.budget = NULL,
    performance.fun = NULL,
    initialize = function(id, configuration, init.fun, train.fun, initial.budget, performance.fun) {
      ##FIXME: argchecks
      self$id = id
      self$configuration = configuration
      self$model = init.fun(initial.budget, configuration)
      self$current.budget = initial.budget
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
