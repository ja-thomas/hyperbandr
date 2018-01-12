bracket = R6Class("bracket",
  public = list(
    id = NULL,
    par.set = NULL,
    sample.fun = NULL,
    n.configs = NULL,
    configurations = NULL,
    R = NULL,
    models = NULL,
    nu = NULL,
    s = NULL,
    B = NULL,
    r = NULL,
    iteration = 0,
    initialize = function(id, par.set, sample.fun, train.fun, performance.fun, s, B, R, nu) {
      self$id = id
      self$nu = nu
      self$n.configs = ceiling((B / R) * (nu^s / (s + 1)))
      self$r = R * nu^(-s)
      self$s = s
      self$configurations = sample.fun(par.set, self$n.configs)
      self$models = mapply(function(conf, name) {
        algorithms$new(id = paste(id, name, sep = "."), configuration = conf,
          init.fun = init.fun, train.fun = train.fun, initial.budget = self$getBudgetAllocation(),
          performance.fun = performance.fun)
      }, conf = self$configurations, name = seq_len(self$n.configs))

    },
    getBudgetAllocation = function() {
      self$r*self$nu^(self$iteration)
    },
    getNumberOfModelsToSelect = function() {
      floor(self$n.configs / self$nu)
    },
    step = function() {
      self$iteration = self$iteration + 1
      self$filterTopKModels(self$getNumberOfModelsToSelect())
      lapply(self$models, function(x) x$continue(self$getBudgetAllocation()))
      invisible(NULL)
    },
    run = function() {
      for (st in seq_len(self$s)) {
        self$printState()
        self$step()
      }
      self$printState()
    },
    getPerformances = function() {
      vapply(self$models, function(x) x$getPerformance(), numeric(1))
    },
    getTopKModels = function(k) {
      #FIXME minimize or maximize performace
      perfs = self$getPerformances()
      self$models[order(perfs)][1:k]
    },
    filterTopKModels = function(k) {
      self$models = self$getTopKModels(k = k)
      self$n.configs = k
      invisible(NULL)
    },
    printState = function() {
      catf("Iteration %i, with %i Algorithms left (Budget: %i)", self$iteration, self$n.configs, self$models[[1]]$current.budget)
    }
  )
)
