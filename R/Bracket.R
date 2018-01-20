

bracket = R6Class("bracket",
  public = list(
    id = NULL,
    par.set = NULL,
    sample.fun = NULL,
    configurations = NULL,
    max.ressources = NULL,
    models = NULL,
    prop.discard = NULL,
    s = NULL,
    B = NULL,
    n.configs = NULL,
    r.config = NULL,
    iteration = 0,
    max.perf = NULL,
    initialize = function(id, par.set, sample.fun, train.fun, performance.fun, s, B, max.ressources, prop.discard, max.perf) {
      self$max.perf = max.perf
      self$id = id
      self$prop.discard = prop.discard
      self$n.configs = ceiling((B / max.ressources) * (prop.discard^s / (s + 1)))
      self$r.config = max.ressources * prop.discard^(-s)
      self$s = s
      self$configurations = sample.fun(par.set, self$n.configs)
      self$models = mapply(function(conf, name) {
        algorithms$new(id = paste(id, name, sep = "."), configuration = conf,
          init.fun = init.fun, train.fun = train.fun, initial.budget = self$getBudgetAllocation(),
          performance.fun = performance.fun)
      }, conf = self$configurations, name = seq_len(self$n.configs))
    },
    getBudgetAllocation = function() {
      self$r.config*self$prop.discard^(self$iteration)
    },
    getNumberOfModelsToSelect = function() {
      floor(self$n.configs / self$prop.discard)
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
      self$filterTopKModels(k = 1)
      self$printState()
    },
    getPerformances = function() {
      vapply(self$models, function(x) x$getPerformance(), numeric(1))
    },
    getTopKModels = function(k) {
      if (self$max.perf == TRUE) {
        perfs = self$getPerformances()
        self$models[order(-perfs)][1:k]
      } else {
        perfs = self$getPerformances()
        self$models[order(perfs)][1:k]
      }
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
