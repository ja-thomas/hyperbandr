bracket = R6Class("bracket",
  public = list(
    id = NULL,
    par.set = NULL,
    sample.fun = NULL,
    n.configs = NULL,
    configurations = NULL,
    max.budget = NULL,
    models = NULL,
    nu = NULL,
    s = NULL,
    iteration = 0,
    initialize = function(id, par.set, sample.fun, train.fun, performance.fun, n.configs, max.budget, nu, s) {
      self$id = id
      self$nu = nu
      self$n.configs = n.configs
      self$max.budget = max.budget
      self$configurations = sample.fun(par.set, n.configs)
      self$models = mapply(function(conf, name) {
        algorithms$new(id = paste(id, name, sep = "."), configuration = conf,
          init.fun = init.fun, train.fun = train.fun, initial.budget = self$getBudgetAllocation(),
          performance.fun = performance.fun)
      }, conf = self$configurations, name = seq_len(n.configs))

    },
    getBudgetAllocation = function() {
      self$max.budget*self$nu^(self$iteration) / self$n.configs
    },
    continue = function(budget) {
      lapply(self$models, function(x) x$continue(budget))
    },
    getPerformances = function() {
      vapply(self$models, function(x) x$getPerformance(), numeric(1))
    },
    getTopKModels = function(k) {
      #FIXME minimize or maximize performace
      perfs = self$getPerformances()
      self$models[order(perfs)][1:k]
    }
  )
)
