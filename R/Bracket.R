#############################################################################
### R6 class to create bracket objects
#
## A bracket object consits of multiple algorithm objects
#
## inputs:
#
# id: unique id of each algorithm object
#
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

#' @title R6 class to create bracket objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} that consits of multiple algorithm objects
#'
#' @field positive [\code{character(1)}]\cr
#'  Only for binary classification: Level of the positive class (\code{NA} otherwise).
#' @field classes [\code{character()}]\cr
#'  Levels of class labels.
#' @field nclasses [\code{integer(1)}]\cr
#'  Number of levels of class labels.
#'
#' @return [\code{\link{TaskClassif}}].
#' @export
#' @examples
#' task = TaskClassif$new("iris", data = iris, target = "Species")
#' task$formula

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
    initialize = function(max.perf, max.ressources, prop.discard, s, B, id, 
                          par.set, sample.fun, train.fun, performance.fun) {
      self$max.perf = max.perf
      self$id = id
      self$prop.discard = prop.discard
      self$s = s
      self$B = B
      
      self$n.configs = ceiling((self$B / max.ressources) * (prop.discard^s / (s + 1)))
      self$r.config = max.ressources * prop.discard^(-s)
      
      self$configurations = sample.fun(par.set, self$n.configs)
      self$models = mapply(function(conf, name) {
        algorithm$new(id = paste(id, name, sep = "."), 
                      configuration = conf,
                      initial.budget = self$getBudgetAllocation(),
                      init.fun = init.fun, 
                      train.fun = train.fun, 
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
