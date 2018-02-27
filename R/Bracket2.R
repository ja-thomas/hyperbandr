#' @title R6 class to create single bracket objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} that consits of multiple algorithm objects
#'
#' @field max.perf [\code{logical()}]\cr
#' TRUE if to maximize the performance (e.g. accuracy of a neural net), 
#' FALSE if to minimize the performance (e.g. find the minimum of the branin function).
#' @field max.ressources [\code{integer()}]\cr
#' The maximum amount of resource that canbe allocated to a single configuration
#' @field prop.discard [\code{integer()}]\cr
#' An input that controls the proportion of configurations discarded in each round of successive halving
#' @field s [\code{integer()}]\cr
#' The s'th bracket object to create. Note that s is in \cr
#' \code{(0,...,floor(log(max.ressources, base = prop.discard)))}
#' @field B [\code{integer()}]\cr
#' The total budget for the bracket. Note that B is given by  \cr
#' \code{(max(s) + 1)*max.ressources}
#' @field id [\code{string}]\cr
#' An id for each Algorithm object in the bracket object
#' @field par.set \cr
#' The parameter set to sample from
#' @field train.fun \cr
#' The function to carry out training
#' @field performance.fun
#' The function to measure the performance
#'
#' @section Methods:
#' \code{$run()} computes the whole bracket \cr
#' \code{$step()} computes one iteration of successive halving \cr
#' \code{$getTopKModels(k)} displays the best k models \cr
#' \code{$filterTopKModels(k)} filters the best k models and deletes the remaining models from the bracket object \cr
#' \code{$getPerformances()} computes the performance of all remaining models \cr
#'
#' @return Bracket object
#' @export
#' @examples

bracketMBO = R6Class("BracketMBO",
  public = list(
    id = NULL,
    par.set = NULL,
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
        par.set, train.fun, performance.fun) {
      self$max.perf = max.perf
      self$id = id
      self$prop.discard = prop.discard
      self$s = s
      self$B = B
      self$n.configs = ceiling((self$B / max.ressources) * (prop.discard^s / (s + 1)))
      self$r.config = max.ressources * prop.discard^(-s)
      self$configurations = par.set
      # initialize the models
      self$models = mapply(function(conf, name) {
        algorithm$new(id = paste(id, name, sep = "."), 
                      configuration = conf,
                      initial.budget = self$getBudgetAllocation(),
                      init.fun = init.fun, 
                      train.fun = train.fun, 
                      performance.fun = performance.fun)
      }, conf = self$configurations, name = seq_len(self$n.configs))
    },
    # method to compute budget allocation at each step of successive halving 
    getBudgetAllocation = function() {
      self$r.config*self$prop.discard^(self$iteration)
    },
    # method to compute the number of models to keep after each step of successive halving 
    getNumberOfModelsToSelect = function() {
      floor(self$n.configs / self$prop.discard)
    },
    # method to compute one step of successive halving
    step = function() {
      self$iteration = self$iteration + 1
      self$filterTopKModels(self$getNumberOfModelsToSelect())
      lapply(self$models, function(x) x$continue(self$getBudgetAllocation()))
      invisible(NULL)
    },
    # method to compute all steps of successive halving (e.g. compute one bracket)
    run = function() {
      for (st in seq_len(self$s)) {
        self$printState()
        self$step()
      }
      self$filterTopKModels(k = 1)
      self$printState()
    },
    # method to obtain the performance of the remaining models
    getPerformances = function() {
      vapply(self$models, function(x) x$getPerformance(), numeric(1))
    },
    # method to display the top k models
    getTopKModels = function(k) {
      if (self$max.perf == TRUE) {
        perfs = self$getPerformances()
        self$models[order(-perfs)][1:k]
      } else {
        perfs = self$getPerformances()
        self$models[order(perfs)][1:k]
      }
    },
    # method to filter the top k models
    filterTopKModels = function(k) {
      self$models = self$getTopKModels(k = k)
      self$n.configs = k
      invisible(NULL)
    },
    # method to print the current state of successive halving
    printState = function() {
      catf("Iteration %i, with %i Algorithms left (Budget: %i)", self$iteration, self$n.configs, 
        self$models[[1]]$current.budget)
    }
  )
)
