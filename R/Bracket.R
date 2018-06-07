#' @title R6 class to create single bracket objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} that consits of multiple algorithm objects
#'
#' @field max.perf [\code{logical()}]\cr
#' TRUE if to maximize the performance (e.g. accuracy of a neural net),
#' FALSE if to minimize the performance (e.g. find the minimum of the branin function).
#' @field max.resources [\code{integer()}]\cr
#' The maximum amount of resource that can be allocated to a single configuration
#' @field prop.discard [\code{integer()}]\cr
#' An input that controls the proportion of configurations discarded in each round of successive halving
#' @field s [\code{integer()}]\cr
#' The s'th bracket object to create. Note that s is in \cr
#' \code{(0,...,floor(log(max.resources, base = prop.discard)))}
#' @field B [\code{integer()}]\cr
#' The total budget for the bracket. Note that B is given by  \cr
#' \code{(max(s) + 1)*max.resources}
#' @field id [\code{string}]\cr
#' An id for each Algorithm object in the bracket object
#' @field par.set \cr
#' The parameter set to sample from
#' @field sample.fun \cr
#' The function to sample from par.set. If no set, random sampling with \code{\link[ParamHelopers]{sampleValues}} is used.
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
#' @examples
#'
#' # we need some packages
#' library("ggplot2")
#' library("smoof")
#' library("data.table")
#'
#' # we choose the 2 dimensional branin function
#' braninProb = makeBraninFunction()
#'
#' # the branin function has 3 global minima
#' opt = data.table(x1 = getGlobalOptimum(braninProb)$param$x1,
#'   x2 = getGlobalOptimum(braninProb)$param$x2)
#' param.set = getParamSet(braninProb)
#'
#'
#' #######################################
#' ## define functions to use hyperband ##
#' #######################################
#'
#' # config space
#' configSpace = makeParamSet(
#'     makeNumericParam(id = "x1", lower = -5, upper = 10.1))
#'
#' # sample fun
#' sample.fun = function(par.set, n.configs, ...) {
#'   sampleValues(par = par.set, n = n.configs)
#' }
#'
#' # init fun
#' init.fun = function(r, config, problem) {
#'   x1 = unname(unlist(config))
#'   x2 = runif(1, 0, 15)
#'   mod = c(x1, x2)
#'   return(mod)
#' }
#'
#' # train fun
#' train.fun = function(mod, budget, problem) {
#'   for(i in seq_len(budget)) {
#'     mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
#'     if(performance.fun(mod.new) < performance.fun(mod))
#'       mod = mod.new
#'   }
#'   return(mod)
#' }
#'
#' # performance fun
#' performance.fun = function(model, problem) {
#'   braninProb(c(model[[1]], model[[2]]))
#' }
#'###### make branin bracket object #####
#' brack = bracket$new(
#'   problem = braninProb,
#'   max.perf = FALSE,
#'   max.resources = 81,
#'   prop.discard = 3,
#'   s = 4,
#'   B = (4 + 1)*81,
#'   id = "branin",
#'   par.set = configSpace,
#'   sample.fun = sample.fun,
#'   init.fun = init.fun,
#'   train.fun = train.fun,
#'   performance.fun = performance.fun)
#'
#' # the data matrix shows us the hyperparameters, the current budget and the performance
#' brack$bracket.storage$data.matrix
#' # run the bracket
#' brack$run()
#' # inspect the data matrix again
#' brack$bracket.storage$data.matrix
#' # visualize the the bracket
#' # access the performance of the best model
#' brack$getPerformances()
#' @export
bracket = R6Class("Bracket",
  public = list(
    id = NULL,
    par.set = NULL,
    sample.fun = NULL,
    configurations = NULL,
    max.resources = NULL,
    models = NULL,
    prop.discard = NULL,
    s = NULL,
    B = NULL,
    n.configs = NULL,
    r.config = NULL,
    iteration = 0,
    max.perf = NULL,
    bracket.storage = NULL,
    adjust = NULL,
    ## initialize the bracket object
    initialize = function(problem, max.perf, max.resources, prop.discard, s, B, id,
        par.set, sample.fun = function(par.set, n.configs, ...) sampleValues(par = par.set, n = n.configs),
        init.fun, train.fun, performance.fun, ...) {
      self$max.perf = max.perf
      self$id = id
      self$prop.discard = prop.discard
      self$s = s
      self$B = B
      self$n.configs = ceiling((self$B / max.resources) * (prop.discard^s / (s + 1)))
      self$r.config = floor(max.resources * prop.discard^(-s))
      self$par.set = par.set
      self$configurations = sample.fun(self$par.set, self$n.configs, ...)
      # create the models
      self$models = mapply(function(conf, name) {
        algorithm$new(problem,
                      id = paste(id, name, sep = "."),
                      configuration = conf,
                      initial.budget = self$getBudgetAllocation(),
                      init.fun = init.fun,
                      train.fun = train.fun,
                      performance.fun = performance.fun)
      }, conf = self$configurations, name = seq_len(self$n.configs))
      # initialize bracket storage
      self$bracket.storage = bracketStorage$new(self$models)
    },
    ## method to compute budget allocation at each step of successive halving
    getBudgetAllocation = function() {
      self$r.config*self$prop.discard^(self$iteration)
    },
    ## method to compute the number of models to keep after each step of successive halving
    getNumberOfModelsToSelect = function() {
      floor(self$n.configs / self$prop.discard)
    },
    ## method to display the top k models
    getTopKModels = function(k) {
      if (self$max.perf == TRUE) {
        perfs = self$getPerformances()
        self$models[order(-perfs)][1:k]
      } else {
        perfs = self$getPerformances()
        self$models[order(perfs)][1:k]
      }
    },
    ## method to filter the top k models
    filterTopKModels = function(k) {
      self$models = self$getTopKModels(k = k)
      self$n.configs = k
      invisible(NULL)
    },
    ## method to compute one step of successive halving
    step = function() {
      self$adjust = self$getBudgetAllocation()
      self$iteration = self$iteration + 1
      self$filterTopKModels(self$getNumberOfModelsToSelect())
      lapply(self$models, function(x) x$continue(self$getBudgetAllocation() - self$adjust))
      # attach model results to the bracket.storage
      self$bracket.storage$attachLines(bracketStorage$new(self$models)$data.matrix)
      invisible(NULL)
    },
    ## method to compute all steps of successive halving (e.g. compute one bracket)
    run = function() {
      for (st in seq_len(self$s)) {
        self$printState()
        self$step()
      }
      self$filterTopKModels(k = 1)
      self$printState()
    },
    ## method to obtain the performance of the remaining models
    getPerformances = function() {
      vapply(self$models, function(x) x$getPerformance(), numeric(1))
    },
    ## method to print the current state of successive halving
    printState = function() {
      catf("Iteration %i, with %i Algorithms left (Budget: %i)", self$iteration, self$n.configs,
        self$models[[1]]$current.budget)
    }
  )
)
