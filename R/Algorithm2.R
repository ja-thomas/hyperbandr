#' @title R6 class to create algorithm objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} to be optimized by hyperband
#' 
#' @field id [\code{string}]\cr
#' An id for the Algorithm object
#' @field configuration \cr
#' The configuration to use
#' @field initial.budget \cr
#' The budget to use for the initialization of the model
#' @field init.fun \cr
#' The function to initialize the model
#' @field train.fun \cr
#' The function to carry out training
#' @field performance.fun
#' The function to measure the performance
#'
#' @section Methods:
#' \code{$continue(budget)} continue training for \code{budget} iterations  \cr
#' \code{$getPerformance()} computes the performance of the model \cr
#'
#' @return Algorithm object
#' @export
#' @examples
#' # simple example for the branin function (minimization problem)
#' library("smoof")
#' problem = makeBraninFunction()
#' 
#' # the red crosses are the three global minima:
#' opt = data.table(x1 = getGlobalOptimum(problem)$param$x1, x2 = getGlobalOptimum(problem)$param$x2)
#' (vis = autoplot(problem) 
#'   + geom_point(data = opt, aes(x = x1, y = x2), 
#'                shape = 4, colour = "red", size = 5) ) 
#' 
#' # we choose a random x1 as our hyperpamater and optimize x2
#' configuration = runif(1, -5, 10.1)
#'  
#' # model initialization function:
#' init.fun = function(r, config) {
#'   x1 = unname(unlist(config))
#'   x2 = runif(1, 0, 15)
#'   mod = c(x1, x2)
#'   return(mod)
#' }
#' 
#' # training function:
#' train.fun = function(mod, budget) {
#'   for(i in seq_len(budget)) {
#'     mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
#'     if(performance.fun(mod.new) < performance.fun(mod))
#'       mod = mod.new
#'   }
#'   return(mod)
#' }
#' 
#' # performance function:
#' performance.fun = function(model) {
#'   problem(c(model[[1]], model[[2]]))
#' }
#' 
#' # create the algorithm object
#' obj = algorithm$new(
#'   id = "branin",
#'   configuration = configuration,
#'   initial.budget = 0,
#'   init.fun = init.fun,
#'   train.fun = train.fun,
#'   performance.fun = performance.fun
#' )
#' 
#' # get the performance of the model
#' obj$getPerformance()
#' 
#' # continue training for 100 iterations and get new performance
#' obj$continue(100)
#' obj$getPerformance()
#' 
#' # visualize the results (blue: result of the retrained model)
#' opt = data.table(x1 = getGlobalOptimum(problem)$param$x1, x2 = getGlobalOptimum(problem)$param$x2)
#' (vis = autoplot(problem) 
#'   + geom_point(data = opt, aes(x = x1, y = x2), 
#'                shape = 4, colour = "red", size = 5) 
#'   + geom_point(aes(x = obj$model[[1]], y = obj$model[[2]]), 
#'                shape = 4, colour = "blue", size = 5))

algorithm2 = R6Class("Algorithm2",
  public = list(
    id = NULL,
    configuration = NULL,
    current.budget = NULL,
    model = NULL,
    train.fun = NULL,
    performance.fun = NULL,
    initialize = function(id, configuration, initial.budget, init.fun, train.fun, performance.fun, ...) {
      ##FIXME: argchecks
      self$id = id
      self$configuration = configuration
      self$current.budget = initial.budget
      self$model = init.fun(initial.budget, configuration, ...)
      self$train.fun = train.fun
      self$performance.fun = performance.fun
    },
    continue = function(budget, ...) {
      ##FIXME: argchecks
      self$model = self$train.fun(self$model, budget, ...)
      self$current.budget = self$current.budget + budget
      invisible(NULL)
    },
    getPerformance = function() {
      self$performance.fun(self$model)
    }
  )
)
