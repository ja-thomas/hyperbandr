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
#' \code{$visPerformance()} visualizes the performance of the model \cr
#'
#' @return Algorithm object
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
#'
#'
#' #######################################
#' ############# applications ############
#' #######################################
#'
#' #### make branin algorithm object ####
#' obj = algorithm$new(
#'   problem = braninProb,
#'   id = "branin",
#'   configuration = sample.fun(par.set = configSpace, n.configs = 1)[[1]],
#'   initial.budget = 1,
#'   init.fun = init.fun,
#'   train.fun = train.fun,
#'   performance.fun = performance.fun)
#'
#' # we can inspect model of our algorithm object
#' obj$model
#' # the data matrix shows us the hyperparameters, the current budget and the performance
#' obj$algorithm.result$data.matrix
#' # if we are interested in the performance, we can also call the getPerformance method
#' obj$getPerformance()
#' # we can continue training our object for one iteration by calling
#' obj$continue(1)
#' # inspect of the data matrix has changed
#' obj$algorithm.result$data.matrix
#' # continue training for 18 iterations to obtain a total of 20 iterations
#' invisible(capture.output(replicate(18, obj$continue(1))))
#' # inspect model the model again
#' obj$model
#' # inspect the data matrix again
#' obj$algorithm.result$data.matrix
#' # we can immediately visualize the performance function
#' obj$visPerformance()
#' @export
algorithm = R6Class("Algorithm",
  public = list(
    id = NULL,
    configuration = NULL,
    current.budget = NULL,
    model = NULL,
    train.fun = NULL,
    performance.fun = NULL,
    algorithm.result = NULL,
    problem = NULL,
    initialize = function(problem, id, configuration, initial.budget, init.fun, train.fun, performance.fun) {
      self$problem = problem
      self$id = id
      self$configuration = configuration
      self$current.budget = initial.budget
      self$model = init.fun(initial.budget, configuration, problem)
      self$train.fun = train.fun
      self$performance.fun = performance.fun
      self$algorithm.result = algorithmStorage$new(self$configuration, self$current.budget,
        self$model, self$performance.fun, self$problem)
    },
    # method to continue training
    continue = function(budget) {
      self$model = self$train.fun(self$model, budget, self$problem)
      self$current.budget = self$current.budget + budget
      # append the performance to the algorithm storage
      self$algorithm.result$attachLine(algorithmStorage$new(self$configuration, self$current.budget,
        self$model, self$performance.fun, self$problem)$data.matrix)
      invisible(NULL)
    },
    # method to obtain the current performance
    getPerformance = function() {
      self$performance.fun(self$model, self$problem)
    },
    # method to visualize the performance
    visPerformance = function() {
      if (dim(self$algorithm.result$data.matrix)[1] == 1) {
        catf("execute $continue() before using $visPerformance()")
      } else {
        ggplot(data = self$algorithm.result$data.matrix,
          aes(x = current_budget, y = y)) +
        scale_y_continuous(name = "performance") +
        scale_x_continuous(labels = function (x) floor(x), name = "budget") +
        theme(legend.position = "none") +
        theme_minimal() +
        geom_line(size = 0.5, colour = "cyan3")
      }
    }
  )
)
