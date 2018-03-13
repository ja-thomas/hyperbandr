#' @title R6 class to create bracketStorage objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} to save the results from each bracket
#' 
#' @field configSpace [\code{string}]\cr
#' A configuration space constructed with makeParamSet from package ParamHelpers
#'
#' @section Methods:
#' \code{$writeDataBase(newline)} rbinds a new row \code{newline} with configurations to the data.matrix  \cr
#'
#' @return Algorithm object
#' @export

# initialize = function(id, configuration, initial.budget, init.fun, train.fun, performance.fun, ...) {
#   self$id = id
#   self$configuration = configuration
#   self$current.budget = initial.budget
#   self$model = init.fun(initial.budget, configuration, ...)
#   self$train.fun = train.fun
#   self$performance.fun = performance.fun
#   
# self$algorithm.result = algorithmStorage$new(self$configuration, self$current.budget, self$model, self$performance.fun)
#   
#   self$algorithm.result = data.frame(matrix(
#     c(unlist(self$configuration), self$current.budget, self$getPerformance()), 
#     ncol = length(self$configuration) + 2))
#   names(self$algorithm.result) = c(names(self$configuration), "current budget", "y")
# },

algorithmStorage = R6Class("algorithmStorage",
  public = list(
    data.matrix = NULL,
    col.names = NULL,
    # initialize the bracketStorage object as a data fame 
    initialize = function(config, budget, model, performance) {
      self$data.matrix = data.frame(matrix(c(unlist(config), budget, performance(model)), 
                                           ncol = length(config) + 2, byrow = TRUE))
      self$col.names = c(names(config), "current budget", "y")
      colnames(self$data.matrix) = self$col.names
    },
    # method to rbind a new line to the data.matrix
    attachLine = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      colnames(self$data.matrix) = self$col.names
    }
  )
)
