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

algorithmStorage = R6Class("algorithmStorage",
  public = list(
    data.matrix = NULL,
    col.names = NULL,
    # initialize the bracketStorage object as a data fame 
    initialize = function(config, budget, model, performance) {
      self$data.matrix = data.frame(matrix(c(unlist(config), 
        budget, performance(model)), ncol = length(config) + 2, byrow = TRUE))
      self$col.names = c(names(config), "current_budget", "y")
      colnames(self$data.matrix) = self$col.names
      # ensure current_budget and y are numerics, else visPerformance does not work
      self$data.matrix$current_budget = as.numeric(as.character(self$data.matrix$current_budget))
      self$data.matrix$y = as.numeric(as.character(self$data.matrix$y))
    },
    # method to rbind a new line to the data.matrix
    attachLine = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      colnames(self$data.matrix) = self$col.names
    }
  )
)
