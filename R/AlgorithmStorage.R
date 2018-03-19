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
    row.names = NULL,
    # initialize the bracketStorage object as a data fame 
    initialize = function(config, budget, model, performance) {
      self$data.matrix = data.frame(config, budget, performance(model))
      self$col.names = c(names(config), "current_budget", "y")
      self$row.names = 1:length(dim(self$data.matrix)[[1]])
      colnames(self$data.matrix) = self$col.names
      rownames(self$data.matrix) = self$row.names
    },
    # method to rbind a new line to the data.matrix
    attachLine = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      colnames(self$data.matrix) = self$col.names
    }
  )
)
