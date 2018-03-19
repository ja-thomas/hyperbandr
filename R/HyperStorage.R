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

hyperStorage = R6Class("hyperStorage",
  public = list(
    data.matrix = NULL,
    col.names = NULL,
    # initialize the bracketStorage object as a data fame 
    initialize = function(par.set) {
      #self$data.matrix = bracket$bracket.storage$data.matrix
      self$data.matrix = data.frame(matrix(ncol = length(par.set$pars) + 2, nrow = 0))
      self$col.names = c(names(par.set$pars), "current_budget", "y")
      colnames(self$data.matrix) = self$col.names
    },
    # method to rbind a new line to the data.matrix
    attachLines = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
    }
  )
)
