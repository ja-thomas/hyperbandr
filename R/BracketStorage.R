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
#' @examples

bracketStorage = R6Class("bracketStorage",
  public = list(
    data.matrix = NULL,
    col.names = NULL,
    # initialize the bracketStorage object as a data fame 
    initialize = function(model.list, par.set) {
      self$data.matrix = data.frame(matrix(unlist(lapply(model.list, function(x) x$algorithm.result)), 
        ncol = (length(par.set$pars) + 1), byrow = TRUE))
      self$col.names = c(names(par.set$pars), "y")
      colnames(self$data.matrix) = self$col.names
    },
    # method to rbind a new line to the data.matrix
    writeBracketStorage = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      colnames(self$data.matrix) = self$col.names
    }
  )
)
