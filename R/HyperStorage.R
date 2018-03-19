#' @title R6 class to create hyperStorage objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} to concatenate the results from each bracket object, automatically created 
#' when the hyperband algorithm is called. Can be used to improve the configuration sample mechanism (e.g. MBO).
#' Check the vignette for a detailed exampled.
#' 
#' @field par.set [\code{string}]\cr
#' The config space
#'
#' @section Methods:
#' \code{$attachLines(newline)} rbinds rows \code{newline} to the data.matrix of the hyperStorage object \cr
#'
#' @return Algorithm object
#' @export


hyperStorage = R6Class("hyperStorage",
  public = list(
    data.matrix = NULL,
    col.names = NULL,
    # initialize the bracketStorage object as a data fame 
    initialize = function(par.set) {
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
