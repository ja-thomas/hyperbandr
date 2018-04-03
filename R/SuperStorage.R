#' @title R6 class to create algorithmStorage objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} to save the results from each algorithm object, automatically created 
#' when a new algorithm object is created.
#' 
#' @field config [\code{string}]\cr
#' An algorithm configuration
#' @field budget [\code{string}]\cr
#' The current budget
#' @field model [\code{string}]\cr
#' A model
#' @field performance [\code{string}]\cr
#' A performance function
#'
#' @section Methods:
#' \code{$attachLines(newline)} rbind a new rows \code{newline} to the data.matrix of the algorithmStorage object \cr
#'
#' @return Algorithm object
#' @export


superStorage = R6Class("SuperStorage",
  public = list(
    # method to rbind a new line to the data.matrix
    attachLines = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      colnames(self$data.matrix) = self$col.names
    }
  )
)
