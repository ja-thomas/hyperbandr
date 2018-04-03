#' @title R6 class to create bracketStorage objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} to save the results from each bracket object, automatically created 
#' when a new bracket object is created.
#' 
#' @field models [\code{string}]\cr
#' A list of models
#'
#' @section Methods:
#' \code{$attachLines(newline)} rbinds rows \code{newline} to the data.matrix of the bracketStorage object \cr
#'
#' @return Algorithm object
#' @export


bracketStorage = R6Class("BracketStorage",
  public = list(
    data.matrix = NULL,
    # initialize the bracketStorage object as a data fame
    initialize = function(models) {
      self$data.matrix = do.call(rbind, lapply(models, 
        function(x) tail(x$algorithm.result$data.matrix, n = 1)))
    },
    # method to rbind a new line to the
    attachLines = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      rownames(self$data.matrix) = 1:dim(self$data.matrix)[[1]]  
    }
  )
)
