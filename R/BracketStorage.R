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

bracketStorage = R6Class("bracketStorage",
  public = list(
    data.matrix = NULL,
    #col.names = NULL,
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
