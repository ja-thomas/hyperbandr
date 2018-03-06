# R6 class 
## creates data matrix 
## rbinds results

#' @title R6 class to create database objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} to save hyperband results
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

database = R6Class("DataBase",
  public = list(
    data.matrix = NULL,
    col.names = NULL,
    # initialize the database object as a data fame with 0 rows
    initialize = function(configSpace) {
      self$data.matrix = data.frame(matrix(nrow = 0, ncol = length(configSpace$pars) + 1))
      self$col.names = c(names(configSpace$pars), "y")
      colnames(self$data.matrix) = self$col.names
    },
    # method to rbind a new line to the data.matrix
    writeDataBase = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      colnames(self$data.matrix) = self$col.names
    }
  )
)
