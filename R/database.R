# R6 class 
## creates data matrix 
## rbinds results

database = R6Class("DataBase",
  public = list(
    data.matrix = NULL,
    col.names = NULL,
    initialize = function(configSpace) {
      ##FIXME: argchecks
      self$data.matrix = data.frame(matrix(nrow = 0, ncol = length(configSpace$pars) + 1))
      self$col.names = c(names(configSpace$pars), "y")
      colnames(self$data.matrix) = self$col.names
    },
    writeDataBase = function(newline) {
      self$data.matrix = rbind(self$data.matrix, newline)
      colnames(self$data.matrix) = self$col.names
    }
  )
)
