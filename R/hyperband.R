#' @title Hyperband
#'
#' @description
#' Runs hyperband
#'
#' @param max.perf [\code{logical()}]\cr
#' TRUE if to maximize the performance (e.g. accuracy of a neural net), 
#' FALSE if to minimize the performance (e.g. find the minimum of the branin function). \cr
#' Default: TRUE
#' @param max.ressources [\code{integer()}]\cr
#' The maximum amount of resource that canbe allocated to a single configuration
#' @param prop.discard [\code{integer()}]\cr
#' An input that controls the proportion of configurations discarded in each round of successive halving
#' @param id [\code{string}]\cr
#' An id for each Algorithm object in the bracket object
#' @param par.set \cr
#' The parameter set to sample from
#' @param sample.fun \cr
#' The function to sample from par.set
#' @param train.fun \cr
#' The function to carry out training
#' @param performance.fun
#' The function to measure the performance
#'   
#' @return List of brackets
#' @export
#' @examples
#' # simple example for the branin function (minimization problem)
#' library("smoof")
#' problem = makeBraninFunction()
#' 
#' # configuration space:
#' configSpace = makeParamSet(
#'   makeNumericParam(id = "x1", lower = -5, upper = 10.1))
#'   
#' # sampling function:
#' sample.fun = function(par.set, n.configs) {
#'  sampleValues(par = par.set, n = n.configs)
#' }
#'  
#' # model initialization function:
#' init.fun = function(r, config) {
#'   x1 = unname(unlist(config))
#'   x2 = runif(1, 0, 15)
#'   mod = c(x1, x2)
#'   return(mod)
#' }

#' # training function:
#' train.fun = function(mod, budget) {
#'   for(i in seq_len(budget)) {
#'     mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
#'     if(performance.fun(mod.new) < performance.fun(mod))
#'       mod = mod.new
#'   }
#'   return(mod)
#' }
#' 
#' # performance function:
#' performance.fun = function(model) {
#'   problem(c(model[[1]], model[[2]]))
#' }
#' 
#' # compute hyperband:
#' hyperhyper = hyperband(
#'   # hyperband
#'   max.perf = FALSE, 
#'   max.ressources = 81, 
#'   prop.discard = 3, 
#'   id = "neural_net", 
#'   par.set = configSpace, 
#'   sample.fun =  sample.fun,
#'   train.fun = train.fun, 
#'   performance.fun = performance.fun
#' )
#' 
#' # visualize the results (red: global minima, blue: result of each bracket)
#' opt = data.table(x1 = getGlobalOptimum(problem)$param$x1, x2 = getGlobalOptimum(problem)$param$x2)
#' (vis = autoplot(problem) 
#'   + geom_point(data = opt, aes(x = x1, y = x2), 
#'                shape = 4, colour = "red", size = 5)
#'   + geom_point(aes(x = hyperhyper[[1]]$models[[1]]$model[1], 
#'                             y = hyperhyper[[1]]$models[[1]]$model[2]), 
#'                         shape = 4, colour = "blue", size = 5)
#'   + geom_point(aes(x = hyperhyper[[2]]$models[[1]]$model[1],
#'                    y = hyperhyper[[2]]$models[[1]]$model[2]),
#'                shape = 4, colour = "blue", size = 5) 
#'   + geom_point(aes(x = hyperhyper[[3]]$models[[1]]$model[1],
#'                    y = hyperhyper[[3]]$models[[1]]$model[2]),
#'                shape = 4, colour = "blue", size = 5)
#'   + geom_point(aes(x = hyperhyper[[4]]$models[[1]]$model[1],
#'                    y = hyperhyper[[4]]$models[[1]]$model[2]),
#'                shape = 4, colour = "blue", size = 5)
#'   + geom_point(aes(x = hyperhyper[[5]]$models[[1]]$model[1],
#'                    y = hyperhyper[[5]]$models[[1]]$model[2]),
#'                shape = 4, colour = "blue", size = 5))

hyperband = function(max.ressources = 81, prop.discard = 3, 
  max.perf = TRUE, export.bracket.storage = FALSE, id, 
  par.set,  sample.fun, train.fun, performance.fun) { 
  # |sMax + 1| are the total number of brackets to try 
  sMax =  floor(log(max.ressources, base = prop.discard))
  B = (sMax + 1)*max.ressources
  # initialize a list for all #sMax brackets
  bracketWinners = as.list(numeric(sMax + 1))
  # begin hyperband
  for(s in sMax:0) {
    catf("Beginning with bracket %s", s)
    brack = bracket$new(
      max.perf = max.perf,
      max.ressources = max.ressources,
      prop.discard = prop.discard,
      s = s,
      B = B, 
      id = id,
      par.set = par.set,
      sample.fun = sample.fun,
      train.fun = train.fun,
      performance.fun = performance.fun
    )
    brack$run()
    if (export.bracket.storage == TRUE) {
      storage.name = paste0("bracket.storage", s)
      assign(storage.name, brack$bracket.storage$data.matrix, envir = .GlobalEnv)
    }
    bracketWinners[[s + 1]] = brack
  }
  # return a list of brackets
  return(rev(bracketWinners))
}

