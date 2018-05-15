#' @title Hyperband
#'
#' @description
#' Runs hyperband
#'
#' @param problem [\code{list()}]\cr
#'  List containing data and other information required for train
#' @param max.perf [\code{logical()}]\cr
#' TRUE if to maximize the performance (e.g. accuracy of a neural net),
#' FALSE if to minimize the performance (e.g. find the minimum of the branin function). \cr
#' Default: TRUE
#' @param max.resources [\code{integer()}]\cr
#' The maximum amount of resource that canbe allocated to a single configuration
#' @param prop.discard [\code{integer()}]\cr
#' An input that controls the proportion of configurations discarded in each round of successive halving
#' @param id [\code{string}]\cr
#' An id for each Algorithm object in the bracket object
#' @param par.set \cr
#' The parameter set to sample from
#' @param init.fun \cr
#' The function to initial a model
#' @param sample.fun \cr
#' The function to sample from par.set
#' @param train.fun \cr
#' The function to carry out training
#' @param performance.fun
#' The function to measure the performance
#' @param ...
#' Further arguments
#'
#' @return List of brackets
#' @examples
#'
#' # we need some packages
#' library("ggplot2")
#' library("smoof")
#' library("data.table")
#'
#' # we choose the 2 dimensional branin function
#' braninProb = makeBraninFunction()
#'
#' # the branin function has 3 global minima
#' opt = data.table(x1 = getGlobalOptimum(braninProb)$param$x1,
#'   x2 = getGlobalOptimum(braninProb)$param$x2)
#' param.set = getParamSet(braninProb)
#'
#'
#' #######################################
#' ## define functions to use hyperband ##
#' #######################################
#'
#' # config space
#' configSpace = makeParamSet(
#'     makeNumericParam(id = "x1", lower = -5, upper = 10.1))
#'
#' # sample fun
#' sample.fun = function(par.set, n.configs, ...) {
#'   sampleValues(par = par.set, n = n.configs)
#' }
#'
#' # init fun
#' init.fun = function(r, config, problem) {
#'   x1 = unname(unlist(config))
#'   x2 = runif(1, 0, 15)
#'   mod = c(x1, x2)
#'   return(mod)
#' }
#'
#' # train fun
#' train.fun = function(mod, budget, problem) {
#'   for(i in seq_len(budget)) {
#'     mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
#'     if(performance.fun(mod.new) < performance.fun(mod))
#'       mod = mod.new
#'   }
#'   return(mod)
#' }
#'
#' # performance fun
#' performance.fun = function(model, problem) {
#'   braninProb(c(model[[1]], model[[2]]))
#' }
#'
#' hyperhyper = hyperband(
#'  problem = braninProb,
#'  max.resources = 81,
#'  prop.discard = 3,
#'  max.perf = FALSE,
#'  id = "branin",
#'  par.set = configSpace,
#'  sample.fun =  sample.fun,
#'  init.fun = init.fun,
#'  train.fun = train.fun,
#'  performance.fun = performance.fun)
#'
#' # get the best performance of each bracket
#' lapply(hyperhyper, function(x) x$getPerformances())
#' @export
hyperband = function(problem, max.resources = 81, prop.discard = 3,
  max.perf = TRUE, id, par.set, sample.fun, init.fun, train.fun, performance.fun, ...) {
  # |sMax + 1| are the total number of brackets to try
  sMax =  floor(log(max.resources, base = prop.discard))
  B = (sMax + 1)*max.resources
  # initialize a list for all #sMax brackets
  bracketWinners = as.list(numeric(sMax + 1))
  totalStorage = hyperStorage$new(par.set)
  # begin hyperband
  for(s in sMax:0) {
    catf("Beginning with bracket %s", s)
    brack = bracket$new(
      problem,
      max.perf = max.perf,
      max.resources = max.resources,
      prop.discard = prop.discard,
      s = s,
      B = B,
      id = id,
      par.set = par.set,
      sample.fun = sample.fun,
      init.fun = init.fun,
      train.fun = train.fun,
      performance.fun = performance.fun,
      hyper.storage = totalStorage$data.matrix,
      ...
    )
    brack$run()
    bracketWinners[[s + 1]] = brack
    totalStorage$attachLines(bracketWinners[[s + 1]]$bracket.storage$data.matrix)
  }
  # return a list of brackets
  return(rev(bracketWinners))
}

