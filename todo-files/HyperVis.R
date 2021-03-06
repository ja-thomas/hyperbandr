#' @title HyperVis
#'
#' @description
#' Visualizes hyperband results
#'
#' @param hyper.object [\code{logical()}]\cr
#' A list of brackets (e.g. a hyperband "object")
#' @param rows [\code{integer(1)}]\cr
#' The number of rows to present the brackets. Default is 1.
#' @param cols [\code{integer(1)}]\cr
#' The number of columns to present the brackets. Default is length(hyper.object).
#' @param perfLimits [\code{vector of integers()}]\cr
#' Vector representing the lower and upper limit of the y-axis (performance).
#' Default: hyperVis tries to automatically determine the upper and lower limits.
#'
#' @return ggplot2 object
#' @export
#' @examples
#'
#' # we need some packages
#' library("ggplot2")
#' library("smoof")
#' library("data.table")
#' library("dplyr")
#'
#' # simple example for the branin function, a minimization problem
#' problem = makeBraninFunction()
#' opt = data.table(x1 = getGlobalOptimum(problem)$param$x1, x2 = getGlobalOptimum(problem)$param$x2)
#' # the three red dots are global minima
#' autoplot(problem) +
#'   geom_point(data = opt, aes(x = x1, y = x2), shape = 20, colour = "red", size = 5)
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
#' init.fun = function(r, config) {
#'   x1 = unname(unlist(config))
#'   x2 = runif(1, 0, 15)
#'   mod = c(x1, x2)
#'   return(mod)
#' }
#'
#' # train fun
#' train.fun = function(mod, budget) {
#'   for(i in seq_len(budget)) {
#'     mod.new = c(mod[[1]], mod[[2]] + rnorm(1, sd = 3))
#'     if(performance.fun(mod.new) < performance.fun(mod))
#'       mod = mod.new
#'   }
#'   return(mod)
#' }
#'
#' # performance fun
#' performance.fun = function(model) {
#'   problem(c(model[[1]], model[[2]]))
#' }
#'
#' ########### call hyperband ############
#' hyperhyper = hyperband(
#'   max.resources = 81,
#'   prop.discard = 3,
#'   max.perf = FALSE,
#'   id = "branin",
#'   par.set = configSpace,
#'   sample.fun =  sample.fun,
#'   train.fun = train.fun,
#'   performance.fun = performance.fun)
#'
#' # visualize all brackets
#' hyperVis(hyperhyper)
hyperVis = function(hyper.object, rows = 1, cols = length(hyper.object),
  ## get lower limit ( = floor(min observed performance over all brackets))
  perfLimits = c(floor(min(unlist(lapply(hyper.object,
    function(x) min(x$bracket.storage$data.matrix$y))))),
  ## get upper limit:
  # if ceiling(max observed performance)  = 1  => round one digit
  # if ceiling(max observed performance) != 1  => use ceiling
  ifelse(ceiling(max(unlist(lapply(hyper.object,
    function(x) max(x$bracket.storage$data.matrix$y))))) == 1,
  round(max(unlist(lapply(hyper.object,
    function(x) max(x$bracket.storage$data.matrix$y)))), digits = 2),
  ceiling(max(unlist(lapply(hyper.object,
    function(x) max(x$bracket.storage$data.matrix$y)))))))) {
  # create plot list
  plotList = lapply(hyper.object,
    function(x) x$visPerformances(make.labs = FALSE, limits = perfLimits))
  # plot brackets in
  do.call("grid.arrange", c(plotList, ncol = cols, bottom = "budget", left = "performance"))
}



