#' @title Hyperband
#'
#' @description
#' Runs hyperband
#'
#' @param problem [\code{list()}]\cr
#'   List containing data or other information required by \code{train.fun}.
#' @param max.perf [\code{logical(1)}]\cr
#'   \code{TRUE} if \code{performance.fun} should be maximized (e.g. accuracy of a neural net),
#'   \code{FALSE} if it should be mimized (e.g. misclassifaction error). Default is \code{TRUE}.
#' @param max.resources [\code{integer(1)}]\cr
#'   The maximum amount of resources (e.g. iterations) that can be allocated to a single configuration.
#' @param prop.discard [\code{integer(1)}]\cr
#'   Proportion of configurations to be discarded in each round of successive halving.
#' @param id [\code{character(1)}]\cr
#'   Name used for \code{\link{algorithm}} objects.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set to tune over.
#' @param init.fun [\code{function}]\cr
#'   The function to initialize a model. Arguments must be \dQuote{r} for the initial resource allocation,
#'   \dQuote{config} for the output of \code{sample.fun} and \dQuote{problem}.
#' @param sample.fun [\code{function}]\cr
#'   The function to sample from par.set. Takes \dQuote{par.set} and number of configurations to sample \dQuote{n} as arguments.
#'   If no set, random sampling with \code{\link[ParamHelpers]{sampleValues}} is used.
#' @param train.fun [\code{function}]\cr
#'   The function to carry out training. Takes the result of \code{init.fun} as first argument, \dQuote{budget} to specify how many resources should be added and \code{problem}.
#'   Should return an object that can be passed to \code{train.fun} again.
#' @param performance.fun [\code{function}]\cr
#'   The function to measure the performance.
#'   Takes argumennts \dQuote{model} (result of \code{train.fun}) and \code{problem}.
#' @param ...
#'   Further arguments.
#'
#' @return List of brackets.
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
  max.perf = TRUE, id, par.set, sample.fun = function(par.set, n.configs, ...) sampleValues(par = par.set, n = n.configs),
  init.fun, train.fun, performance.fun, ...) {
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

