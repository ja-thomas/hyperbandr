#' @title R6 class to create single bracket objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' An \code{\link[R6]{R6Class}} that consits of multiple algorithm objects
#'
#' @field max.perf [\code{logical()}]\cr
#' TRUE if to maximize the performance (e.g. accuracy of a neural net), 
#' FALSE if to minimize the performance (e.g. find the minimum of the branin function).
#' @field max.ressources [\code{integer()}]\cr
#' The maximum amount of resource that canbe allocated to a single configuration
#' @field prop.discard [\code{integer()}]\cr
#' An input that controls the proportion of configurations discarded in each round of successive halving
#' @field s [\code{integer()}]\cr
#' The s'th bracket object to create. Note that s is in \cr
#' \code{(0,...,floor(log(max.ressources, base = prop.discard)))}
#' @field B [\code{integer()}]\cr
#' The total budget for the bracket. Note that B is given by  \cr
#' \code{(max(s) + 1)*max.ressources}
#' @field id [\code{string}]\cr
#' An id for each Algorithm object in the bracket object
#' @field par.set \cr
#' The parameter set to sample from
#' @field sample.fun \cr
#' The function to sample from par.set
#' @field train.fun \cr
#' The function to carry out training
#' @field performance.fun
#' The function to measure the performance
#'
#' @section Methods:
#' \code{$run()} computes the whole bracket \cr
#' \code{$step()} computes one iteration of successive halving \cr
#' \code{$getTopKModels(k)} displays the best k models \cr
#' \code{$filterTopKModels(k)} filters the best k models and deletes the remaining models from the bracket object \cr
#' \code{$getPerformances()} computes the performance of all remaining models \cr
#'
#' @return Bracket object
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
#' 
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
#' # create bracket s = 4 with hyperbands recommended parameters
#' brack = bracket$new(
#'   max.perf = FALSE,
#'   max.ressources = 81,
#'   prop.discard = 3, 
#'   s = 4, 
#'   B = (4 + 1)*81,
#'   id = "branin",
#'   par.set = configSpace,
#'   sample.fun = sample.fun,
#'   train.fun = train.fun,
#'   performance.fun = performance.fun
#' )
#' 
#' # carry out successive halving:
#' brack$run()
#' 
#' # get the performance of the remaining (best) model
#' brack$getPerformances()
#' 
#' # visualize the results (red: global minima, blue: result of the bracket)
#' opt = data.table(x1 = getGlobalOptimum(problem)$param$x1, x2 = getGlobalOptimum(problem)$param$x2)
#' (vis = autoplot(problem) 
#'   + geom_point(data = opt, aes(x = x1, y = x2), 
#'                shape = 4, colour = "red", size = 5) 
#'   + geom_point(aes(x = brack$models[[1]]$model[[1]], y = brack$models[[1]]$model[[2]]), 
#'                shape = 4, colour = "blue", size = 5))

bracket = R6Class("Bracket",
  public = list(
    id = NULL,
    par.set = NULL,
    sample.fun = NULL,
    configurations = NULL,
    max.ressources = NULL,
    models = NULL,
    prop.discard = NULL,
    s = NULL,
    B = NULL,
    n.configs = NULL,
    r.config = NULL,
    iteration = 0,
    max.perf = NULL,
    bracket.storage = NULL,
    ## initialize the bracket object
    initialize = function(max.perf, max.ressources, prop.discard, s, B, id, 
        par.set, sample.fun, train.fun, performance.fun, ...) {
      self$max.perf = max.perf
      self$id = id
      self$prop.discard = prop.discard
      self$s = s
      self$B = B
      self$n.configs = ceiling((self$B / max.ressources) * (prop.discard^s / (s + 1)))
      self$r.config = max.ressources * prop.discard^(-s)
      self$par.set = par.set
      self$configurations = sample.fun(self$par.set, self$n.configs, ...)
      # create the models
      self$models = mapply(function(conf, name) {
        algorithm$new(id = paste(id, name, sep = "."), 
                      configuration = conf,
                      initial.budget = self$getBudgetAllocation(),
                      init.fun = init.fun, 
                      train.fun = train.fun, 
                      performance.fun = performance.fun)
      }, conf = self$configurations, name = seq_len(self$n.configs))
      # initialize bracket storage
      self$bracket.storage = bracketStorage$new(self$models)
    },
    ## method to compute budget allocation at each step of successive halving 
    getBudgetAllocation = function() {
      self$r.config*self$prop.discard^(self$iteration)
    },
    ## method to compute the number of models to keep after each step of successive halving 
    getNumberOfModelsToSelect = function() {
      floor(self$n.configs / self$prop.discard)
    },
    ## method to display the top k models
    getTopKModels = function(k) {
      if (self$max.perf == TRUE) {
        perfs = self$getPerformances()
        self$models[order(-perfs)][1:k]
      } else {
        perfs = self$getPerformances()
        self$models[order(perfs)][1:k]
      }
    },
    ## method to filter the top k models
    filterTopKModels = function(k) {
      self$models = self$getTopKModels(k = k)
      self$n.configs = k
      invisible(NULL)
    },
    ## method to compute one step of successive halving
    step = function() {
      self$iteration = self$iteration + 1
      self$filterTopKModels(self$getNumberOfModelsToSelect())
      lapply(self$models, function(x) x$continue(self$getBudgetAllocation()))
      # attach model results to the bracket.storage
      self$bracket.storage$attachLines(bracketStorage$new(self$models)$data.matrix)
      invisible(NULL)
    },
    ## method to compute all steps of successive halving (e.g. compute one bracket)
    run = function() {
      for (st in seq_len(self$s)) {
        self$printState()
        self$step()
      }
      self$filterTopKModels(k = 1)
      self$printState()
    },
    ## method to obtain the performance of the remaining models
    getPerformances = function() {
      vapply(self$models, function(x) x$getPerformance(), numeric(1))
    },
    ## method to print the current state of successive halving
    printState = function() {
      catf("Iteration %i, with %i Algorithms left (Budget: %i)", self$iteration, self$n.configs, 
        self$models[[1]]$current.budget)
    },
    ## method to visualize the bracket performance
    visPerformances = function() {
      if (dim(self$bracket.storage$data.matrix)[1] == self$n.configs) {
        catf("execute $run() or $step() before using $visPerformances()")
      } else {
      # some dplyr magic to prepare the bracket data for plotting
        help.df = self$bracket.storage$data.matrix %>%
          select(1:length(self$par.set$pars)) %>% unique() %>% mutate(configuration = 1:nrow(.))
        group.names = names(self$bracket.storage$data.matrix)[1:length(self$par.set$pars)]
        df.gg = self$bracket.storage$data.matrix %>% left_join(help.df, by = group.names) %>%
          group_by_at(vars(one_of(group.names))) %>% mutate(count = n())
        df.gg$configuration = as.factor(df.gg$configuration)
      # plot the mutated data
        ggplot(df.gg, aes(x = current_budget, y = y, colour = configuration)) +
          geom_point(show.legend = FALSE) +
          geom_line(data = df.gg[df.gg$count > 1, ], 
            aes(x = current_budget, y = y, colour = configuration), show.legend = FALSE)
      }  
    }
  )
)
