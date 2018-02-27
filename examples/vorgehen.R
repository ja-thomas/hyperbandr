# 1) initialisiere bracket 1 mit generate design
# 2) ctrl = makeMBOControl()
#    ctrl = setMBOControlInfill(ctrl, crit = crit.ei)
# 3) initialisiere SMBO
#    opt.state = initSMBO()
# 4) => bracket 2: 
#    prop = proposePoints(opt.state)
####
# library(mlrMBO)
# library(ggplot2)
# set.seed(1)
# 
# fun = function(x) {
#   x^2 + sin(2 * pi * x) * cos(0.3 * pi * x)
# }
# ps = makeParamSet(
#   makeNumericParam("x", lower = -3, upper = 3)
# )
# des = generateDesign(n = 81, par.set = ps)
# des$y = apply(des, 1, fun) #getPerformance()
# des
# surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))
# ctrl = makeMBOControl(propose.points = 3)
# ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
# opt.state = initSMBO(
#   par.set = ps, 
#   design = des, 
#   control = ctrl,
#   learner = surr.km,
#   minimize = TRUE, 
#   noisy = FALSE)
# plot(opt.state)
# prop = proposePoints(opt.state)
# prop

#######################################
############# hyperbandMBO ############
#######################################
### step 1
## initialize bracket 1: draw random configs
brack = bracket$new(
  max.perf = FALSE,
  max.ressources = 81,
  prop.discard = 3,
  s = 4,
  B = (4 + 1)*81,
  id = "xgboost",
  par.set = configSpace,
  sample.fun = sample.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)
### step 2
## apply MBO on the 81 configurations
library("mlrMBO")
# generate pseudo design and append each configs performance performance
test = as.data.frame(matrix(unlist(brack$configurations), ncol = length(unlist(brack$configurations[1])), byrow = TRUE))
test$y = brack$getPerformances()
colnames(test) = c("max_depth", "colsample_bytree", "subsample", "y")
surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))
ctrl = makeMBOControl(propose.points = 34)
ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
opt.state = initSMBO(
  par.set = configSpace, 
  design = test, 
  control = ctrl,
  learner = surr.km,
  minimize = TRUE, 
  noisy = FALSE)
prop = proposePoints(opt.state)
propPoints = prop$prop.points
rownames(propPoints) = c()
propPoints
### step 3
## finish the bracket
brack = brack$run()
### step 4
## initialize bracket 2: take proposed points
# erst mal nur ein algorithm object
init.fun = function(r, config) {
  watchlist = list(eval = dtest, train = dtrain)
  capture.output({mod = xgb.train(config, dtrain, nrounds = r, watchlist, verbose = 1)})
  return(mod)
}
init.fun(r = 3, config = propPoints[1,])

obj = algorithm$new(
  id = "xgboost",
  configuration = propPoints[1,],
  initial.budget = 1,
  init.fun = init.fun,
  train.fun = train.fun,
  performance.fun = performance.fun)
# make propPoints to a list
str(propPoints)
bla = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
bla
# apply bracketMBO:
# uses           self$configurations = par.set
# instead of     self$configurations = sample.fun(par.set, self$n.configs)
# to init models
brack2 = bracketMBO$new(
  max.perf = FALSE,
  max.ressources = 81,
  prop.discard = 3,
  s = 3,
  B = (4 + 1)*81,
  id = "xgboost",
  par.set = bla,
  train.fun = train.fun,
  performance.fun = performance.fun)


#######################################
####### hyperbandMBO algorithm ########
#######################################
hyperbandMBO = function(max.perf = TRUE, max.ressources = 81, prop.discard = 3, id, 
  par.set,  sample.fun, train.fun, performance.fun) { 
  # |sMax + 1| are the total number of brackets to try 
  sMax =  floor(log(max.ressources, base = prop.discard))
  B = (sMax + 1)*max.ressources
  # initialize a list for all #sMax brackets
  bracketWinners = as.list(numeric(sMax + 1))
  # begin hyperbandMBO
  catf("Beginning with bracket %s", sMax)
  bracksMax = bracket$new(
    max.perf = max.perf,
    max.ressources = max.ressources,
    prop.discard = prop.discard,
    s = sMax,
    B = B, 
    id = id,
    par.set = par.set,
    sample.fun = sample.fun,
    train.fun = train.fun,
    performance.fun = performance.fun)
  design = as.data.frame(matrix(unlist(bracksMax$configurations), ncol = length(unlist(bracksMax$configurations[1])), byrow = TRUE))
  design$y = bracksMax$getPerformances()
  colnames(design) = c("max_depth", "colsample_bytree", "subsample", "y")
  surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))
  bracksMax$run()
  bracketWinners[[sMax + 1]] = bracksMax
  # here: brackets (sMax - 1:0)
  for(s in (sMax - 1):0) {
    catf("Beginning to propose points for bracket %s", s)
    ctrl = makeMBOControl(propose.points = ceiling(bracksMax$B/length(bracksMax$configurations)*((bracksMax$prop.discard^s)/(s+1))))
    ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
    opt.state = initSMBO(
      par.set = configSpace, 
      design = design, 
      control = ctrl,
      learner = surr.km,
      minimize = TRUE, 
      noisy = FALSE)
    prop = proposePoints(opt.state)
    propPoints = prop$prop.points
    rownames(propPoints) = c()
    propPoints = convertRowsToList(propPoints, name.list = FALSE, name.vector = TRUE)
    catf("Beginning with bracket %s", s)
    brack = bracketMBO$new(
      max.perf = max.perf,
      max.ressources = max.ressources,
      prop.discard = prop.discard,
      s = s,
      B = B, 
      id = id,
      par.set = propPoints,
      train.fun = train.fun,
      performance.fun = performance.fun
    )
    brack$run()
    bracketWinners[[s + 1]] = brack
  }
  # return a list of brackets
  return(rev(bracketWinners))
}

#######################################
######### very sick benchmark #########
#######################################
## "classic" hyperband
benchmarkThis = function(howManyIt, precision) {
  results = data.frame(matrix(ncol = 5, nrow = howManyIt))
  for (i in 1:howManyIt) {
    catf("Iteration %i", i)
    hyperhyper = hyperband(
      max.perf = FALSE, 
      max.ressources = 81, 
      prop.discard = 3, 
      id = "xgboost", 
      par.set = configSpace, 
      sample.fun =  sample.fun,
      train.fun = train.fun, 
      performance.fun = performance.fun)
    results[i, 1] = round(hyperhyper[[1]]$getPerformances(), digits = precision)
    results[i, 2] = round(hyperhyper[[2]]$getPerformances(), digits = precision)
    results[i, 3] = round(hyperhyper[[3]]$getPerformances(), digits = precision)
    results[i, 4] = round(hyperhyper[[4]]$getPerformances(), digits = precision)
    results[i, 5] = round(hyperhyper[[5]]$getPerformances(), digits = precision)
  }
  return(results)
}

# make 10 iterations
xgboostBenchmark = benchmarkThis(10, precision = 6)

# visualize the results
hyperband = ggplot(stack(xgboostBenchmark), aes(x = ind, y = values, fill = ind)) + 
  scale_x_discrete(labels=c("bracket 1","bracket 2","bracket 3","bracket 4", "bracket 5")) + 
  scale_y_continuous(limits = c(0, 0.00175)) +
  theme(legend.position = "none") + 
  labs(x = "", y = "performance") + 
  geom_boxplot()

# the alot more fancy hyperbandMBO
benchmarkThisAsWell = function(howManyIt, precision) {
  results = data.frame(matrix(ncol = 5, nrow = howManyIt))
  for (i in 1:howManyIt) {
    catf("Iteration %i", i)
    hyperhyper = hyperbandMBO(
      max.perf = FALSE, 
      max.ressources = 81, 
      prop.discard = 3, 
      id = "xgboost", 
      par.set = configSpace, 
      sample.fun =  sample.fun,
      train.fun = train.fun, 
      performance.fun = performance.fun)
    results[i, 1] = round(hyperhyper[[1]]$getPerformances(), digits = precision)
    results[i, 2] = round(hyperhyper[[2]]$getPerformances(), digits = precision)
    results[i, 3] = round(hyperhyper[[3]]$getPerformances(), digits = precision)
    results[i, 4] = round(hyperhyper[[4]]$getPerformances(), digits = precision)
    results[i, 5] = round(hyperhyper[[5]]$getPerformances(), digits = precision)
  }
  return(results)
}

# make 10 iterations
xgboostBenchmarkMBO = benchmarkThisAsWell(10, precision = 6)

# visualize the results
hyperbandMBO = ggplot(stack(xgboostBenchmarkMBO), aes(x = ind, y = values, fill = ind)) + 
  scale_x_discrete(labels=c("bracket 1","bracket 2","bracket 3","bracket 4", "bracket 5")) + 
  scale_y_continuous(limits = c(0, 0.00175)) +
  theme(legend.position = "none") + 
  labs(x = "", y = "performance") + 
  geom_boxplot()

# next to next
require(gridExtra)
grid.arrange(hyperband, hyperbandMBO, ncol = 2)

