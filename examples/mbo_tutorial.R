#######################################
############## packages ###############
#######################################

library(mlrMBO)


#######################################
############ mbo workflow #############
#######################################

# 1) Define objective function and its parameters using the package smoof.
# 2) Generate initial design (optional).
# 3) Define mlr` learner for surrogate model (optional).
# 4) Set up a MBO control object.
# 4) Start the optimization with mbo().

#######################################
## 1) define the problem to optimize ##
#######################################

obj.fun = makeCosineMixtureFunction(1)
obj.fun = convertToMinimization(obj.fun)
print(obj.fun)
ggplot2::autoplot(obj.fun)


#######################################
########## 2) initial design ##########
#######################################

des = generateDesign(n = 5, par.set = getParamSet(obj.fun), fun = lhs::randomLHS)
des$y = apply(des, 1, obj.fun)


#######################################
######### 3) surrogate model ##########
#######################################

surr.km = makeLearner("regr.km", predict.type = "se", 
  covtype = "matern3_2", control = list(trace = FALSE))


#######################################
######## 4) MBO control object ########
#######################################

control = makeMBOControl()
control = setMBOControlTermination(control, iters = 10)
control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())


#######################################
######## 5) start optimization ########
#######################################

run = mbo(obj.fun, design = des, learner = surr.km, 
  control = control, show.info = TRUE)

# vis

run = exampleRun(obj.fun, learner = surr.km, 
  control = control, show.info = FALSE)
print(run)
plotExampleRun(run, iters = c(1L, 2L, 10L), pause = FALSE)





