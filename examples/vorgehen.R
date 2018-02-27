



# 1) initialisiere bracket 1 mit generate design
# 2) ctrl = makeMBOControl()
#    ctrl = setMBOControlInfill(ctrl, crit = crit.ei)
# 3) initialisiere SMBO
#    opt.state = initSMBO()
# 4) => bracket 2: 
#    prop = proposePoints(opt.state)
####
library(mlrMBO)
library(ggplot2)
set.seed(1)

fun = function(x) {
  x^2 + sin(2 * pi * x) * cos(0.3 * pi * x)
}
ps = makeParamSet(
  makeNumericParam("x", lower = -3, upper = 3)
)
des = generateDesign(n = 81, par.set = ps)
des$y = apply(des, 1, fun) #getPerformance()
des
surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))
ctrl = makeMBOControl(propose.points = 3)
ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
opt.state = initSMBO(
  par.set = ps, 
  design = des, 
  control = ctrl,
  learner = surr.km,
  minimize = TRUE, 
  noisy = FALSE)
plot(opt.state)
prop = proposePoints(opt.state)
prop









