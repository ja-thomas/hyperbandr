library(mlr)
library(mlrMBO)
set.seed(103)
library(gridExtra)
library(ggplot2)

trans_theme = theme(
    panel.grid.minor = element_line(colour = "#DDDDDD"), 
    panel.grid.major = element_line(colour = "#D0D0D0"),
    panel.background = element_rect(fill=NA, colour = NA),
    plot.background = element_rect(fill=NA),
    legend.background = element_rect(fill=NA),
    panel.border = element_rect(fill = NA, colour = "#DDDDDD")
)

# cnn
lrn = makeLearner("classif.mxff",
  layers = 2, num.layer1 = 32, num.layer2 = 16, 
  act1 = "relu", act2 = "relu", act.out = "softmax",
  conv.data.shape = c(28,28),
  conv.layer1 = TRUE,
  conv.kernel1 = c(2,2), conv.stride1 = c(2,2),
  pool.kernel1 = c(2,2), pool.stride1 = c(2,2),
  begin.round = 1,
  num.round = 10,
  array.batch.size = 128
)
# task
task = makeClassifTask(data = mnist, target = "label")
# param set
ps = makeParamSet(
  makeNumericParam(id = "dropout.input", lower = 0.1, upper = 0.9),
  #makeNumericParam(id = "learning.rate", lower = 0.01, upper = 0.99),
  makeNumericParam(id = "momentum", lower = 0, upper = 0.99)
)
#####################################################################################
# grid search
grid.ctrl = makeTuneControlGrid(resolution = 5)  
rdesc = makeResampleDesc("CV", iters = 3)
# okay this line runs code
res.grid = tuneParams(lrn, task, rdesc, par.set = ps, control = grid.ctrl, show.info = FALSE)
# more stuff i dont know
op.df.gs = as.data.frame(res.grid$opt.path)
g2 = ggplot(op.df.gs, aes(x = dropout.input, y = momentum, fill = mmce.test.mean))
add.stuff = function(g) {g + geom_tile() + scale_x_continuous(name = "dropout input") + 
    scale_y_continuous(name = "momentum") + trans_theme + 
    scale_fill_gradient2(name = "mmce", low = "yellow", high = "red", mid = "orange", limits = range(op.df.gs$mmce.test.mean), midpoint = mean(range(op.df.gs$mmce.test.mean)))}
g2 = add.stuff(g2)
g2 = g2 + geom_point()
print(g2) 
##################################################################################### 
# mbo
mbo.ctrl = makeMBOControl()
mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = 10)
surrogate.lrn = makeLearner("regr.km", predict.type = "se")
ctrl = mlr:::makeTuneControlMBO(learner = surrogate.lrn, 
  mbo.control = mbo.ctrl, same.resampling.instance = FALSE)
rdesc = makeResampleDesc("CV", iters = 3)
res.mbo = tuneParams(lrn, task, rdesc, par.set = ps, control = ctrl, show.info = FALSE)  
##################################################################################### 
# Grid Search vs. MBO
set.seed(1)
#configureMlr(on.par.without.desc = 'quiet')
op.df.mbo = as.data.frame(res.mbo$opt.path)
grid = generateGridDesign(ps, 50)
surrogate.lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE, multistart = 5)
m = train(surrogate.lrn, task = makeRegrTask(data = op.df.mbo[,c("dropout.input","momentum","mmce.test.mean")], 
  target = "mmce.test.mean"))
grid.mbo = grid
grid.mbo$mmce.test.mean = getPredictionResponse(predict(m, newdata = grid))
lrn.fnn = makeLearner("regr.fnn", k = 1)
m = train(lrn.fnn, task = makeRegrTask(data = op.df.gs[,c("dropout.input","momentum","mmce.test.mean")], 
  target = "mmce.test.mean"))
grid.gs = grid
grid.gs$mmce.test.mean = getPredictionResponse(predict(m, newdata = grid))
data.points = rbind(cbind(op.df.mbo, method = "MBO"), cbind(op.df.gs, method = "grid search"))
data.grid = rbind(cbind(grid.mbo, method = "MBO"), cbind(grid.gs, method = "grid search"))
data.grid$method = factor(data.grid$method, levels = rev(levels(data.grid$method)))
data.points$method = factor(data.points$method, levels = rev(levels(data.points$method)))
g3 = ggplot(mapping = aes(x=dropout.input, y=momentum, fill=mmce.test.mean))
g3 = g3 + geom_tile(data = data.grid) + geom_point(data = data.points)
g3 = g3 + scale_x_continuous(name = "dropout input") + scale_y_continuous(name = "momentum") + trans_theme
g3 = g3 + scale_fill_gradient2(name = "mmce", low = "yellow", high = "red", mid = "orange", limits = range(data.grid$mmce.test.mean), midpoint = mean(range(data.grid$mmce.test.mean)))
g3 = g3 + facet_grid(~method)
g3 = g3 + annotate("text", x = 1, y = 0.0265, label = "Best dense net so far")
g3
  
  
  
  
  
  
  
  
  
  