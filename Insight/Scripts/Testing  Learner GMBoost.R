pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme, mlr)
mldat = read.csv("Data/mldat.csv")

task = makeRegrTask(data = mldat, target = "PAYM_30_AMI")
mod = train(lrn, task, subset = train.set)
meas <- list(expvar, mse, kendalltau, mape)
rdesc <- makeResampleDesc('Bootstrap', iters = 20) 

lrn = makeLearner("regr.gbm")
ps <- makeParamSet(makeIntegerParam('n.trees', lower = 1, upper = 200))
gr <- makeTuneControlGrid(resolution = 10)
res <- tuneParams(lrn, task, rdesc, measures = meas, ps, gr)


lrn = makeLearner("regr.glmnet")
ps <- makeParamSet(makeIntegerParam('nlambda', lower = 1, upper = 200))
gr <- makeTuneControlGrid(resolution = 10)
res <- tuneParams(lrn, task, rdesc, measures = meas, ps, gr)


plotLearnerPrediction("regr.gbm", features = "lstat", task = task)


plot(response ~ truth, as.data.frame(res))
