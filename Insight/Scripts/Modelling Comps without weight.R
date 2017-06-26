pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, 
               nlme, mlr, parallelMap)

load("Data/care.final.rda")
load("Data/hai.final.rda")
load("Data/readmission.rda")
load("Data/reimbursement.rda")
load("Data/care_info.rda")
options(warn=-1)
dattot = inner_join(read.final, reimbursement.final) %>% inner_join(hai.final) %>% inner_join(care.final)

#save(dattot, file = "Data/dattot.rda")

# mod = lm(MORT_30_HF ~ H_CLEAN_STAR_RATING   +   H_COMP_1_STAR_RATING    + 
#            H_COMP_2_STAR_RATING   +  H_COMP_3_STAR_RATING  +   H_COMP_4_STAR_RATING  +   H_COMP_5_STAR_RATING    +
#            H_COMP_6_STAR_RATING   + H_COMP_7_STAR_RATING   +  H_HSP_RATING_STAR_RATING + H_QUIET_STAR_RATING  + 
#            H_RECMND_STAR_RATING    + H_STAR_RATING + State, dattot)
# anova(mod)
# plot(allEffects(mod), multiline = T)

hist(dattot$READM_30_HF)
thresh = quantile(dattot$READM_30_HF, c(.70))  ## 70th percentile of readmission rate and above
dat2 = filter(dattot, READM_30_HF != 0) %>% mutate(outcome = as.factor(ifelse(READM_30_HF > thresh, "bad", "normal")))
ggplot(dat2, aes(outcome, ..count..)) + geom_bar(aes(fill = outcome), position = "dodge") + theme_bw(base_size = 18)

# modlog = glm(outcome ~ H_CLEAN_STAR_RATING   +   H_COMP_1_STAR_RATING    + 
#       H_COMP_2_STAR_RATING   +  H_COMP_3_STAR_RATING  +   H_COMP_4_STAR_RATING  +   H_COMP_5_STAR_RATING    +
#       H_COMP_6_STAR_RATING   + H_COMP_7_STAR_RATING   +  H_HSP_RATING_STAR_RATING + H_QUIET_STAR_RATING  + 
#       H_RECMND_STAR_RATING    + H_STAR_RATING + State, data = dat2, family = "binomial")
# 
# Anova(modlog)
# plot(allEffects(modlog), multiline = T)

# modlog = glm(outcome ~ H_COMP_2_STAR_RATING, data = dat2, family = "binomial")
# Anova(modlog)
# plot(allEffects(modlog), multiline = T)
# plot(H_COMP_6_STAR_RATING ~ outcome, dat2)

###LEARNER
# dattest = ungroup(dat2) %>% select(outcome, H_CLEAN_STAR_RATING, H_COMP_1_STAR_RATING, 
#                  H_COMP_2_STAR_RATING,  H_COMP_3_STAR_RATING,   H_COMP_4_STAR_RATING,   H_COMP_5_STAR_RATING,
#                  H_COMP_6_STAR_RATING, H_COMP_7_STAR_RATING,  H_HSP_RATING_STAR_RATING, H_QUIET_STAR_RATING, 
#                  H_RECMND_STAR_RATING, H_STAR_RATING) %>% data.frame()
#write.csv(dattest, file = "Data/mldat.csv")

dattest2 = ungroup(dat2) %>% select(outcome, H_CLEAN_STAR_RATING, H_COMP_1_STAR_RATING, 
                                    H_COMP_2_STAR_RATING,  H_COMP_3_STAR_RATING,   H_COMP_4_STAR_RATING,   H_COMP_5_STAR_RATING,
                                    H_COMP_6_STAR_RATING, H_COMP_7_STAR_RATING, H_QUIET_STAR_RATING) %>% data.frame()
#save(dattest2, file =  "Data/mldat.rda")

task = makeClassifTask(data = dattest2, target = "outcome", positive = 'bad')
#task = normalizeFeatures(task)
meas <- list(bac, mlr::auc, tpr, tnr)

getWeight <- function(task){
  x <- table(getTaskData(task, target.extra = T)$target)
  max(x)/min(x)
}

inner <- makeResampleDesc("RepCV", folds = 10, reps = 2, stratify = T)
outer <- makeResampleDesc("RepCV", folds = 10, reps = 2,  stratify = T)
ctrl <- makeTuneControlGrid(resolution = 10)

### LASSO
lrn <- makeLearner("classif.LiblineaRL2LogReg", id = 'Lasso', predict.type = "prob")
weight.wrap <- makeWeightedClassesWrapper(lrn, wcw.param = 'wi', wcw.weight =  getWeight(task))
ps <- makeParamSet(makeNumericParam("cost",lower = 0.01, upper = 10))
tune.wrap <- makeTuneWrapper(lrn, inner, par.set = ps, control = ctrl, measures = bac)

### KSVM
# lrn <- makeLearner("classif.ksvm", class.weights = getWeight(task))
# getParamSet(lrn)
# weight.wrap <- makeWeightedClassesWrapper(lrn, class.weights = getWeight(task))
# ps <- makeParamSet(makeNumericParam("nu",lower = 0, upper = 5))
# tune.wrap <- makeTuneWrapper(lrn, inner, par.set = ps, control = ctrl, measures = bac)

parallelStartSocket(6)
out <- resample(tune.wrap, task, outer, measures = meas, extract = getTuneResult)
parallelStop()

perf = out$measures.test %>% gather() %>% filter(key != "iter") %>% group_by(key) %>% 
  summarise(mean(value)) %>% rename(Measure = key, Accuracy = `mean(value)`)

print(perf)
#save(perf, file = "Data/lasso perf.rda")
ggplot(perf, aes(x = Measure, y = Accuracy, fill = Measure)) + geom_bar(stat="identity") + 
  coord_cartesian(ylim = c(0.50, .70))  + theme_bw(base_size = 18)






out$extract
w = getWeight(task)
w
lrn = makeLearner("classif.LiblineaRL2LogReg", id = 'Lasso', predict.type = "prob", 
                  cost = 6.67, wi = c(bad = w, normal = 1))
#outt <- resample(lrn, task, outer, measures = meas)

getHyperPars(lrn)
mod = train(lrn, task)
pred = as.data.frame(predict(mod, newdata = dattest2[1:500, 2:ncol(dattest2)]))
table(pred$response)

#### Get Variable Significance
opt.weight.wrap <- makeWeightedClassesWrapper(lrn, wcw.param = 'wi', wcw.weight = w)
signi.var = train(opt.weight.wrap, task)$learner.model$next.model$learner.model$W
sig.var = as.data.frame(signi.var) %>% select(-Bias) %>% mutate_all(funs(as.numeric)) 
colnames(sig.var) = c("Cleanliness", "Nurse interaction", "Doctors interaction", "Staff interaction", "Pain management",
                      "Medication info", "Discharge info", "Care transition", "Quietness")
sig.var = gather(sig.var)
#save(sig.var, file = "Data/sig.var.rda")

ggplot(sig.var, aes(x = key, y = value, fill = key)) + geom_bar(stat="identity") + theme_bw(base_size = 18) +
  xlab("Measure") + ylab("Contribution in prediction model")










