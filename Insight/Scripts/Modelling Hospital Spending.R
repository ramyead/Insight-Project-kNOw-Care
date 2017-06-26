pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

load("Data/care.final.rda")
load("Data/reimbursement.rda")
load("Data/spending.final.rda")
dat = inner_join(care.final, reimbursement.final) %>% inner_join(spending.final) %>% 
rename(Durable.Med.Equip = `Durable Medical Equipment`, Home.Health.Agency = `Home Health Agency`,
       Skilled.Nursing.Facility = `Skilled Nursing Facility`) %>% mutate(State = as.factor(State))

options(contrasts = c("contr.sum", "contr.poly"))
mod = lm(PAYM_30_AMI ~ Hospital.Name, dat)
anova(mod)
summary(mod)
plot(allEffects(mod), multiline = T)

mod = lm(PAYM_30_AMI ~ State + sumscores + Carrier + Durable.Med.Equip + Home.Health.Agency +
           Hospice + Inpatient + Outpatient + Skilled.Nursing.Facility, dat)

mod = lm(PAYM_30_AMI ~ State + H_CLEAN_STAR_RATING   +   H_COMP_1_STAR_RATING    + 
H_COMP_2_STAR_RATING   +  H_COMP_3_STAR_RATING  +   H_COMP_4_STAR_RATING  +   H_COMP_5_STAR_RATING    +
H_COMP_6_STAR_RATING   + H_COMP_7_STAR_RATING   +  H_HSP_RATING_STAR_RATING + H_QUIET_STAR_RATING  + 
H_RECMND_STAR_RATING    + H_STAR_RATING, dat)
plot(PAYM_30_AMI ~ H_CLEAN_STAR_RATING, dat)


mod = lmer(PAYM_30_AMI ~ + H_CLEAN_STAR_RATING   +   H_COMP_1_STAR_RATING    + 
           H_COMP_2_STAR_RATING   +  H_COMP_3_STAR_RATING  +   H_COMP_4_STAR_RATING  +   H_COMP_5_STAR_RATING    +
           H_COMP_6_STAR_RATING   + H_COMP_7_STAR_RATING   +  H_HSP_RATING_STAR_RATING + H_QUIET_STAR_RATING  + 
           H_RECMND_STAR_RATING    + H_STAR_RATING + (1|State), dat)
Anova(mod)
plot(allEffects(mod), multiline = T) 
