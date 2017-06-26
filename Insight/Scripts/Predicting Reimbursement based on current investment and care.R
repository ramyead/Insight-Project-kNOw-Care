pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

load("Data/care.final.rda")
load("Data/reimbursement.rda")
load("Data/spending.final.rda")

dat = inner_join(care.final, reimbursement.final) %>% inner_join(spending.final)

