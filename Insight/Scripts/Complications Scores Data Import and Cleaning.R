pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

compli = read.csv("Data/Hospital_Revised_Flatfiles/Complications - Hospital.csv") %>% 
  mutate(Score = as.numeric(as.character(Score))) %>% 
  filter(!Compared.to.National %in% c("Not Available", "Number of Cases Too Small")) %>% 
  select(Hospital.Name, State, Measure.Name, Measure.ID, Compared.to.National,Score) %>% 
  tbl_df()

compli.n = distinct(compli, Measure.Name, Measure.ID) %>% as.data.frame()

compli2 = select(compli, Hospital.Name, State, Compared.to.National, Measure.ID, Score)
compli2$row <- 1:nrow(compli2)
compli3 = spread(compli2, Measure.ID, Score, fill = F) %>% select(-row)
### Scores not well calculated, had to recalculate them
compli.final = group_by(compli3, Hospital.Name, State, Compared.to.National) %>% summarise_all(sum) %>% 
  select(Hospital.Name:Compared.to.National, COMP_HIP_KNEE:ncol(.))

compli.test = filter(compli, Measure.ID == "PSI_3_ULCER")
ggplot(compli.test, aes(Compared.to.National, Score)) + geom_boxplot()
ggplot(compli.final, aes(Compared.to.National, PSI_3_ULCER)) + geom_boxplot()

plot(PSI_3_ULCER ~ Compared.to.National, compli.final)
mod = lm(PSI_3_ULCER ~ Compared.to.National, compli.final)
Anova(mod, type = 3)
plot(allEffects(mod), multiline = T)
summary(mod)

plot(Score ~ Compared.to.National, compli.test)
mod = lm(Score ~ Compared.to.National, compli.test)
Anova(mod, type = 3)
plot(allEffects(mod), multiline = T)
summary(mod)







