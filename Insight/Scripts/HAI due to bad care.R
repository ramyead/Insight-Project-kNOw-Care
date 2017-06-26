pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

hai = read.csv("Data/Hospital_Revised_Flatfiles/Healthcare Associated Infections - Hospital.csv") %>% 
  mutate(Score = as.numeric(as.character(Score)), Hospital = as.character(Hospital.Name)) %>% 
  filter(!Compared.to.National %in% c("Not Available", "Number of Cases Too Small")) %>% 
  select(Hospital.Name, State, Measure.Name, Measure.ID, Compared.to.National,Score) %>% 
  tbl_df()

hai.n = distinct(hai, Measure.Name, Measure.ID) %>% as.data.frame()

hai2 = select(hai, Hospital.Name, State, Compared.to.National, Measure.ID, Score)
hai2$row <- 1:nrow(hai2)
hai3 = spread(hai2, Measure.ID, Score, fill = F) %>% select(-row)
### Scores not well calculated, had to recalculate them
hai.final = group_by(hai3, Hospital.Name, State, Compared.to.National) %>% summarise_all(sum) %>% 
  mutate(CAUTI = (HAI_2_NUMERATOR/HAI_2_ELIGCASES),
         SSI.Colon = (HAI_3_NUMERATOR/HAI_3_ELIGCASES),
         SSI.Abdo = (HAI_4_NUMERATOR/HAI_4_ELIGCASES),
         MRSA = (HAI_5_NUMERATOR/HAI_5_ELIGCASES),
         C.diff = (HAI_6_NUMERATOR/HAI_6_ELIGCASES)) %>% select(Hospital.Name, State, CAUTI:C.diff, Compared.to.National)
#save(hai.final,file = "Data/hai.final.rda")

filter(hai.final, CAUTI > 8) %>% summary()
plot(CAUTI ~ State, hai.final)

hai.test = filter(hai, Measure.ID == "HAI_2_CI_UPPER")
ggplot(hai.test, aes(Compared.to.National, Score)) + geom_boxplot()
ggplot(hai.final, aes(Compared.to.National, CAUTI)) + geom_boxplot()

care = read.csv("Data/Hospital_Revised_Flatfiles/HCAHPS - Hospital.csv") %>% 
  select(Hospital.Name, State, HCAHPS.Measure.ID, HCAHPS.Question,HCAHPS.Answer.Description,
         Patient.Survey.Star.Rating, HCAHPS.Linear.Mean.Value) %>% 
  filter(!Patient.Survey.Star.Rating %in% c("Not Applicable", "Not Available")) %>% 
  tbl_df()

#info.care = distinct(select(care, HCAHPS.Measure.ID, HCAHPS.Question, HCAHPS.Answer.Description)) 
#save(info.care, file = "Data/care_info.rda")

care1 = select(care, Hospital.Name, State, HCAHPS.Measure.ID, Patient.Survey.Star.Rating)
care1$row <- 1:nrow(care1)
care2 = spread(care1, HCAHPS.Measure.ID, Patient.Survey.Star.Rating, fill = F) %>% select(-row) 
  
care.final = group_by(care2, Hospital.Name, State) %>%  mutate_each(funs(as.character)) %>%  mutate_each(funs(as.numeric)) 
care.final[is.na(care.final)] <- 0
care.final =  group_by(care.final, Hospital.Name, State) %>%  summarise_all(sum)
care.final$sumscores = rowSums(care.final[3:ncol(care.final)])
# save(care.final,file = "Data/care.final.rda")


ggplot(care.final, aes(y = sumscores, x = State)) + geom_boxplot()
dat.tot = inner_join(hai.final, care.final)

plot(sumscores ~ CAUTI, dat.tot)
mod = lm(CAUTI ~ sumscores, dat.tot)
Anova(mod, type = 3)
plot(allEffects(mod), multiline = T)
summary(mod)



