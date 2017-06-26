pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

spending = read.csv("Data/Hospital_Revised_Flatfiles/Medicare Hospital Spending by Claim.csv") %>% 
  filter(Period != "During Index Hospital Admission") %>%  
  select(Hospital.Name = Hospital_Name, State, Claim_Type, Avg_Spending_Per_Episode_Hospital, 
         Avg_Spending_Per_Episode_State, Avg_Spending_Per_Episode_Nation)

spending.n = distinct(spending, Claim_Type) %>% as.data.frame()
spending2 = spending

spending2$row <- 1:nrow(spending2)
spending2 = spread(spending2, Claim_Type, Avg_Spending_Per_Episode_Hospital, fill = F) %>% select(-row)
### Scores not well calculated, had to recalculate them
spending.final = group_by(spending2, Hospital.Name, State) %>% 
  summarise_all(sum) 
#save(spending.final, file = "Data/spending.final.rda")
