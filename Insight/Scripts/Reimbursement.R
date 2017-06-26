pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

reimbursement = read.csv("Data/Hospital_Revised_Flatfiles/Payment and Value of Care - Hospital.csv") %>%  
  select(Hospital.Name = Hospital.name, State, Payment.measure.name, Payment.measure.ID, Payment)

reimbursement2= select(reimbursement, Hospital.Name, State, Payment.measure.ID, Payment) %>% 
  mutate(Payment = as.numeric(gsub('[$,]', '', reimbursement$Payment)))

reimbursement2$row <- 1:nrow(reimbursement2)
reimbursement3 = spread(reimbursement2,Payment.measure.ID, Payment, fill = F) %>% select(-row) %>% 
  group_by(Hospital.Name, State) 

reimbursement.final = group_by(reimbursement3, Hospital.Name, State) %>% 
  summarise_all(sum) 
# save(reimbursement.final, file = "Data/reimbursement.rda")
