pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

imaging = read.csv("Data/Hospital_Revised_Flatfiles/Outpatient Imaging Efficiency - Hospital.csv") 
  
imaging.n = distinct(imaging, Measure.ID, Measure.Name) %>% as.data.frame()
imaging2 = select(imaging, Hospital.Name, State, Measure.ID , Score) %>% mutate(Score = as.numeric(as.character(Score)))

imaging2$row <- 1:nrow(imaging2)
imaging2 = spread(imaging2, Measure.ID , Score, fill = F) %>% select(-row) 

imaging.final = group_by(imaging2, Hospital.Name, State) %>% 
  summarise_all(sum) 
