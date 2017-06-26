pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

readmission = read.csv("Data/Hospital_Revised_Flatfiles/Readmissions and Deaths - Hospital.csv")
read = read.csv("Data/Hospital_Revised_Flatfiles/Readmissions and Deaths - Hospital.csv") %>%  
  select(Hospital.Name, State, Measure.ID, Score) %>% mutate(Score = as.numeric(as.character(Score)))

read$row <- 1:nrow(read)
read2 = spread(read, Measure.ID, Score, fill = F) %>% select(-row) %>% 
  group_by(Hospital.Name, State) 
read.final = group_by(read2, Hospital.Name, State) %>% 
  summarise_all(sum) 


#### Define proper threshold
th = read.csv("Data/Hospital_Revised_Flatfiles/Readmissions and Deaths - Hospital.csv") %>%  
  select(Hospital.Name, State, Measure.ID, Score, Compared.to.National) %>% 
  mutate(Score = as.numeric(as.character(Score))) %>% filter( Measure.ID == "READM_30_HF")
group_by(th, Compared.to.National) %>% summarise(min = min(Score), max = max(Score), mean = mean(Score))
  

#dat2 = select(readmission, Hospital.Name, Measure.ID, Denominator) %>% filter(Measure.ID == "READM_30_HF") %>%
  select(Hospital.Name, tot.hf.read = Denominator)
#read.final = inner_join(read.final, dat2)

save(read.final, file = "Data/readmission.rda")
