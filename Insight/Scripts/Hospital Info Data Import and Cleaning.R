pacman::p_load(stringr, ggplot2, car, effects, lme4, lmerTest, dplyr, reshape2, tidyr, sjPlot, nlme)

hospi.info = read.csv("Data/Hospital_Revised_Flatfiles/Hospital General Information.csv") %>% 
select(Hospital.Name, State, City, County.Name, Hospital.Type, 
       Hospital.Ownership, Emergency.Services, Meets.criteria.for.meaningful.use.of.EHRs, 
       Hospital.overall.rating, Mortality.national.comparison, Safety.of.care.national.comparison,
       Readmission.national.comparison, Patient.experience.national.comparison, 
       Effectiveness.of.care.national.comparison,
       Timeliness.of.care.national.comparison, Efficient.use.of.medical.imaging.national.comparison) %>% 
    tbl_df()
hospi.info.final = select(hospi.info, Hospital.Name : Meets.criteria.for.meaningful.use.of.EHRs)
#save(hospi.info.final, file = "Data/hospital info.rda")

struct = read.csv("Data/Hospital_Revised_Flatfiles/Structural Measures - Hospital.csv")
struct.n = distinct(struct, Measure.ID, Measure.Name) 

struct2 = select(struct, Hospital.Name, State, Measure.ID, Measure.Response) %>% 
  mutate(Measure.Response = as.character(Measure.Response)) %>% 
  mutate(Measure.Response = ifelse(Measure.Response == "N", "No", Measure.Response)) %>% 
  mutate(Measure.Response = ifelse(Measure.Response == "Y", "Yes", Measure.Response))

### NEED SOME WORK HERE ###  
struct2$row <- 1:nrow(struct2)
struct3 = spread(struct2, Measure.ID, Measure.Response, fill = F) %>% select(-row) %>% 
  mutate()
### Scores not well calculated, had to recalculate them
struct.final = group_by(struct2, Hospital.Name, State) %>% 
  summarise_all(sum) 
