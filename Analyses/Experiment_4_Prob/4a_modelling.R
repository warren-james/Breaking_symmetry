#### Modelling probability matching study ####
# Models to make:
#   - Fixations to "most likely" side without the LFA 
#   - Same as above but for accuracy 



#### Library ####
library(tidyverse)
library(brms)
library(rstan) 
# NB: Not sure which to use just yet... we'll try both though 

#### Load data ####
load("scratch/new_data/df_part2")
load("scratch/new_data/AccMea")


#### Sort Model data ####
model_data_fixations <- df_part2 %>% 
  mutate(fixated_likely = ifelse(standard_boxes == "most likely", 1, 0),
         fixated_centre = ifelse(standard_boxes == "centre", 1, 0),
         fixated_side = 1 - fixated_centre) %>% # Should we add in dist_type?
  select(participant, 
         bias_type, 
         separation, 
         fixated_likely,
         fixated_centre,
         fixated_side)


