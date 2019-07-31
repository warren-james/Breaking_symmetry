#### Modelling probability matching study ####
# Models to make:
#   - fix to "most likely" side without the LFA 
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
m_data_fix <- df_part2 %>% 
  mutate(participant = as.factor(participant),
         fixated_likely = ifelse(standard_boxes == "most likely", 1, 0),
         fixated_centre = ifelse(standard_boxes == "centre", 1, 0),
         fixated_side = 1 - fixated_centre) %>% # Should we add in dist_type?
  select(participant, 
         bias_type, 
         separation, 
         fixated_likely,
         fixated_centre,
         fixated_side)

# remove lfa separation for now
m_data_fix_trim <- m_data_fix %>%
  group_by(participant) %>%
  filter(separation != max(separation)) %>%
  mutate(separation = separation/max(separation))


#### MODELS ####
#### MODELS: Fixated "likely" ####
#### MODELS: m_fl_1 - likey ~ bias_type ####
m_fl_1 <- brm(fixated_likely ~ bias_type, 
              data = m_data_fix_trim,
              family = "bernoulli",
              cores = 1, 
              chains = 1, 
              iter = 2000,
              warmup = 1000)

# add rand intercepts
m_fl_1_1 <- brm(fixated_likely ~ bias_type + (1|participant),
                data = m_data_fix_trim,
                family = "bernoulli",
                cores = 1,
                chains = 1,
                iter = 2000,
                warmup = 1000)

# add rand effects 
# doesn't run well...
# m_fl_1_2 <- brm(fixated_likely ~ bias_type + (1 + bias_type|participant),
#             data = m_data_fix_trim,
#             family = "bernoulli",
#             cores = 1,
#             chains = 1,
#             iter = 2000,
#             warmup = 1000)

#### MODELS: m_fl_2 - likely ~ bias_type + separation ####
m_fl_2 <- brm(fixated_likely ~ (bias_type + separation)^2, 
              data = m_data_fix_trim,
              family = "bernoulli",
              cores = 1, 
              chains = 1, 
              iter = 2000,
              warmup = 1000)

# add rand intercepts
# issues here too...
# m2_1 <- brm(fixated_likely ~ (bias_type + separation)^2 + (1|participant),
#             data = m_data_fix_trim,
#             family = "bernoulli",
#             cores = 1,
#             chains = 1,
#             iter = 2000,
#             warmup = 1000)

#### MODELS: Fixated centre ####
#### MODELS: m_fc_1 - centre ~ bias_type ####
m_fc_1 <- brm(fixated_centre ~ bias_type, 
              data = m_data_fix_trim,
              family = "bernoulli",
              cores = 1, 
              chains = 1, 
              iter = 2000,
              warmup = 1000)


#### MODELS: m_fc_2 - centre ~ (bias_type + separation)^2 ####
m_fc_2 <- brm(fixated_centre ~ (bias_type + separation)^2, 
              data = m_data_fix_trim,
              family = "bernoulli",
              cores = 1, 
              chains = 1, 
              iter = 2000,
              warmup = 1000)
