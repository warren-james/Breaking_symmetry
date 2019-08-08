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

 # save
save(m_data_fix, file = "modelling/Stan/model_data/m_data_fix")

# remove lfa separation for now
m_data_fix_trim <- m_data_fix %>%
  group_by(participant) %>%
  filter(separation != max(separation)) %>%
  mutate(separation = separation/max(separation))

# save
save(m_data_fix_trim, file = "modelling/Stan/model_data/m_data_fix_trim")

#### MODELS ####
#### MODELS: Fixated "likely" ####
#### MODELS: m_fl_1 - likey ~ bias_type ####
# m_fl_1 <- brm(fixated_likely ~ bias_type,
#               data = m_data_fix_trim,
#               family = "bernoulli",
#               cores = 1,
#               chains = 1,
#               iter = 2000,
#               warmup = 1000)
# save 
# save(m_fl_1, file = "modelling/Brms/model_output/m_fl_1")

# add rand intercepts
# m_fl_1_1 <- brm(fixated_likely ~ bias_type + (1|participant),
#                 data = m_data_fix_trim,
#                 family = "bernoulli",
#                 cores = 1,
#                 chains = 1,
#                 iter = 2000,
#                 warmup = 1000)
# save 
# save(m_fl_1_1, file = "modelling/Brms/model_output/m_fl_1_1")

# add rand effects 
m_fl_1_2 <- brm(fixated_likely ~ bias_type + (bias_type|participant),
            data = m_data_fix_trim,
            family = "bernoulli",
            cores = 1,
            chains = 1,
            iter = 2000,
            warmup = 1000)

#### MODELS: m_fl_2 - likely ~ bias_type + separation ####
# m_fl_2 <- brm(fixated_likely ~ (bias_type + separation)^2, 
#               data = m_data_fix_trim,
#               family = "bernoulli",
#               cores = 1, 
#               chains = 1, 
#               iter = 2000,
#               warmup = 1000)

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
# m_fc_1 <- brm(fixated_centre ~ bias_type, 
#               data = m_data_fix_trim,
#               family = "bernoulli",
#               cores = 1, 
#               chains = 1, 
#               iter = 2000,
#               warmup = 1000)


#### MODELS: m_fc_2 - centre ~ (bias_type + separation)^2 ####
# m_fc_2 <- brm(fixated_centre ~ (bias_type + separation)^2, 
#               data = m_data_fix_trim,
#               family = "bernoulli",
#               cores = 1, 
#               chains = 1, 
#               iter = 2000,
#               warmup = 1000)

#### STAN MODELS ####
#### STAN: m1 - likely ~ bias_type ####
m_matrix <- model.matrix(fixated_likely ~ bias_type, data = m_data_fix_trim)

stan_df <- list(
  N = nrow(m_data_fix_trim),
  K = ncol(m_matrix),
  y = m_data_fix_trim$fixated_likely,
  X = m_matrix
)

m1_fl_berno <- stan(
  file = "modelling/Stan/models/berno.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save output 
save(m1_fl_berno, file = "modelling/Stan/model_outputs/m1_fl_berno")

#### STAN: m2 - likely ~ (bias_type + separation)^2 ####
m_matrix <- model.matrix(fixated_likely ~ (bias_type + separation)^2, data = m_data_fix_trim)

stan_df <- list(
  N = nrow(m_data_fix_trim),
  K = ncol(m_matrix),
  y = m_data_fix_trim$fixated_likely,
  X = m_matrix
)

m2_fl_berno <- stan(
  file = "modelling/Stan/models/berno.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save output 
save(m2_fl_berno, file = "modelling/Stan/model_outputs/m2_fl_berno")
