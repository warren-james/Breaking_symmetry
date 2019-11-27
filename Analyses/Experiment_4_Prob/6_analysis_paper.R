#### Analysis for Probability study ####
# These are the analyses that will be included in the paper

#### Library ####
library(tidyverse)
library(lme4)
library(brms)

#### functions ####

#### load data ####
load("scratch/new_data/df_part2_fixed")

# processing 
df_model <- df_part2_fixed %>% 
  filter(separation != 640) %>% # remove furthest point for now
  select(participant, dist_type, bias_type, separation, st_box, accuracy) %>% 
  mutate(Ml_fix = ifelse(st_box == "most likely", 1, 0),
         C_fix = ifelse(st_box == "centre", 1, 0),
         ) # rescale separation to avoid 

#### analysis ####
#### Frequentist ####
# likely fixation ~ bias_type
m_fix_like <- glmer(Ml_fix ~ bias_type + (bias_type|participant), 
                    data = df_model,
                    family = "binomial")

# add in separation? 
m_fix_like_sep <- glmer(Ml_fix ~ (bias_type + dist_type)^2 + (dist_type + bias_type|participant), 
                    data = df_model,
                    family = "binomial")

# centre fixation ~ bias_type 
m_fix_centre <- glmer(C_fix ~ bias_type + (1|participant),
                      data = df_model,
                      family = "binomial")


#### Bayesian ####
# most complex model we need?
bm_fix_like_dt <- brm(Ml_fix ~ (bias_type + dist_type)^2 + (dist_type + bias_type|participant),
                      data = df_model,
                      family = "bernoulli",
                      chains = 1,
                      iter = 1000,
                      warmup = 500)
# let's remove random effects all together and see how it changes 
test <- brm(Ml_fix ~ (bias_type + dist_type)^2,
            data = df_model,
            family = "bernoulli",
            chains = 1,
            iter = 1000,
            warmup = 500)
