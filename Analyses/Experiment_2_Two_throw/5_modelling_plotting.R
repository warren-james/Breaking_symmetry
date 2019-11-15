#### Modelling of the Two Hoops data ####
# Using the Bayes approach now 

#### Library ####
library(tidybayes)
library(tidyverse)
library(brms)
library(rstan)

#### Constants ####

#### Functions ####

#### Load in data ####
load("scratch/df_part2")

#### sort data ####
model_data_pos <- df_part2 %>%
  group_by(Participant) %>%
  mutate(norm_delta = HoopDelta/max(HoopDelta)) %>%
  ungroup() %>%
  filter(abspos <= 1) %>% 
  mutate(abspos = (abspos+1e-4)*(1-1e-3))

# to be sorted
# model_data_acc

#### Modelling ####
#### Modelling: Position ####
#### m1_pos: abspos ~ norm_delta ####
m1_pos <- brm(abspos ~ norm_delta,
              family = "beta",
              data = model_data_pos,
              cores = 1,
              chains = 1,
              iter = 2000)


#### m2_pos: abspos ~ norm_delta + Num_throws ####
m2_pos <- brm(abspos ~ norm_delta + Num_throws,
              family = "beta",
              data = model_data_pos,
              cores = 1,
              chains = 1,
              iter = 2000)



#### m3_pos: abspos ~ (norm_delta + Num_throws)^2 ####
m3_pos <- brm(abspos ~ (norm_delta + Num_throws)^2,
              family = "beta",
              data = model_data_pos,
              cores = 1,
              chains = 1,
              iter = 2000)



