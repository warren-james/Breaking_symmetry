#### Modelling of the Two Hoops data ####
# Using the Bayes approach now 

#### Library ####
library(tidybayes)
library(tidyverse)
library(brms)
library(rstan)

#### Constants ####

#### Functions ####
# This function squashes the range of values so as to be used in a beta regression 
squash <- function(y, max, min, squash){
  y <- y * ((max-squash) - (min + squash)) + (min + squash)
}

# get draws 
source("Functions.R")

#### Load in data ####
load("scratch/df_part2")

#### sort data ####
model_data_pos <- df_part2 %>%
  group_by(Participant) %>%
  mutate(norm_delta = HoopDelta/max(HoopDelta)) %>%
  ungroup() %>%
  filter(abspos <= 1) %>% 
  mutate(abspos = squash(abspos, 1, 0, 1e-4))

# save 
save(model_data_pos, file = "scratch/model_data_pos")

# to be sorted
# model_data_acc

#### Modelling ####
#### Modelling: Position ####
#### m1_pos: abspos ~ norm_delta * Num_throws ####
m1_pos <- brm(abspos ~ norm_delta * Num_throws + (norm_delta * Num_throws|Participant),
              family = "beta",
              data = model_data_pos,
              prior = c(set_prior("student_t(3,0,3)", class = "b")),
              cores = 1,
              chains = 1,
              iter = 2000,
              warmup = 1000)

# save 
save(m1_pos, file = "modelling/model_outputs/m1_pos")

# get draws 
draws <- draw_post_delta(m1_pos, model_data_pos)

# save 
save(draws, file = "modelling/model_outputs/m1_pos_draws")

#### Modelling: Accuracy ####
#### m1_acc: exp_acc ~ Num_throws ####
# at the moment, we don't care about distance... 
# just overall accuracy for now...
# load data 
load("scratch/df_wit_acc")
model_data_acc <- df_new %>% 
  group_by(Participant, Num_throws) %>% 
  summarise(mu_Actacc = mean(Accuracy),
            mu_Expacc = mean(ExpAcc))

m1_acc <- brm(mu_Expacc ~ Num_throws + (1 + Num_throws|Participant),
              data = model_data_acc,
              family = "beta",
              chains = 1, 
              iter = 2000,
              warmup = 1000)

# save 
save(m1_acc, file = "modelling/model_outputs/m1_acc")

# get draws 
draws <- draw_post_acc(m1_acc, model_data_acc, "Expected Accuracy")
  
# save this 
save(draws, file = "modelling/model_outputs/m1_acc_draws")
  