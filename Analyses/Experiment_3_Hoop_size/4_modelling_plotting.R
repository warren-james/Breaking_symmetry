#### Modelling and plotting Hoop Size ####

#### Library ####
library(brms)
library(rstan)
library(tidybayes)
library(tidyverse)

#### Constants ####
Hoop_size <- 0.46

#### Functions ####

#### Load in data ####
load("scratch/df_part2_norm") 

# sort out data for modelling? 
model_data <- norm_dat %>%
  select(participant, hoop_pos, norm_dist) %>% 
  group_by(participant) %>% 
  mutate(norm_hoop_pos = hoop_pos/max(hoop_pos)) %>%
  ungroup() %>% 
  filter(norm_dist >= -1, norm_dist <= 1) %>% 
  mutate(norm_dist2 = (norm_dist + 1)/2)
model_data$norm_dist2[model_data$norm_dist2 == 1] <- 1-1e-5

# Should we model this as a beta dist and make centre <- 0.5?


#### Quick Plots ####
# density plot 
model_data %>% 
  ggplot(aes(norm_dist)) + 
  geom_density()

#### Modelling ####
#### Modelling: New idea ####
# raw norm_dist
m1 <- brm(norm_dist ~ norm_hoop_pos, 
          data = model_data,
          family = "normal",
          cores = 1,
          chains = 1,
          iter = 2000)

# plot posterior?
plt <- model_data %>%
  add_predicted_draws(m1) %>%
  ggplot(aes(.prediction)) + 
  geom_density() + 
  theme_minimal()
plt

# scaled_norm_dist
m2 <- brm(norm_dist2 ~ norm_hoop_pos,
          data = model_data,
          family = "beta",
          cores = 1,
          chains = 1,
          iter = 2000)

plt <- model_data %>%
  add_predicted_draws(m2) %>%
  ggplot(aes(.prediction)) + 
  geom_density() + 
  theme_minimal()
plt

#### Modelling: Paper idea ####
load("scratch/df_part2_raw")
model_data_2 <- dat %>%
  filter(subject_position != 0)
# tidy 
rm(dat)
# add in predictors
# stood_left
model_data_2$stood_left <- 0 
model_data_2$stood_left[model_data_2$subject_position < 0] <- 1
# small_hoop_left
model_data_2$small_hoop_left <- 0 
model_data_2$small_hoop_left[model_data_2$small_pos < 0] <- 1
model_data_2$small_hoop_left <- as.factor(model_data_2$small_hoop_left)

# model 
m3 <- brm(stood_left ~ small_hoop_left, 
          data = model_data_2,
          family = "bernoulli",
          chains = 1,
          cores = 1,
          iter = 2000)

plt <- model_data_2 %>%
  add_predicted_draws(m3) %>%
  group_by(.row, small_hoop_left) %>%
  summarise(.prediction = mean(.prediction)) %>%
  ggplot(aes(.prediction,
             colour = small_hoop_left,
             fill = small_hoop_left)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  ggthemes::scale_color_ptol() +
  ggthemes::scale_fill_ptol()
plt$labels$colour <- "Small Hoop Left:"
plt$labels$fill <- "Small Hoop Left:"
plt

