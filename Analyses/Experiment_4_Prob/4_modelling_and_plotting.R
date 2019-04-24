#### Do Some modelling of the Prob_match study ####
# Again, this is proportion data I guess so we should use beta dist?
# Use this on proportion to one side I guess?
# But we have the issue of there being three options

# ... Probably need to rescale it so 0 is left or right, and 0.5 is centre?
# then we can use more likely left vs. right?
# or just make it so 1 is most likely side in bias condition and left in the random one?

# Try using binomial data 
# make predicted "fixated common" then use bias_type as a predictor
# can also included separation or close vs far to check things

#### Library ####
library(rstan)
library(tidyverse)
library(brms)
library(tidybayes)

#### Constants ####
# Gets Visual Degrees
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Any constants ####
Screen_dist <- 54.4
ppcm <- 1920/54

#### load in data ####
load("scratch/new_data/df_part2")

#### Sort data ####
model_data <- df_part2 %>% 
  select(participant, bias, block, trial, separation, fixated_box, standard_boxes, bias_type) %>%
  group_by(participant) %>%
  mutate(Delta = separation/max(separation)) %>%
  ungroup() %>% 
  mutate(fixated_likely = as.factor(ifelse(standard_boxes == "most likely", 1, 0)),
         fixated_centre = as.factor(ifelse(standard_boxes == "centre", 1, 0)))

#### Modelling ####
#### Modelling: Centre Vs. Side ####
#### m1: fixated_likely ~ bias_type ####
m1_centre_binom <- brm(fixated_centre ~ bias_type, 
                       data = model_data,
                       family = "bernoulli",
                       cores = 1,
                       chains = 1,
                       iter = 2000)

#### m2: fixated_likely ~ bias_type + Delta ####
m2_centre_binom <- brm(fixated_centre ~ bias_type + Delta, 
                       data = model_data,
                       family = "bernoulli",
                       cores = 1,
                       chains = 1,
                       iter = 2000)

#### m3: fixated_likely ~ (bias_type + Delta)^2 ####
m3_centre_binom <- brm(fixated_centre ~ (bias_type + Delta)^2, 
                       data = model_data,
                       family = "bernoulli",
                       cores = 1,
                       chains = 1,
                       iter = 2000)

# mess about with this to get it working
plt <- model_data %>%
  add_predicted_draws(m3_centre_binom) %>%
  group_by(.row, bias_type, Delta) %>%
  summarise(.prediction = mean(.prediction)) %>%
  ungroup() %>%
  filter(Delta == max(Delta) | Delta == min(Delta)) %>%
  mutate(Delta = as.factor(Delta)) %>%
  ggplot(aes(.prediction,
             colour = Delta,
             fill = Delta)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  ggthemes::scale_color_ptol() +
  ggthemes::scale_fill_ptol() + 
  facet_wrap(~bias_type)
plt

#### Modelling: Fixated Likely side ####
#### modelling: binomial #####
#### m1: fixated_likely ~ bias_type ####
m1_likely_binom <- brm(fixated_likely ~ bias_type, 
                       data = model_data,
                       family = "bernoulli",
                       cores = 1,
                       chains = 1,
                       iter = 2000)

#### m2: fixated_likely ~ bias_type + Delta ####
m2_likely_binom <- brm(fixated_likely ~ bias_type + Delta, 
                       data = model_data,
                       family = "bernoulli",
                       cores = 1,
                       chains = 1,
                       iter = 2000)

#### m3: fixated_likely ~ (bias_type + Delta)^2 ####
m3_likely_binom <- brm(fixated_likely ~ (bias_type + Delta)^2, 
                       data = model_data,
                       family = "bernoulli",
                       cores = 1,
                       chains = 1,
                       iter = 2000)

#### modelling: beta ####
# sort out data first 
model_data_beta <- model_data %>%
  group_by(participant, bias_type) %>%
  summarise(fixated_likely = mean(fixated_likely)) %>%
  mutate(fixated_likely = fixated_likely + 1e-4) %>%
  ungroup()

#### m1: fixated_likely ~ bias_type ####
m1_likely_beta <- brm(fixated_likely ~ bias_type, 
                      data = model_data_beta,
                      family = "beta",
                      cores = 1,
                      chains = 1,
                      iter = 2000)


#### Plotting ####
#### plotting: beta ####
# plot m1, most basic model beta
plt <- model_data_beta %>%
  add_predicted_draws(m1_likely_beta) %>%
  # ungroup() %>%
  # mutate(Norm_Delta = round(Norm_Delta, digits = 3)) %>%
  # filter(Norm_Delta == 1 | Norm_Delta == 0.2 | Norm_Delta == 0.529) %>%
  ggplot(aes(.prediction,
             colour = bias_type,
             fill = bias_type)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol()
plt






