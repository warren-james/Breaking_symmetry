#### Gambling Study - Plotting model output ####
# Now we'll start to plot the models

#### library ####
library(tidyverse)
library(rstan)
library(tidybayes)
library(brms)

#### load data ####
load("scratch/data/model_data")

#### m1 Dist ~ Delta ####
# This model should be pretty bad at the moment... so don't put any faith in it 

# load in data 
load("scratch/model_outputs/m1_ouput")

# extract samples
post_m1 <- rstan::extract(m1)

intercept <- mean(post_m1$a)
b_delta <- mean(post_m1$b_delta)

# plot this... 
model_data %>%
  mutate(p = Norm_Delta * b_delta + intercept) %>%
  ggplot(aes(Norm_Delta, Norm_Dist, colour = Gamble_Type)) + 
  geom_point() + 
  geom_line(aes(Norm_Delta, p))


#### m2 Dist ~ Delta + Gamble_type ####
# again, not great, should talk to Alasdair about getting good beta models going...
# This one is really bad... work on it 

# load data 
load("scratch/model_outputs/m2_ouput")

# extract samples 
post_m2 <- rstan::extract(m2)

intercept <- mean(post_m2$a)
b_delta <- mean(post_m2$b_delta)
b_unequal <- mean(post_m2$b_unequal)

# plot it 
model_data %>% 
  mutate(p = Norm_Delta * b_delta + Unequal * b_unequal + intercept) %>%
  ggplot(aes(Norm_Delta, Norm_Dist, colour = Gamble_Type)) + 
  geom_jitter() + 
  geom_line(aes(Norm_Delta, p, colour = Gamble_Type))


#### m3 - Norm_Dist ~ (Norm_Delta + Unequal)^2 ####
load("scratch/model_outputs/m3_ouput")
post_m3 <- rstan::extract(m3)

intercept <- mean(post_m3$a)
b_delta <- mean(post_m3$b_delta)
b_unequal <- mean(post_m3$b_unequal)
b_du <- mean(post_m3$b_du)

# plot it 
model_data %>% 
  mutate(p = Norm_Delta * b_delta + Unequal * b_unequal +
           Norm_Delta * Unequal * b_du + intercept) %>%
  ggplot(aes(Norm_Delta, Norm_Dist, colour = Gamble_Type)) + 
  geom_jitter() + 
  geom_line(aes(Norm_Delta, p, colour = Gamble_Type))



#### m6 - Norm_Dist ~ (Norm_Delta + Unequal)^2 Beta distritbution used ####
# still need to figure out how to plot this

#### BRMS models ####
# first one 
load("scratch/model_outputs/m_brms")

# get marginal effects 
post <- marginal_effects(m_brms)

# make quick plots to show interaction
plt <- plot(post, plot = F)[[3]] + 
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol() + 
  theme_bw()
plt$labels$x <- "Normalised Hoop Delta"
plt$labels$y <- "Normalised Standing Position"
plt$labels$colour <- "Split"
plt$labels$fill <- "Split"
plt

# save this plot 
ggsave("../../Figures/Experiment_5_Unequal_Reward/Model_output.png",
       height = 12,
       width = 18,
       units = "cm")

#### BRMS: posterior predictions ####
plt <- model_data %>%
  add_predicted_draws(m_brms) %>%
  ungroup() %>%
  mutate(Norm_Delta = round(Norm_Delta, digits = 3)) %>%
  filter(Norm_Delta == 1 | Norm_Delta == 0.2 | Norm_Delta == 0.529) %>%
  ggplot(aes(.prediction,
             colour = Gamble_Type,
             fill = Gamble_Type)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol() + 
  facet_wrap(~Norm_Delta)
plt

#### BRMS: dist_type ####
# load model 
load("scratch/model_outputs/m_brms_v2")

# plot 
plt <- model_data %>%
  add_predicted_draws(m_brms_v2) %>%
  ggplot(aes(.prediction,
             colour = Gamble_Type,
             fill = Gamble_Type)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol() + 
  facet_wrap(~dist_type)
plt
