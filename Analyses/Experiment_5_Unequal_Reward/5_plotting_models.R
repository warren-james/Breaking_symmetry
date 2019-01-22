#### Gambling Study - Plotting model output ####
# Now we'll start to plot the models

#### library ####
library(tidyverse)
library(rstan)
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
  ggthemes::scale_fill_ptol()
plt$labels$x <- "Normalised Hoop Delta"
plt$labels$y <- "Normalised Standing Position"
plt$labels$colour <- "Split"
plt$labels$fill <- "Split"

