#### Modelling and plotting Hoop Size ####

#### Library ####
library(brms)
library(rstan)
library(tidybayes)
library(tidyverse)

#### Constants ####
Hoop_size <- 0.46

#### Functions ####
# This function squashes the range of values so as to be used in a beta regression 
squash <- function(y, max, min, squash){
  y <- y * ((max-squash) - (min + squash)) + (min + squash)
}

# get draws 
draw_post <- function(model, data){
  close <- min(data$norm_hoop_pos)
  mid <- median(data$norm_hoop_pos)
  far <- max(data$norm_hoop_pos)
  
  draws_df <- model %>% 
    spread_draws(b_Intercept,
                 b_norm_hoop_pos) %>% 
    mutate(hoop_Close = b_Intercept + close * b_norm_hoop_pos,
           hoop_Mid = b_Intercept + mid * b_norm_hoop_pos,
           hoop_Far = b_Intercept + far * b_norm_hoop_pos) %>% 
    select(.iteration,
           hoop_Close,
           hoop_Mid,
           hoop_Far) %>% 
    gather(c(hoop_Close:hoop_Far),
           key = "parameter",
           value = "estimate") %>% 
    separate(parameter,
             into = c("remove", "Distance Type")) %>% 
    select(-remove) 
  # estimates
  plt_estimates <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>% 
    ggplot(aes(prop,
               colour = `Distance Type`,
               fill = `Distance Type`)) +
    geom_density(alpha = .3) + 
    see::scale_color_flat() + 
    see::scale_fill_flat() + 
    scale_x_continuous("Normalised Delta") + 
    theme_bw()
  # amount above .5
  prop_above.5 <- draws_df %>%
    mutate(above0_5 = ifelse(boot::inv.logit(estimate) > .5, 1, 0)) %>%
    summarise(above0_5 = mean(above0_5))
  # get hdi 
  draws_hdi <- draws_df %>%
    group_by(`Distance Type`) %>%
    mutate(prop = boot::inv.logit(estimate)) %>%
    summarise(lower = hdi(prop)[,1],
              mean = mean(prop),
              upper = hdi(prop)[,2],
              med = median(prop)) 
  # overall hdi 
  draws_hdi_overall <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>% 
    summarise(lower = hdi(prop)[,1],
              mean = mean(prop),
              upper = hdi(prop)[,2],
              med = median(prop)) 
  
  Hdi_pos <- list(draws_hdi,
                  draws_hdi_overall)
  output <- list(draws_df,
                 plt_estimates,
                 prop_above.5,
                 Hdi_pos)
  names(output) <- c("draws_df",
                     "plt_estimates",
                     "Prop_above.5",
                     "draws_HDI")
  return(output)
}


#### Load in data ####
load("scratch/df_part2_norm") 

# sort out data for modelling? 
model_data <- norm_dat %>%
  select(participant, hoop_pos, norm_dist) %>% 
  group_by(participant) %>% 
  mutate(norm_hoop_pos = hoop_pos/max(hoop_pos)) %>%
  ungroup() %>% 
  filter(norm_dist >= -1, norm_dist <= 1) %>% 
  mutate(norm_dist2 = (norm_dist + 1)/2,
         norm_dist2 = squash(norm_dist2, 1, 0, 1e-4))
# Should we model this as a beta dist and make centre <- 0.5?


#### Quick Plots ####
# density plot 
model_data %>% 
  ggplot(aes(norm_dist)) + 
  geom_density() + 
  geom_histogram(aes(y = ..density..))

#### Modelling ####
#### Modelling: New idea ####
# normalised dist  sp > .5 =close to small, sp < .5 close to big
m1 <- brm(norm_dist2 ~ norm_hoop_pos + (norm_hoop_pos|participant), 
          data = model_data,
          family = "beta",
          prior = c(set_prior("student_t(3,0,3)", class = "b")),
          cores = 1,
          chains = 1,
          iter = 2000,
          warmup = 1000)

# draw from samples 
draws <- draw_post(m1, model_data)


# plot posterior?
plt <- model_data %>%
  add_predicted_draws(m1) %>%
  ggplot(aes(boot::inv.logit(.prediction))) + 
  geom_density() + 
  theme_minimal()
plt


