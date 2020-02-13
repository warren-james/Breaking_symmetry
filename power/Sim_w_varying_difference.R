#### Try something about power ####
# library 
library(tidyverse)
library(lme4)

# load Alasdair's functions
source("my_functions.R")


# Take some variance 
sigma <- .2 

# for random slopes and effects
sigma_p <- .5
sigma_cond <- .1

# now setup pairs of means/size of effect 
mu1 <- seq(.55, .95, .1)
mu2 <- .5

# n's to test 
N_people <- seq(10,20,2)

# number of conditions
N_cond <- 2

# setup df
test <- tibble()
max_iter <- 1
for(i in 1:max_iter){
  for(N in N_people){
    for(mu in mu1){
      probs <- c(mu2, mu)
      eff <- mu - mu2
      
      # fill in data
      temp <- sim_data(N, n_trial = 100, N_cond, sigma_p, sigma_cond) %>% 
        mutate(eff = eff, 
               iter = i,
               N = N)
      test <- rbind(test,temp)
    }
  }
  print(paste((i/max_iter)*100, "%", sep = ""))
}

# plot this 
test %>% 
  mutate(condition = ifelse(condition == 1, "cond_1", "cond_2"),
         eff = as.factor(eff),
         N = as.factor(N)) %>%
  group_by(iter, person, condition, eff, N) %>% 
  summarise(prop = mean(response)) %>%
  spread(condition, prop) %>% 
  mutate(diff = cond_1 - cond_2) %>% 
  ggplot(aes(diff,
             colour = N, 
             fill = N)) + 
  geom_density(alpha = .1) +
  # geom_histogram(alpha = .3,
  #                position = "dodge") +
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  # facet_wrap(~eff) 
  facet_grid(N~eff)

