# ADFC's simple test script

library(tidyverse)
library(lme4)

source("my_functions.R")

#Things will break if this isn't 2! 
# We could generalise this at a later date
n_conditions <- 2

# there are on the log-odds scale
sigma_person <- 0.5
sigma_person_condition <- 0.1

n_itr <- 10
n_people <- 10
n_trial <- 10000
probs <- c(0.45, 0.55)


d <- sim_data(n_people, n_trial, n_conditions)

d %>% group_by(person, condition) %>%
  summarise(acc = mean(response)) %>%
  spread(condition, acc) %>%
  rename('condition1' = '1', 'condition2' = '2') %>%
  ggplot(aes(condition1, condition2)) + geom_point()

