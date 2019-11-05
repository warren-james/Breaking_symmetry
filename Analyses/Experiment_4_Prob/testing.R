# library 
library(tidyverse)

# work out opt values 
load("scratch/new_data/acc_sep")
load("scratch/new_data/df_part2")

# quick pre-process 
df_part2 <- df_part2 %>% 
  mutate(max_chance = ifelse(bias_type == "symmetric", 0.5, 0.8))

# dists for each participant 
c_dist <- df_part2 %>% 
  select(participant, bias_type, separation, max_chance) %>% 
  group_by(participant, bias_type, max_chance) %>% 
  distinct(separation)

# get acc for this strat
c_acc <- merge(c_dist, acc_sep) %>% 
  mutate(acc_type = "Centre")

# side strat version 
s_dist <- c_dist %>% 
  ungroup() %>% 
  mutate(ML_dist = 1,
         LL_dist = separation * 2)

ml_acc <- acc_sep %>% 
  mutate(ML_dist = separation,
         ML_acc = accuracy) %>% 
  select(-separation,
         -accuracy)
ll_acc <- acc_sep %>% 
  mutate(LL_dist = separation,
         LL_acc = accuracy) %>% 
  select(-separation,
         -accuracy)


s_acc <- merge(s_dist, ml_acc) %>% 
  merge(ll_acc) %>% 
  mutate(acc_type = "Side",
         accuracy = (ML_acc * max_chance) + (LL_acc * (1 - max_chance))) %>% 
  select(separation, participant, bias_type, max_chance, accuracy, acc_type)

# bind together 
acc_sep2 <- rbind(s_acc, c_acc) %>% 
  spread(acc_type, accuracy) %>% 
  mutate(opt_side = ifelse(Side > Centre, 1, 0))

# need to do the above on all accuracy data to look at the switch point
acc_c <- acc_sep %>% 
  rbind(acc_sep) %>%
  mutate(acc_type = "Centre") %>%
  mutate(bias_type = rep(c("symmetric", "bias"), each = length(acc_sep$separation)))

acc_ml <- acc_sep %>% 
  mutate(ML_dist = separation,
         ML_acc = accuracy) %>% 
  select(-separation,
         -accuracy)
acc_ll <- acc_sep %>% 
  mutate(LL_dist = separation,
         LL_acc = accuracy) %>% 
  select(-separation,
         -accuracy)

acc_s <- acc_sep %>% 
  mutate(ML_dist = 1,
         LL_dist = separation * 2) %>%
  merge(acc_ml) %>% 
  merge(acc_ll) %>% 
  mutate(accuracy_bias = (ML_acc * 0.8) + (LL_acc * 0.2),
         accuracy_symmetric = (ML_acc * 0.5) + (LL_acc * 0.5)) %>% 
  # select(participant, separation, accuracy_bias, accuracy_symmetric) %>% 
  gather(c(accuracy_bias, accuracy_symmetric),
         key = "bias_type", 
         value = "accuracy") %>% 
  separate(bias_type, 
           c("remove", "bias_type"),
           sep = "_") %>% 
  mutate(acc_type = "Side") %>% 
  select(participant, separation, accuracy, acc_type, bias_type)


# all together
acc_all <- rbind(acc_c, acc_s) %>% 
  spread(acc_type, accuracy) %>% 
  drop_na() %>% 
  mutate(opt_side = ifelse(Centre > Side, 0, 1))

# do some checks for each participant 
checking <- acc_all %>% 
  group_by(participant, bias_type) %>% 
  mutate(n = n()) %>% 
  summarise(prop = sum(opt_side)/unique(n))

# plot to have a look 
checking %>% 
  ggplot(aes(participant, prop,
             fill = bias_type)) + 
  geom_bar(stat = "identity",
           position = "dodge") + 
  see::scale_fill_flat() + 
  scale_y_continuous("Proportion of Distances participant should fixate the side box")
  
# plot diff in strat over distance 
acc_all %>% 
  mutate(diff = Side - Centre) %>% 
  ggplot(aes(separation, diff, colour = bias_type)) + 
  geom_line() + 
  facet_wrap(~participant) + 
  # see::scale_color_flat() + 
  scale_colour_viridis_d() + 
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "white") + 
  scale_y_continuous("Side - Centre") + 
  see::theme_blackboard() + 
  theme(strip.text.x = element_text(colour = "white"))
   