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
c_acc <- merge(dist, acc_sep) %>% 
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


test <- merge(s_dist, ml_acc) %>% 
  merge(ll_acc) %>% 
  mutate(acc_type = "Side")