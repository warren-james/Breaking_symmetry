# library 
library(tidyverse)

# work out opt values 
load("scratch/new_data/acc_sep")
load("scratch/new_data/df_part2")
load("scratch/new_data/AccMea")

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
  see::scale_color_flat() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "white") + 
  scale_y_continuous("Side - Centre")
   
#### Sort out Expected and Optimal accuracy ####
acc_opt <- acc_all %>% 
  mutate(Optimal = pmax(Centre, Side)) %>% 
  select(participant, separation, Optimal) %>% 
  distinct()

r_acc <- acc_c %>%
  mutate(r_dist = separation,
         r_acc = accuracy) %>% 
  select(participant, r_dist, r_acc) %>% 
  distinct()
l_acc <- acc_c %>%
  mutate(l_dist = separation,
         l_acc = accuracy) %>% 
  select(participant, l_dist, l_acc) %>%
  distinct()
c_acc <- acc_c %>% 
  mutate(c_dist = separation, 
         c_acc = accuracy) %>% 
  select(participant, c_dist, c_acc) %>% 
  distinct()
fix_acc <- acc_c %>% 
  mutate(fix_dist = separation,
         fix_acc = accuracy) %>% 
  select(participant, fix_dist, fix_acc) %>% 
  distinct()
far_acc <- acc_c %>% 
  mutate(far_dist = separation,
         far_acc = accuracy) %>%
  select(participant, far_dist, far_acc) %>% 
  distinct()

new_acc_measures <- df_part2 %>% 
  select(participant, lcr, standard_boxes, bias_type, bias_left, separation, fixated_box, accuracy) %>% 
  mutate(l_dist = ifelse(fixated_box == 1, separation, 
                         ifelse(fixated_box == 2, 1, 2*separation)),
         r_dist = ifelse(fixated_box == 1, separation, 
                         ifelse(fixated_box == 3, 1, 2*separation)),
         c_dist = separation,
         fix_dist = 1,
         far_dist = separation * 2,
         ll_bias = 1 - bias_left) %>% 
  merge(l_acc) %>% 
  merge(r_acc) %>% 
  merge(c_acc) %>%
  merge(far_acc) %>%
  merge(fix_acc) %>% 
  mutate(Expected = (l_acc * bias_left) + (r_acc * ll_bias),
         Centre = (c_acc * bias_left) + (c_acc * ll_bias),
         Side = (fix_acc * pmax(bias_left, ll_bias)) + (far_acc * (pmin(bias_left, ll_bias))),
         Optimal = pmax(Side, Centre))%>% 
  select(participant,
         lcr,
         standard_boxes,
         separation, 
         bias_type, 
         accuracy, 
         Expected,
         Centre,
         Side,
         Optimal)

# overall plot
plt_check <- new_acc_measures %>% 
  group_by(participant, bias_type, separation) %>% 
  summarise(Actual = mean(accuracy),
            Centre = mean(Centre),
            Side = mean(Side),
            Expected = mean(Expected),
            Optimal = mean(Optimal)) %>% 
  ungroup() %>% 
  mutate(diff_EO = Expected/Optimal,
         diff_CO = Centre/Optimal,
         participant = as.factor(as.numeric(participant))) %>% 
  ggplot(aes(separation, diff_EO, colour = bias_type)) + 
  geom_line() + 
  see::scale_colour_flat() + 
  facet_wrap(~participant) + 
  theme_bw() + 
  geom_line(aes(separation, diff_CO, colour = bias_type),
            linetype = "dashed") + 
  scale_y_continuous("Expected/Optimal") + 
  scale_x_continuous("Delta (pixels)")
plt_check

# plot side vs. centre 
plt_sc <- plt_check[["data"]] %>% 
  gather(c(Centre, Side),
         key = "strat",
         value = "acc") %>% 
  ggplot(aes(separation, acc, 
             colour = strat)) + 
  geom_line(aes(linetype = bias_type)) + 
  facet_wrap(~participant) + 
  guides(linetype = F)
plt_sc

# now take the above data and we can plot something like the boxplots from before 
plt_box_acc <- new_acc_measures %>% 
  mutate(dist_type = ifelse(Centre > Side, "close", "far")) %>% 
  group_by(participant, bias_type, dist_type) %>% 
  summarise(Actual = mean(accuracy),
            Centre = mean(Centre),
            Side = mean(Side),
            Expected = mean(Expected),
            Optimal = mean(Optimal)) %>% 
  ungroup() %>% 
  mutate(diff_EO = Expected/Optimal,
         diff_CO = Centre/Optimal,
         participant = as.factor(as.numeric(participant))) %>% 
  ggplot(aes(dist_type, diff_EO,
             colour = bias_type,
             fill = bias_type)) + 
  geom_boxplot(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  scale_y_continuous("Expected/Optimal") + 
  scale_x_discrete("Distance Type")
plt_box_acc

#### recreate fixation prop plots with more accurate switch point ####
# labels should reflect participants' own biases in the "symmetric" condition 
# get dist type as well 
new_df <- new_acc_measures %>%
  mutate(dist_type = ifelse(Centre > Side, "close", "far")) %>% 
  select(participant, bias_type, lcr, standard_boxes, dist_type) %>%
  group_by(participant, bias_type, lcr, standard_boxes, dist_type) %>%
  summarise(n = n()) %>% 
  ungroup() %>%
  complete(lcr,
           nesting(participant, bias_type, standard_boxes, dist_type),
           fill = list(n = 0)) %>% 
  ungroup() %>%
  group_by(participant, bias_type, dist_type) %>%
  spread(lcr, n) %>% 
  mutate(most = ifelse(sum(`-1`) > sum(`1`), -1, 1),
         least = ifelse(sum(`-1`) < sum(`1`), -1, 1),
         # need to make sure that the biased group does not change 
         # will need to do this in the main df 
         lcr2 = ifelse(standard_boxes == "centre", 0,
                       ifelse(standard_boxes == "most likely", most, least)),
         n = ifelse(lcr2 == 0, max(`0`),
                    ifelse(lcr2 == 1, max(`1`), max(`-1`)))) %>% 
  select(participant, bias_type, standard_boxes, lcr2, most, least, dist_type) %>% 
  merge(new_acc_measures) %>% 
  mutate(st_box = ifelse(bias_type == "Biased", standard_boxes,
                         ifelse(lcr == 0, "centre",
                                ifelse(lcr == most, "most likely", "least likely"))))







