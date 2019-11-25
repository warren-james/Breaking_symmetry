#### Making plots for the paper ####
# probability matching 

#### Library ####
library(tidyverse)

#### functions ####

#### Load data ####
load("scratch/new_data/df_part2")
load("scratch/new_data/AccMea")
load("scratch/new_data/acc_sep")

#### Processing ####
df_part2 <- df_part2 %>% 
  mutate(dist_type = ifelse(separation < Switching_point,
                            "Close", "Far"),
         bias_type = ifelse(bias_type == "biased", "Biased", "Symmetric")) %>% 
  select(participant,
         trial,
         block,
         bias_type,
         bias_left,
         separation,
         dist_type,
         Switching_point,
         lcr,
         fixated_box,
         standard_boxes,
         accuracy)

#### NB: Really close... one point isn't working for some reason.... ####
# Need to work on labels 
# labels should reflect participants' own biases in the "symmetric" condition 
check <- df_part2 %>% 
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
  merge(df_part2) %>% 
  mutate(st_box = ifelse(bias_type == "Biased", standard_boxes,
                         ifelse(lcr == 0, "centre",
                                ifelse(lcr == most, "most likely", "least likely"))))

# make plot of this 
plt_fix <- check %>% 
  ungroup() %>% 
  group_by(participant, bias_type, dist_type) %>% 
  mutate(centre = ifelse(st_box == "centre", 1, 0),
         ML = ifelse(st_box == "most likely", 1, 0),
         LL = ifelse(st_box == "least likely", 1, 0),
         n = n()) %>%
  summarise(centre = mean(centre),
            ML = mean(ML),
            LL = mean(LL)) %>% 
  gather(centre:LL,
         key = "prop_type",
         value = "proportion") %>%
  mutate(prop_type = factor(prop_type, c("centre", "ML", "LL"),
                            labels = c("Centre", "Most Likely", "Least Likely"))) %>%
  ggplot(aes(dist_type, proportion,
             fill = bias_type,
             colour = bias_type)) +
  geom_boxplot(alpha = .3) +
  # ggplot(aes(proportion, fill = prop_type)) +
  # geom_density(alpha = .3) + 
  # geom_histogram(position = "dodge") +
  scale_y_continuous(element_blank(),
                     breaks = seq(0,1,.2),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~prop_type) +
  see::scale_fill_flat(name = "Condition") +
  see::scale_color_flat(name = "Condition") + 
  scale_x_discrete("Delta") + 
  theme_bw()
plt_fix 

# some quick descriptives 
plt_fix[["data"]] %>% 
  group_by(prop_type, dist_type, bias_type) %>% 
  summarise(mu = mean(proportion),
            med = median(proportion),
            quantiles = list(quantile(proportion))) %>%
            # iqr = qwraps2::median_iqr(proportion)) %>% 
  unnest %>% 
  mutate(Q = rep(c("min","Q1","median","Q3","max"), length(mu)/5)) %>% 
  spread(key = Q, value = quantiles) %>% 
  select(-max, -min, - median)

# check if you want to save 
# ggsave("../../Figures/Experiment_4_Prob/boxes_prop_CML.png",
#        width = 5.6,
#        heigh = 3.5)


#### PLOTS ####
#### centre vs side by switch point ####
# start simple 
# just proportion of fixations to the centre
df_part2 %>% 
  mutate(centre = ifelse(lcr == 0, 1, 0)) %>%
  group_by(participant, bias_type, dist_type) %>% 
  summarise(centre = mean(centre)) %>% 
  ggplot(aes(dist_type, centre, fill = bias_type)) + 
  geom_boxplot()

# comparing this by condition
# this one's weird.... make it simpler
df_part2 %>%
  mutate(centre = ifelse(lcr == 0, 1, 0)) %>% 
  group_by(participant, bias_type, dist_type) %>% 
  summarise(centre = mean(centre)) %>% 
  spread(key = "bias_type",
         value = centre) %>% 
  mutate(diff = Biased - Symmetric) %>% 
  ggplot(aes(dist_type, diff)) + 
  geom_boxplot()

df_part2 %>% 
  mutate(centre = ifelse(lcr == 0, 1, 0)) %>% 
  group_by(participant, bias_type, dist_type) %>% 
  summarise(centre = mean(centre)) %>% 
  ggplot(aes(dist_type, centre,
             fill = bias_type)) + 
  geom_boxplot() + 
  see::scale_fill_flat()

#### Sort out region plot ####
plt_region <- AccMea %>% 
  filter(Pred_type != "Expected",
         separation != 640) %>%
  ungroup() %>%
  spread(Pred_type, 
         Acc) %>%
  mutate(separation = separation/switch_point,
         ratio_cen_opt = Centre/Optimal,
         m_cen_op = Centre - Optimal) %>%
  group_by(participant, condition, separation) %>%
  summarise(mean_centre = mean(ratio_cen_opt))

# Exp / Opt
plt <- AccMea %>% 
  filter(separation != 640) %>% 
  group_by(participant, condition, separation) %>% 
  spread(Pred_type, 
         Acc) %>% 
  ungroup() %>% 
  mutate(Expected = round(Expected, 2),
         Optimal = round(Optimal, 2),
         Diff = Expected/Optimal,
         separation = separation/switch_point) %>% 
  ggplot(aes(separation, Diff, colour = condition)) +
  # geom_line(data = plt_region,
  #           aes(separation,
  #               mean_centre,
  #               group = condition,
  #               linetype = condition),
  #           colour = "green") +
  geom_hline(yintercept = 1, colour = "black",
             linetype = "dashed") + 
  geom_segment(aes(x = 1, y = 0,
                   xend = 1, yend = 1),
               colour = "black",
               linetype = "dashed") +
  # geom_vline(xintercept = 1, colour = "black",
  #            linetype = "dashed") + 
  geom_path(aes(group = interaction(condition, participant)), 
            alpha = 1) + 
  see::scale_colour_flat() + 
  theme_bw() +
  # see::theme_modern() + 
  coord_cartesian(expand = F)
plt$labels$y <- "Accuracy ratio (Expected/Optimal)"
plt$labels$x <- "Distance relative to switch point"
plt$labels$colour <- "Condition"
plt

# show by participant
# plt + facet_wrap(~participant)

#### Region of worst and best performance ####
# some pre processing 
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
  mutate(bias = (ML_acc * 0.8) + (LL_acc * 0.2),
         symmetric = (ML_acc * 0.5) + (LL_acc * 0.5)) %>% 
  select(-accuracy) %>% 
  # select(participant, separation, accuracy_bias, accuracy_symmetric) %>% 
  gather(c(bias, symmetric),
         key = "bias_type", 
         value = "accuracy") %>% 
  mutate(acc_type = "Side") %>% 
  select(participant, separation, accuracy, acc_type, bias_type)


# all together
acc_all <- rbind(acc_c, acc_s) %>% 
  spread(acc_type, accuracy) %>% 
  drop_na() %>% 
  mutate(opt_side = ifelse(Centre > Side, 0, 1))

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
  select(participant, block, trial, lcr, standard_boxes, bias_type, bias_left, separation, fixated_box, accuracy) %>% 
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
         Side_opt = (fix_acc * pmax(bias_left, ll_bias)) + (far_acc * (pmin(bias_left, ll_bias))),
         Side_nopt = (fix_acc * pmin(bias_left, ll_bias)) + (far_acc * (pmax(bias_left, ll_bias))),
         Optimal = pmax(Side_opt, Centre))%>% 
  select(participant,
         block,
         trial,
         lcr,
         standard_boxes,
         separation, 
         bias_type, 
         accuracy, 
         Expected,
         Centre,
         Side_opt,
         Side_nopt,
         Optimal)

# tidy 
rm(acc_s, acc_c, acc_opt, acc_ll, acc_ml, acc_all)
# tidy... a clever way? 
# test <- "acc" %in% list(ls())

df_regions <- new_acc_measures %>%
  group_by(participant) %>% 
  mutate(sep_factor = as.numeric(as.factor(separation))) %>%
  group_by(participant, sep_factor, bias_type) %>% 
  summarise(Expected = mean(Expected),
            Centre = mean(Centre),
            Side_opt = mean(Side_opt),
            Side_nopt = mean(Side_nopt)) %>% 
  ungroup() %>% 
  group_by(sep_factor, bias_type) %>%
  mutate(Best = pmax(Expected, Centre, Side_opt, Side_nopt),
         Worst = pmin(Expected, Centre, Side_opt, Side_nopt),
         ymin_Worst = min(Worst),
         ymax_Worst = max(Worst),
         ymin_Best = min(Best),
         ymax_Best = max(Best)) 

plt_lines_region <- df_regions %>% 
  ungroup() %>% 
  mutate(#bias_type = ifelse(bias_type == "biased", "Biased", "Symmetric"),
         sep_factor = as.numeric(sep_factor)/as.numeric(max(sep_factor))) %>%
  ggplot(aes(sep_factor, Expected)) + 
  geom_ribbon(aes(ymin = ymin_Worst,
                  ymax = ymax_Worst,
                  fill = "red"),
              alpha = .3) + 
  geom_ribbon(aes(ymin = ymin_Best,
                  ymax = ymax_Best,
                  fill = "green"),
              alpha = .3) +
  geom_line(aes(group = participant)) +
  scale_y_continuous("Expected Accuracy") +
  scale_x_continuous("Normalised Delta") +
  # see::scale_color_pizza_d() +
  # see::scale_fill_pizza() +
  facet_wrap(~bias_type) + 
  guides(fill = F) +
  theme_bw()
plt_lines_region


# # save 
# ggsave("../../Figures/Experiment_4_Prob/region_performance.png",
#        width = 5.6,
#        heigh = 3.5)
