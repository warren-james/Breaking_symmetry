#### Making plots for the paper ####
# probability matching 

#### Library ####
library(tidyverse)

#### Load data ####
load("scratch/new_data/df_part2")
load("scratch/new_data/AccMea")

#### Processing ####
df_part2 <- df_part2 %>% 
  mutate(dist_type = ifelse(separation < Switching_point,
                            "Close", "Far")) %>% 
  select(participant,
         block,
         bias_type,
         bias_left,
         separation,
         dist_type,
         Switching_point,
         lcr,
         standard_boxes,
         accuracy)

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

# need to check these two... slightly different results
df_part2 %>% 
  group_by(participant, bias_type, dist_type) %>% 
  mutate(centre = ifelse(standard_boxes == "centre", 1, 0),
         ML = ifelse(standard_boxes == "most likely", 1, 0),
         LL = ifelse(standard_boxes == "least likely", 1, 0),
         n = n()) %>%
  summarise(centre = mean(centre),
            ML = mean(ML),
            LL = mean(LL)) %>% 
  mutate(total = centre + ML + LL) %>%
  gather(centre:LL,
         key = "prop_type",
         value = "proportion") %>%
  mutate(prop_type = factor(prop_type, c("centre", "ML", "LL"),
                            labels = c("Centre", "Most Likely", "Least Likely"))) %>%
  ggplot(aes(dist_type, proportion, fill = bias_type)) +
  geom_boxplot() +
  # ggplot(aes(proportion, fill = prop_type)) +
  # geom_density(alpha = .3) + 
  # geom_histogram(position = "dodge") +
  scale_y_continuous(element_blank(),
                     breaks = seq(0,1,.2),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~prop_type) +
  see::scale_fill_flat(name = "Condition") +
  scale_x_discrete("Delta") + 
  theme_bw()


df_part2 %>%
  group_by(participant, bias_type, dist_type) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(participant, bias_type, dist_type, n, standard_boxes) %>%
  summarise(x_n = n()) %>%
  mutate(proportion = x_n/n) %>%
  ggplot(aes(dist_type, proportion, fill = standard_boxes)) +
  geom_boxplot() +
  facet_wrap(~bias_type) +
  see::scale_fill_flat() +
  theme_bw()


# comparing this by condition
plt_cs_sp <- df_part2 %>%
  mutate(centre = ifelse(lcr == 0, 1, 0)) %>% 
  group_by(participant, bias_type, dist_type) %>% 
  summarise(centre = mean(centre)) %>% 
  spread(key = "bias_type",
         value = centre) %>% 
  mutate(diff = biased - random) %>% 
  ggplot(aes(dist_type, diff)) + 
  geom_boxplot()
  


##### Accuracy plots ####  
AccMea %>% 
  filter(separation != 640) %>%
  group_by(participant, condition, separation) %>% 
  spread(Pred_type, 
         Acc) %>% 
  gather(c(Actual, Optimal, Expected),
         key = "Acc_type",
         value = "Acc") %>% 
  ungroup() %>%
  mutate(separation = separation/switch_point) %>%
  ggplot(aes(separation, Acc, colour = Acc_type)) + 
  geom_path(aes(group = interaction(participant, Acc_type)),
            alpha = 1) + 
  facet_wrap(~condition, scales = "free") + 
  scale_colour_viridis_d()



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
  mutate(Diff = Expected/Optimal,
         separation = separation/switch_point) %>% 
  ggplot(aes(separation, Diff, colour = condition)) +
  geom_line(data = plt_region,
            aes(separation,
                mean_centre,
                group = condition,
                linetype = condition),
            colour = "green") +
  geom_hline(yintercept = 1, colour = "white") + 
  geom_vline(xintercept = 1, colour = "white") + 
  geom_path(aes(group = interaction(condition, participant)), 
            alpha = 1) + 
  see::scale_colour_flat() + 
  see::theme_modern() 
plt$labels$y <- "Accuracy ratio (Expected/Optimal)"
plt$labels$x <- "Distance relative to switch point"
plt$labels$colour <- "Condition"
plt
plt + facet_wrap(~participant)



# Opt - Exp
plt <- AccMea %>% 
  filter(separation != 640) %>% 
  group_by(participant, condition, separation) %>% 
  spread(Pred_type, 
         Acc) %>% 
  ungroup() %>% 
  mutate(Diff = Expected - Optimal,
         separation = separation/switch_point) %>% 
  ggplot(aes(separation, Diff, colour = condition)) + 
  geom_hline(yintercept = 0, colour = "white") + 
  geom_vline(xintercept = 1, colour = "white") + 
  geom_path(aes(group = interaction(condition, participant)), 
            alpha = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  see::scale_colour_flat() + 
  see::theme_blackboard()
plt$labels$y <- "Difference (Expected - Optimal)"
plt$labels$x <- "Distance relative to switch point"
plt



df_part2 %>% 
  group_by(participant, bias_type, dist_type) %>% 
  summarise(acc = mean(accuracy)) %>% 
  ggplot(aes(dist_type, acc, fill = bias_type)) + 
  geom_boxplot()

# difference of actual and optimal 
AccMea %>%  
  spread(Pred_type, 
         Acc) %>%
  mutate(dist_type = ifelse(separation < switch_point, "Close", "Far"),
         Diff_Opt_m_Acc = Optimal - Actual,
         Diff_Opt_m_Exp = Optimal - Expected) %>%
  group_by(participant, condition, dist_type) %>%
  summarise(Diff_Opt_m_Acc = mean(Diff_Opt_m_Acc)) %>% 
  ggplot(aes(dist_type, Diff_Opt_m_Acc, fill = condition)) + 
  geom_boxplot()
  
