#### Making plots for the paper ####
# probability matching 

#### Library ####
library(tidyverse)

#### functions ####

#### Load data ####
load("scratch/new_data/df_part2")
load("scratch/new_data/AccMea")

#### Processing ####
df_part2 <- df_part2 %>% 
  mutate(dist_type = ifelse(separation < Switching_point,
                            "Close", "Far"),
         bias_type = ifelse(bias_type == "biased", "Biased", "Symmetric")) %>% 
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

# save this 
ggsave("../../Figures/Experiment_4_Prob/boxes_prop_CML.png",
       width = 5.6,
       heigh = 3.5)

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


#### Fit beta dist to proportions ####
# might want to try fitting a binomial instead... might look nicer?
# and a better representation of the data 
data_fitting <- plt_fix[["data"]] %>% 
  ungroup() %>% 
  mutate(proportion = (proportion + 1e-5)*(1-1e-4)) %>% 
  group_by(bias_type, prop_type) %>% 
  summarise(ests = list(fitdistrplus::fitdist(proportion,
                                              "beta")$estimate)) %>% 
  unnest %>% 
  mutate(params = rep(c("a","b"), length(ests)/2)) %>% 
  spread(key = params, 
         value = ests)

x_vals <- seq(.01,.99,.01)
a <- data_fitting$a
b <- data_fitting$b
bias <- unique(data_fitting$bias_type)
prop <- unique(data_fitting$prop_type)

data_line <- tibble(bias_type = rep(bias, each = length(x_vals)*3),
                    prop_type = rep(rep(prop, each = length(x_vals)),2),
                    a = rep(a, each = length(x_vals)),
                    b = rep(b, each = length(x_vals)),
                    x = rep(x_vals, (length(bias)*length(prop))),
                    density = dbeta(x, a, b))

# make a plot 
plt_fix[["data"]] %>% 
  ggplot(aes(proportion,
             fill = bias_type,
             colour = bias_type)) + 
  geom_histogram(binwidth = .1,
                 alpha = .3,
                 aes(y = ..density..),
                 position = "dodge") + 
  geom_line(data = data_line, 
            aes(x, density)) + 
  facet_wrap(~prop_type)

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
         Diff_Opt_m_Exp = Optimal - Expected,
         Diff_Opt_d_Exp = Expected/Optimal) %>%
  group_by(participant, condition, dist_type) %>%
  summarise(Diff_Opt_m_Acc = mean(Diff_Opt_m_Acc),
            Diff_Opt_m_Exp = mean(Diff_Opt_m_Exp),
            Diff_Opt_d_Exp = mean(Diff_Opt_d_Exp)) %>% 
  ggplot(aes(dist_type, Diff_Opt_d_Exp,
             fill = condition,
             colour = condition)) + 
  geom_boxplot(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() + 
  scale_x_discrete("Distance Type") + 
  scale_y_continuous("Expected Accuracy / Optimal Accuracy")
  
