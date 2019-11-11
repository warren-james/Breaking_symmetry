# library 
library(tidyverse)
library(psyphy)

# work out opt values 
load("scratch/new_data/acc_sep")
load("scratch/new_data/df_part2")
load("scratch/new_data/AccMea")

# try working out new version of acc_sep with a set value of 100% for the closest separation and see what changes... 
load("scratch/new_data/Part_1_data_nar")
# to be added 
df_close <- tibble(participant = rep(unique(df$participant), each = 12),
                   separation = rep(1, length(participant)),
                   accuracy = rep(1, length(participant)))

df_test <- df %>%
  as_tibble() %>% 
  select(participant, separation, accuracy) %>% 
  bind_rows(df_close)

df_summ <- df %>% 
  group_by(participant, separation) %>% 
  summarise(acc = mean(accuracy))

# run model 
m_extra <- glm(accuracy ~ separation:participant, 
               data = df_test, 
               family = binomial(mafc.probit(10)))
m_orig <- glm(accuracy ~ separation:participant, 
              data = df, 
              family = binomial(mafc.logit(10)))

# get df for diff between model estimates 
seps <- seq(1, 500, 1)
df_est <- tibble(participant = rep(unique(df$participant), each = length(seps)),
                 separation = rep(seps, length(unique(participant))),
                 p = as.numeric(predict(m_orig,
                                        data.frame(separation = separation,
                                                   participant = participant),
                                        type = "response")),
                 type = "Original") %>% 
  bind_rows(tibble(participant = rep(unique(df$participant), each = length(seps)),
                   separation = rep(seps, length(unique(participant))),
                   p = as.numeric(predict(m_extra,
                                          data.frame(separation = separation,
                                                     participant = participant),
                                          type = "response")),
                   type = "Extra")) %>% 
  spread(type, p) %>% 
  mutate(diff = Extra - Original) %>% 
  # creates a plot of the difference,
  # empty shaded region shows the size of the difference 
  # nothing showing means they were the same
  ggplot(aes(separation, Extra)) + 
  geom_ribbon(aes(ymin = Extra,
                  ymax = Original)) + 
  facet_wrap(~participant)
  # # Makes a line plot of the data
  # ggplot(aes(Extra, Original)) +
  # geom_point(size = .1) +
  # geom_abline(intercept = 0,
  #             slope = 1) +
  # facet_wrap(~participant)
  # # plots the difference
  # ggplot(aes(separation, diff)) + 
  # geom_line() + 
  # facet_wrap(~participant)
df_est 

cor.test(df_est[["data"]]$Original, df_est[["data"]]$Extra)

# make quick plt 
df_test %>% 
  group_by(participant, separation) %>% 
  summarise(acc = mean(accuracy)) %>% 
  ggplot(aes(separation, acc)) + 
  geom_point() + 
  geom_smooth(method = "glm",
              method.args = list(family = binomial(mafc.logit(10))),
              se = F) + 
  geom_smooth(data = df_summ,
              aes(separation, acc),
              method = "glm", 
              method.args = list(family = binomial(mafc.logit(10))),
              se = F,
              colour = "red") +
  facet_wrap(~participant)

df %>% 
  group_by(participant, separation) %>% 
  summarise(acc = mean(accuracy)) %>% 
  ggplot(aes(separation, acc)) + 
  geom_point() + 
  geom_smooth(method = "glm",
              method.args = list(family = binomial(mafc.logit(10))),
              se = F,
              fullrange = T) + 
  scale_x_continuous(limits = c(1, 460)) + 
  facet_wrap(~participant)

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
         Side_opt = (fix_acc * pmax(bias_left, ll_bias)) + (far_acc * (pmin(bias_left, ll_bias))),
         Side_nopt = (fix_acc * pmin(bias_left, ll_bias)) + (far_acc * (pmax(bias_left, ll_bias))),
         Optimal = pmax(Side_opt, Centre))%>% 
  select(participant,
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

# overall plot
plt_check <- new_acc_measures %>% 
  group_by(participant, bias_type, separation) %>% 
  summarise(Actual = mean(accuracy),
            Centre = mean(Centre),
            Side = mean(Side_opt),
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
  mutate(dist_type = ifelse(Centre > Side_opt, "close", "far")) %>% 
  group_by(participant, bias_type, dist_type) %>% 
  summarise(Actual = mean(accuracy),
            Centre = mean(Centre),
            Side = mean(Side_opt),
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

# use this one, need to rescale it so 0 is the minimum accuracy and 1 is maximum
test <- new_acc_measures %>% 
  # filter(separation != 640) %>% 
  mutate(max_acc = pmax(Centre, Side_opt, Side_nopt),
         min_acc = pmin(Centre, Side_opt, Side_nopt)) %>% 
  group_by(participant, separation, bias_type) %>% 
  summarise(Expected = mean(Expected),
            Centre = mean(Centre),
            Side_opt = mean(Side_opt),
            Side_nopt = mean(Side_nopt),
            min = mean(min_acc),
            max = mean(max_acc)) %>% 
  ggplot(aes(separation, Expected,
             # colour = bias_type,
             fill = bias_type)) +
  # geom_ribbon(aes(ymin = Centre, 
  #             ymax = Side_nopt),
  #             alpha = .3) + 
  geom_ribbon(aes(ymin = Centre, 
                  ymax = Side_opt),
              alpha = .3) +  
  # geom_line(colour = "black") + 
  geom_line(aes(colour = bias_type)) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  scale_y_continuous("Accuracy") +
  facet_wrap(~participant) 
  # facet_grid(bias_type ~ participant)
test

#### recreate fixation prop plots with more accurate switch point ####
# first try without dist_type since that's confusing 
df_part2 %>% 
  group_by(participant, bias_type, standard_boxes, lcr) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  complete(lcr,
           nesting(participant, bias_type, standard_boxes),
           fill = list(n = 0)) %>% 
  ungroup() %>%
  group_by(participant, bias_type) %>%
  spread(lcr, n) %>% 
  mutate(most = ifelse(sum(`-1`) > sum(`1`), -1, 1),
         least = ifelse(sum(`-1`) < sum(`1`), -1, 1),
         # need to make sure that the biased group does not change 
         # will need to do this in the main df 
         lcr2 = ifelse(standard_boxes == "centre", 0,
                       ifelse(standard_boxes == "most likely", most, least)),
         n = ifelse(lcr2 == 0, max(`0`),
                    ifelse(lcr2 == 1, max(`1`), max(`-1`)))) %>% 
  select(participant, bias_type, standard_boxes, lcr2, most, least) %>% 
  merge(df_part2) %>% 
  mutate(st_box = ifelse(bias_type == "Biased", standard_boxes,
                         ifelse(lcr == 0, "centre",
                                ifelse(lcr == most, "most likely", "least likely")))) %>% 
  ungroup() %>% 
  group_by(participant, bias_type) %>%
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
  ggplot(aes(bias_type, proportion,
             fill = bias_type,
             colour = bias_type)) +
  geom_boxplot(alpha = .3) + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom") +
  scale_y_continuous(element_blank(), 
                     breaks = seq(0,1,.2), 
                     labels = scales::percent_format(accuracy = 1)) +
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  facet_wrap(~prop_type)

# labels should reflect participants' own biases in the "symmetric" condition 
# get dist type as well 
new_df <- new_acc_measures %>%
  mutate(dist_type = ifelse(Centre > Side_opt, "close", "far")) %>% 
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



# sactter 
new_acc_measures %>%
  group_by(participant, separation, bias_type) %>% 
  summarise(Expected = mean(Expected),
            Optimal = mean(Optimal)) %>% 
  ggplot(aes(Expected, Optimal, colour = participant)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  facet_wrap(~bias_type) +
  theme(legend.position = "none")

# same again but for overall accuracy
new_acc_measures %>% 
  group_by(participant, bias_type) %>% 
  mutate(side_fix = ifelse(lcr != 0, 1, 0)) %>%
  summarise(Actual = mean(accuracy),
            Expected = mean(Expected), 
            Optimal = mean(Optimal),
            side_prop = sum(side_fix)/n()) %>% 
  ggplot(aes(Optimal, Expected,
             colour = side_prop)) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point() + 
  facet_wrap(~bias_type) + 
  geom_text(aes(label = participant),
            nudge_x = -.004,
            nudge_y = -.004,
            angle = 45)

# above looks nice, but maybe we want to look at how far they are from the line? 

# with separation as a factor 
new_acc_measures %>% 
  group_by(participant, bias_type) %>% 
  mutate(side_fix = ifelse(lcr != 0, 1, 0),
         sep_factor = as.numeric(as.factor(separation))) %>%
  ungroup() %>% 
  group_by(participant, bias_type, sep_factor) %>% 
  summarise(Actual = mean(accuracy),
            Expected = mean(Expected), 
            Optimal = mean(Optimal),
            side_prop = sum(side_fix)/n()) %>% 
  # plot change over separation 
  ggplot(aes(sep_factor, Expected, 
             size = side_prop,
             colour = bias_type)) + 
             # colour = side_prop,
             # shape = bias_type)) + 
  geom_point(alpha = .6) +
  facet_wrap(~participant) + 
  see::scale_color_flat()
  # # plot for each separation
  # ggplot(aes(Optimal, Expected,
  #            colour = side_prop)) + 
  # geom_point() + 
  # # geom_text(aes(label = participant),
  # #           nudge_x = .004, 
  # #           nudge_y = .004,
  # #           angle = 45) + 
  # geom_abline(intercept = 0, slope = 1) + 
  # facet_grid(bias_type ~ sep_factor)

# box plots again 
new_acc_measures %>%
  group_by(participant, bias_type) %>% 
  summarise(Expected = mean(Expected),
            Optimal = mean(Optimal)) %>% 
  mutate(diff_edo = Expected/Optimal) %>% 
  ggplot(aes(bias_type, diff_edo,
             fill = bias_type)) + 
  geom_boxplot()
  
# recreate line plot from paper at the moment 
new_acc_measures %>% 
  group_by(participant, bias_type, separation) %>% 
  summarise(Expected = mean(Expected),
            Optimal = mean(Optimal),
            Actual = mean(accuracy)) %>% 
  mutate(diff_edo = Expected/Optimal) %>% 
  ggplot(aes(separation, diff_edo, 
             colour = bias_type)) + 
  geom_line(aes(group = interaction(bias_type, participant))) + 
  see::scale_color_flat() + 
  facet_wrap(~bias_type)

