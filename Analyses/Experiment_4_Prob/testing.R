# library 
library(tidyverse)
library(psyphy)

# work out opt values 
load("scratch/new_data/acc_sep")
load("scratch/new_data/df_part2")
load("scratch/new_data/AccMea")

#### revisiting at part 1 ####
# no need to do this, but at least we know now 
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

#### pre process the data ####
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

# do some checks for each participant 
checking <- acc_all %>% 
  group_by(participant, bias_type) %>% 
  mutate(n = n()) %>% 
  summarise(prop = sum(opt_side)/unique(n))

#### plot amount of time participants should have fixated the side ####
# plot to have a look 
checking %>% 
  ggplot(aes(participant, prop,
             fill = bias_type)) + 
  geom_bar(stat = "identity",
           position = "dodge") + 
  see::scale_fill_flat() + 
  scale_y_continuous("Proportion of Distances participant should fixate the side box")
  

#### plot difference between the strategies ####
# plot diff in strat over distance 
acc_all %>% 
  mutate(diff = Side - Centre) %>% 
  ggplot(aes(separation, diff, colour = bias_type)) + 
  geom_line() + 
  geom_ribbon(aes(ymax = diff, ymin = 0,
                  fill = bias_type),
              alpha = .3) +
  facet_wrap(~participant) + 
  see::scale_color_flat() +
  see::scale_fill_flat() +
  theme_bw() +
  # geom_hline(yintercept = 0,
  #            linetype = "dashed",
  #            colour = "black") + 
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

#### plot each participant and their expected acc against the centre strat ####
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
  theme_bw() +
  facet_wrap(~participant) +
  # facet_wrap(participant ~ bias_type) + 
  # theme(strip.text.x = element_blank()) +
  geom_line(aes(separation, diff_CO, colour = bias_type),
            linetype = "dashed") + 
  scale_y_continuous("Expected/Optimal") + 
  scale_x_continuous("Delta (pixels)")
plt_check

#### Plot region of worst performance ####
# region of worst performance and expected accuracy 
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
  # see::scale_color_pizza_d() +
  # see::scale_fill_pizza() +
  facet_wrap(~bias_type) + 
  guides(fill = F) +
  theme_bw()
plt_lines_region

# gather the best and worst regions
plt_lines_region.2 <- df_regions %>% 
  select(-Best, -Worst) %>%
  gather(c(ymin_Worst:ymax_Best),
         key = "test",
         value = "val") %>% 
  separate(test, 
           into = c("y_type", "BW"),
           sep = "_") %>% 
  spread(y_type, val) %>% 
  ggplot(aes(sep_factor, Expected)) + 
  geom_line(aes(group = participant)) + 
  geom_ribbon(aes(ymin = ymin, 
                  ymax = ymax,
                  fill = BW),
              alpha = .1) + 
  facet_wrap(~bias_type) +
  scale_fill_brewer() +
  theme_bw()
  
  
#### similar to above, but comparing side vs centre strats ####
# plot side vs. centre 
plt_sc <- plt_check[["data"]] %>% 
  gather(c(Centre, Side),
         key = "strat",
         value = "acc") %>% 
  ggplot(aes(separation, acc, 
             colour = strat)) + 
  geom_line(aes(linetype = bias_type)) + 
  facet_wrap(~participant)# + 
  #guides(linetype = F)
plt_sc

#### overal box plots for accuracy in the "close" and "far" distances ####
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

#### line plots with shaded area for region of accuracy for side and centre strats ####
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
# relabel data for appropriate most/least likely box fixations 
df_fixed_boxtype <- df_part2 %>% 
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
                                ifelse(lcr == most, "most likely", "least likely"))),
         
         bias_type = ifelse(bias_type == "biased", "Biased", "Symmetric")) %>% 
  ungroup() %>% 
  mutate(centre = ifelse(st_box == "centre", 1, 0),
         ML = ifelse(st_box == "most likely", 1, 0),
         LL = ifelse(st_box == "least likely", 1, 0))

# first try without dist_type since that's confusing 
plt_fix_box <- df_fixed_boxtype %>%
  group_by(participant, bias_type) %>% 
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
  see::scale_color_flat(name = "Condition") + 
  see::scale_fill_flat(name = "Condition") +
  facet_wrap(~prop_type)
plt_fix_box

# # save 
# ggsave("../../Figures/Experiment_4_Prob/fixprop_minus_dist_type.png",
#        width = 5.6,
#        heigh = 3.5)

# histogram version?
plt_fix_box[["data"]] %>%
  group_by(prop_type, bias_type) %>%
  mutate(med_prop = median(proportion)) %>%
  ggplot(aes(proportion,
             fill = bias_type,
             colour = bias_type)) + 
  geom_histogram(#aes(y = ..density..),
                 position = "dodge",
                 alpha = .3,
                 binwidth = .05) + 
  # geom_vline(aes(xintercept = med_prop),
  #            linetype = "dashed") + 
  geom_density(alpha = .1) +
  facet_wrap(~prop_type) + 
  theme_bw() + 
  scale_x_continuous("Proporition of fixations", 
                     breaks = seq(0,1,.2)) +
  see::scale_color_flat(name = "Condition") + 
  see::scale_fill_flat(name = "Condition")

# get dist type as well 
# this doesn't work just yet 
# duplicates data too much data... 
# TODO need a data frame labelling each box in a column (I know what that means...)
# temp_df <- new_acc_measures %>% 
#   group_by(participant, standard_boxes, lcr) %>%
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   complete(lcr, 
#            nesting(participant)) %>% 
#   mutate(standard_boxes = ifelse(lcr == 0, "centre", standard_boxes)) %>% 
#   select(-n)
# 
# # Still doesn't work... 
# new_df <- new_acc_measures %>%
#   mutate(dist_type = ifelse(Centre > Side_opt, "close", "far")) %>% 
#   select(participant, bias_type, lcr, standard_boxes, dist_type) %>%
#   group_by(participant, bias_type, lcr, dist_type) %>%
#   summarise(n = n()) %>% 
#   ungroup() %>%
#   complete(dist_type, 
#            nesting(participant, bias_type)) %>% 
#   complete(lcr, 
#            nesting(participant, bias_type, dist_type),
#            fill = list(n = 0)) %>% 
#   merge(temp_df) %>%
#   group_by(participant, bias_type, dist_type) %>%
#   spread(lcr, n) %>% 
#   replace(is.na(.), 0) %>% 
#   mutate(most = ifelse(sum(`-1`) > sum(`1`), -1, 1),
#          least = ifelse(sum(`-1`) < sum(`1`), -1, 1),
#          # need to make sure that the biased group does not change 
#          # will need to do this in the main df 
#          lcr2 = ifelse(standard_boxes == "centre", 0,
#                        ifelse(standard_boxes == "most likely", most, least)),
#          n = ifelse(lcr2 == 0, max(`0`),
#                     ifelse(lcr2 == 1, max(`1`), max(`-1`)))) %>% 
#   select(participant, bias_type, standard_boxes, lcr2, most, least, dist_type) %>% 
#   merge(new_acc_measures) %>% 
#   mutate(st_box = ifelse(bias_type == "Biased", standard_boxes,
#                          ifelse(lcr == 0, "centre",
#                                 ifelse(lcr == most, "most likely", "least likely")))) %>% 
#   distinct()


#### Try some scatter plots ####
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

# try doing a historgram plot? 
new_acc_measures %>% 
  group_by(participant, bias_type) %>% 
  mutate(side_fix = ifelse(lcr != 0, 1, 0)) %>%
  summarise(Actual = mean(accuracy),
            Expected = mean(Expected), 
            Optimal = mean(Optimal),
            side_prop = sum(side_fix)/n()) %>%
  mutate(diff_EdO = Expected/Optimal) %>%
  ggplot(aes(diff_EdO,
             fill = bias_type,
             colour = bias_type)) + 
  geom_histogram(alpha = .3, 
                 position = "dodge",
                 binwidth = .05,
                 aes(y = ..density..)) + 
  geom_density(alpha = .1)

# with separation as a factor 
plt_sep_factor <- new_acc_measures %>% 
  group_by(participant, bias_type) %>% 
  mutate(side_fix = ifelse(lcr != 0, 1, 0),
         sep_factor = as.numeric(as.factor(separation))) %>%
  ungroup() %>% 
  group_by(participant, bias_type, sep_factor) %>% 
  summarise(Actual = mean(accuracy),
            Expected = mean(Expected), 
            Optimal = mean(Optimal),
            side_prop = sum(side_fix)/n()) %>% 
  mutate(diff_EdO = Expected/Optimal) %>% 
  # plot change over separation 
  ggplot(aes(sep_factor,
             # Expected,
             diff_EdO, 
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
plt_sep_factor

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

#### Try stacking bars for each separation and by condition ####
# just do side centre for now 
# needs some work... 
plt_stacked_bars <- plt_sep_factor[["data"]] %>% 
  mutate(centre = 1- side_prop,
         side = side_prop) %>% 
  gather(c(centre, side),
         key = "prop_type",
         value = "proportion") %>%
  ggplot(aes(sep_factor, proportion,
             colour = prop_type,
             fill = prop_type)) + 
  geom_bar(stat = "identity",
           alpha = .3) + 
  facet_wrap(participant ~ bias_type) + 
  scale_x_continuous(breaks = seq(1,9,1)) + 
  theme_bw() + 
  theme(strip.text.x = element_blank())
plt_stacked_bars

# try again with box type added in
plt_stacked_bars_bt <- df_fixed_boxtype %>% 
  select(participant, bias_type, separation, st_box) %>%
  group_by(participant, bias_type) %>%
  mutate(sep_factor = as.numeric(as.factor(separation)),
         sep_factor = ifelse(separation == 640, 10, sep_factor)) %>% 
  ungroup() %>% 
  group_by(participant, bias_type, st_box, sep_factor) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  complete(sep_factor,
           nesting(participant, bias_type, st_box)) %>% 
  complete(st_box,
           nesting(participant, bias_type, sep_factor)) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         bias_box = paste(bias_type, st_box)) %>% 
  # ungroup() %>% 
  group_by(participant, bias_type, sep_factor) %>% 
  mutate(total_n = sum(n),
         prop = n/total_n,
         st_box = factor(st_box, c("least likely", "centre", "most likely"),
                         labels = c("Leasy Likely", "Centre", "Most Likely"))) %>% 
  ungroup() %>%
  mutate(participant = as.numeric(participant)) %>%
  ggplot(aes(sep_factor, 
             prop,
             colour = st_box,
             fill = st_box)) + 
  geom_bar(stat = "identity",
           alpha = .3) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,10),
                     labels = c("1","2","3","4","5","6","7","8","LP")) + 
  see::scale_fill_flat() + 
  see::scale_colour_flat() +
  scale_y_continuous(breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  # theme(strip.text.x = element_blank()) +
  # facet_wrap(participant ~ bias_type, ncol = 8)
  facet_grid(participant ~ bias_type)
plt_stacked_bars_bt

# particiapnt 12 only has 8 separations... 
# check this 
# df_part2 %>%
#   group_by(participant, separation) %>%
#   summarise(n = n()) %>%
#   ggplot(aes(separation, n)) +
#   geom_point() +
#   facet_wrap(~participant)

# looks 12 had 2 lots of the closest separation we hardcoded

#### same strat, different condition ####
# how would participant performance look if they had behaved the way they did 
# in the other condition... 
# hopefully that makes sense... Just use sym strat with bias condition and vice versa 

# first we want a dataset that's sensible 
df_swapstrat <- df_fixed_boxtype %>% 
  select(participant, bias_type, separation, bias_left,
         accuracy, st_box) %>% 
  mutate(ML_dist = ifelse(st_box == "centre", separation,
                          ifelse(st_box == "most likely", 1, separation * 2)),
         LL_dist = ifelse(ML_dist == separation, separation,
                          ifelse(ML_dist == 1, separation * 2, 1))) %>% 
  merge(acc_ll) %>% 
  merge(acc_ml) %>% 
  rowwise() %>% 
  mutate(maxchance_standard = ifelse(bias_type == "Biased", 0.8, 0.5),
         maxchance_swapped = ifelse(maxchance_standard == 0.8, 0.5, 0.8),
         biastype_standard = bias_type,
         biastype_swapped = ifelse(bias_type == "Biased", "Symmetric", "Biased"),
         Expected_standard = (ML_acc * maxchance_standard) + (LL_acc * (1-maxchance_standard)),
         Expected_swapped = (ML_acc * maxchance_swapped) + (LL_acc * (1-maxchance_swapped))) %>% 
  ungroup()

# a scatter plot for now 
df_swapstrat %>% 
  group_by(participant, bias_type) %>% 
  summarise(Actual = mean(accuracy),
            Expected_swapped = mean(Expected_swapped),
            Expected_standard = mean(Expected_standard)) %>% 
  ggplot(aes(Expected_standard, Expected_swapped,
             colour = bias_type)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)


# now how about some distributions? 
df_swapstrat %>% 
  group_by(participant, bias_type) %>% 
  summarise(Expected_swapped = mean(Expected_swapped),
            Expected_standard = mean(Expected_standard)) %>%
  gather(c(Expected_standard, Expected_swapped),
         key = "Expected_type",
         value = "Expected_acc") %>% 
  separate(Expected_type, 
           into = c("remove", "Expected_type"),
           sep = "_") %>%
  ggplot(aes(Expected_acc,
             colour = Expected_type,
             fill = Expected_type)) + 
  geom_density(alpha = .1) + 
  geom_histogram(aes(y = ..density..),
                 position = "dodge") + 
  facet_wrap(~bias_type) + 
  see::scale_color_flat() + 
  see::scale_fill_flat()
