#### Script to extract Data ####
# 1st year PhD project
# Probability Matching in the detection task
# Difference in this version is that participants are aiming
# to detect 1 of 10 letters rather than whether a dot is
# up or down. 

# This second script is to look at performance in the second half
# of the experiment

#### NOTES ####
# Looks like maximising simply becomes the best strategy when there 
# is a large bias for one side over the other. So in the bias condition
# participants shouldn't look in the centre ever really...

#### load libraries ####
library(tidyverse)
# library(psyphy)

#### any functions ####
# To convert pixels to visual degrees
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Constants ####
Screen_dist <- 54.4
ppcm <- 1920/54

#### load data ####
# Accuracy over distance 
load("scratch/new_data/acc_sep")

# Change colnames so accuracy isn't repeated 
colnames(acc_sep) <- c("participant",
                       "separation",
                       "Pred_Acc")

# Need part 2 
load("scratch/new_data/df_part2")

# set one column for swith_point 
df_part2$switch_point <- ifelse(df_part2$bias_left == 0.5,
                                df_part2$Fifty_Fifty,
                                df_part2$Eighty_Twenty)

# trim df_part2 down to be just what is needed for now...
trimmed_bias <- select(df_part2[df_part2$bias_type == "biased",],
                       participant,
                       condition,
                       block,
                       trial, 
                       separation, 
                       bias_left, 
                       cross_side,
                       accuracy,
                       lcr,
                       # Only need this for actual accuracy calculation, but isn't strictly necessary
                       standard_boxes,
                       switch_point)

trimmed_sym <- select(df_part2[df_part2$bias_type == "symmetric",],
                      participant,
                      condition,
                      block,
                      trial, 
                      separation, 
                      bias_left, 
                      cross_side,
                      accuracy,
                      # Only need this for actual accuracy calculation, but isn't strictly necessary
                      lcr,
                      standard_boxes,
                      switch_point)

#### Sort data ####
# merge the data 
trimmed_bias <- merge(trimmed_bias, acc_sep)
trimmed_sym <- merge(trimmed_sym, acc_sep)


# add in min_acc for 50-50 and 80-20
trimmed_bias$min_acc <- 0.82
trimmed_sym$min_acc <- 0.55

# get left and right bias 
# Don't think this is needed
trimmed_bias$bias_right <- 1 - trimmed_bias$bias_left
trimmed_sym$bias_right <- 1 - trimmed_sym$bias_left


#### Getting standardised scores ####
#### BIAS ####
#### BIAS: EXPECTED ####
# Expected given startegy used
# Use this to select the side the option that's most likely... I think this should work 
# temp$bias_test <- temp$temp * pmax(temp$bias_left, temp$bias_right, na.rm = TRUE)
trimmed_bias$ML_dist <- trimmed_bias$separation
trimmed_bias$LL_dist <- trimmed_bias$separation

trimmed_bias$ML_dist[trimmed_bias$standard_boxes == "most likely"] <- 1
trimmed_bias$ML_dist[trimmed_bias$standard_boxes == "least likely"] <- 2*trimmed_bias$separation[trimmed_bias$standard_boxes == "least likely"]

trimmed_bias$LL_dist[trimmed_bias$standard_boxes == "least likely"] <- 1
trimmed_bias$LL_dist[trimmed_bias$standard_boxes == "most likely"] <- 2*trimmed_bias$separation[trimmed_bias$standard_boxes == "most likely"]

# sort out ML acc first 
acc_ML <- acc_sep 
colnames(acc_ML) <- c("participant",
                      "ML_dist",
                      "ML_acc")

# merge
dat_bias <- merge(trimmed_bias, acc_ML)

# same again for LL
acc_LL <- acc_sep 
colnames(acc_LL) <- c("participant",
                      "LL_dist",
                      "LL_acc")

# merge
dat_bias <- merge(dat_bias, acc_LL)

# tidy 
# rm(ML_acc, LL_acc)

# Make lower limit for accuracy for chance 
dat_bias$LL_acc[dat_bias$LL_acc < 0.11] <- 0.1
dat_bias$ML_acc[dat_bias$ML_acc < 0.11] <- 0.1

# Get expected acc 
dat_bias$Exp_Acc <- (dat_bias$LL_acc * pmin(dat_bias$bias_left,
                                            dat_bias$bias_right)) +
  (dat_bias$ML_acc * pmax(dat_bias$bias_left,
                          dat_bias$bias_right))

#### BIAS: OPTIMAL ####
# trim dataset to what we need
dat_bias <- select(dat_bias,
                   participant,
                   separation,
                   block,
                   accuracy,
                   lcr,
                   standard_boxes,
                   switch_point,
                   Pred_Acc,
                   min_acc,
                   bias_left,
                   Exp_Acc)

# Add in opt_fix 
dat_bias$opt_fix <- 0
dat_bias$opt_fix[dat_bias$separation > dat_bias$switch_point] <- 1

# Sort out distances and accuracy 
dat_bias$ML_dist <- dat_bias$separation
dat_bias$LL_dist <- dat_bias$separation

dat_bias$ML_dist[dat_bias$opt_fix == 1] <- 1
dat_bias$LL_dist[dat_bias$opt_fix == 1] <- 2*dat_bias$LL_dist[dat_bias$opt_fix == 1]

# merge this with the acc_sep stuff 
dat_bias <- merge(dat_bias, acc_ML)
dat_bias <- merge(dat_bias, acc_LL)

# set bottom to 10%
dat_bias$LL_acc[dat_bias$LL_acc < 0.11] <- 0.1
dat_bias$ML_acc[dat_bias$ML_dist == 1] <- 1

# Now get opt_acc
dat_bias$Opt_Acc <- (dat_bias$ML_acc*0.8)+(dat_bias$LL_acc*0.2)

# Keep only the needed data 
dat_bias <- select(dat_bias,
                   participant, 
                   separation,
                   block,
                   accuracy,
                   lcr,
                   standard_boxes,
                   switch_point,
                   Pred_Acc,
                   min_acc,
                   bias_left,
                   Exp_Acc,
                   Opt_Acc)


# Get temp measures of things 
temp <- dat_bias %>%
  group_by(participant, separation, bias_left, switch_point) %>%
  summarise(Actual = mean(accuracy),
            Centre = mean(Pred_Acc),
            Expected = mean(Exp_Acc),
            Optimal = mean(Opt_Acc))

# need to reshape this for plotting
AccMea_bias <- temp %>%
  gather(Pred_type, Acc, Centre:Optimal)

AccMea_bias$condition <- "Bias"

# tidy 
rm(temp)

# sort levels

# quick plot to check
plt <- ggplot(AccMea_bias, aes(get_VisDegs(separation/ppcm, Screen_dist),
                               Acc,
                               colour = Pred_type))
plt <- plt + geom_point()
plt <- plt + geom_line()
plt <- plt + theme_bw()
plt <- plt + ggtitle("Biased Condition")
plt <- plt + facet_wrap(~as.numeric(participant))
plt <- plt + scale_y_continuous(breaks = seq(0, 1, by = .2))
plt <- plt + theme(legend.position = "bottom")
plt$labels$x <- "Delta (Visual Degrees)"
plt$labels$y <- "Accuracy"
plt$labels$colour <- "Accuracy Type"
# plt$coordinates$limits$y <- c(0,1)
plt
# save
# ggsave("../../Figures/Experiment_4_prob/exp_vs_opt_bias.png",
#        height = 12,
#        width = 18,
#        units = "cm")

# tidy 
rm(plt)

#### temp plot to check accuracy across distance for each participant ####
# temp_plt <- ggplot(acc_ML, aes(ML_dist, ML_acc))
# temp_plt <- temp_plt + geom_line()
# temp_plt <- temp_plt + facet_wrap(~participant)
# temp_plt

#### symmetric ####
#### symmetric: EXPECTED ####
# get distances
trimmed_sym$LL_dist <- trimmed_sym$separation
trimmed_sym$ML_dist <- trimmed_sym$separation

trimmed_sym$LL_dist[trimmed_sym$lcr != 0] <- 2*trimmed_sym$separation[trimmed_sym$lcr != 0]
trimmed_sym$ML_dist[trimmed_sym$lcr != 0] <- 1

# create new data 
dat_sym <- merge(trimmed_sym, acc_ML)
dat_sym <- merge(dat_sym, acc_LL)

# set bottom to 10%
dat_sym$LL_acc[dat_sym$LL_acc < 0.11] <- 0.1
dat_sym$ML_acc[dat_sym$ML_acc < 0.11] <- 0.1
dat_sym$ML_acc[dat_sym$ML_dist == 1] <- 1

dat_sym$Exp_Acc <- (0.5*dat_sym$ML_acc) + (0.5*dat_sym$LL_acc)

# tidy dataset 
dat_sym <- select(dat_sym,
                  participant, 
                  separation,
                  condition,
                  block, 
                  trial,
                  bias_left,
                  cross_side,
                  accuracy,
                  lcr,
                  min_acc,
                  standard_boxes,
                  switch_point,
                  Pred_Acc,
                  Exp_Acc)

#### symmetric: OPTIMAL ####
# Get optimal Fixations
dat_sym$Opt_Fix <- 0
dat_sym$Opt_Fix[dat_sym$separation > dat_sym$switch_point] <- 1

dat_sym$ML_dist <- dat_sym$separation
dat_sym$LL_dist <- dat_sym$separation

dat_sym$ML_dist[dat_sym$Opt_Fix == 1] <- 1
dat_sym$LL_dist[dat_sym$Opt_Fix == 1] <- 2*dat_sym$LL_dist[dat_sym$Opt_Fix == 1]

# merge with acc data 
dat_sym <- merge(dat_sym, acc_ML)
dat_sym <- merge(dat_sym, acc_LL)

# set minvalue for chance
dat_sym$LL_acc[dat_sym$LL_acc < 0.11] <- 0.1
dat_sym$ML_acc[dat_sym$ML_acc < 0.11] <- 0.1
dat_sym$ML_acc[dat_sym$ML_dist == 1] <- 1

dat_sym$Opt_Acc <- (0.5*dat_sym$ML_acc) + (0.5*dat_sym$LL_acc)

# tidy dataset
dat_sym <- select(dat_sym,
                  participant, 
                  separation,
                  condition,
                  block, 
                  trial,
                  bias_left,
                  cross_side,
                  accuracy,
                  lcr,
                  min_acc,
                  standard_boxes,
                  switch_point,
                  Pred_Acc,
                  Exp_Acc,
                  Opt_Acc)
#set plot data
temp <- dat_sym %>%
  group_by(participant, separation, bias_left, switch_point) %>%
  summarise(Actual = mean(accuracy),
            Centre = mean(Pred_Acc),
            Expected = mean(Exp_Acc),
            Optimal = mean(Opt_Acc))

# need to reshape this for plotting
AccMea_sym <- temp %>%
  gather(Pred_type, Acc, Centre:Optimal)

AccMea_sym$condition <- "Symmetric"

# plot this
plt <- AccMea_sym %>% 
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             Acc,
             colour = Pred_type)) + 
  geom_point() + 
  geom_line() + ggtitle("symmetric Condition") +
  theme_bw() +
  facet_wrap(~as.numeric(participant)) +
  see::scale_color_flat() + 
  scale_y_continuous(breaks = seq(0, 1, by = .2)) +
  theme(legend.position = "bottom")
plt$labels$x <- "Delta (Visual Degrees)"
plt$labels$y <- "Accuracy"
plt$labels$colour <- "Accuracy Type"
# plt$coordinates$limits$y <- c(0,1)
plt
# save 
# ggsave("../../Figures/Experiment_4_prob/exp_vs_opt_sym.png",
#        height = 12,
#        width = 18,
#        units = "cm")
# 

# combine both and save 
AccMea <- rbind(AccMea_bias, AccMea_sym)
save(AccMea, file = "scratch/new_data/AccMea")

#### setup plot for paper ####
# lines plts for bias and sym comparison of optimal to 
# expected given actual 
plt_sym <- AccMea_sym %>%
  # filter(separation != 640) %>%
  ungroup() %>%
  mutate(participant = as.numeric(participant),
         separation = separation/switch_point) %>%
  # ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
  #            Acc,
  #            colour = Pred_type)) +
  ggplot(aes(separation, Acc, colour = Pred_type)) + 
  geom_line(aes(group = interaction(participant, Pred_type)),
            alpha = 1) +
  # geom_point() + 
  # geom_line() +
  # geom_smooth(method = glm,
  #             method.args = list(family = binomial(mafc.logit(10))),
  #             se = F, fullrange = T,
  #             linetype = "dashed") +
  see::scale_colour_flat() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() + 
  # scale_x_continuous(limits = c(0, 4)) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm"))) +
  ggtitle("Symmetrical Condition") +
  facet_wrap(~participant)
plt_sym$labels$x <- "Delta (Scaled by switch point)"
plt_sym$labels$y <- "Accuracy"
plt_sym$labels$colour <- "Line Type"
plt_sym

# save 
ggsave("../../Figures/Experiment_4_prob/exp_vs_opt_sym_lines.png",
       height = 12,
       width = 18,
       units = "cm")

plt_bias <- AccMea_bias %>%
  ungroup() %>%
  mutate(participant = as.numeric(participant)) %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             Acc,
             colour = Pred_type)) +
  geom_line() + 
  ggthemes::scale_colour_ptol() +
  theme_minimal() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm"))) +
  ggtitle("Bias Condition") +
  facet_wrap(~participant)
plt_bias$labels$x <- "Delta (Visual Degrees)"
plt_bias$labels$y <- "Accuracy"
plt_bias$labels$colour <- "Line Type"
plt_bias

# save
ggsave("../../Figures/Experiment_4_prob/exp_vs_opt_bias_lines.png",
       height = 12,
       width = 18,
       units = "cm")





#### Plot both in one? ####
# Doesn't work just yet unforunately 
All_Acc <- rbind(AccMea_bias, AccMea_sym)

plt_dat_sym <- All_Acc[All_Acc$condition == "symmetric" & All_Acc$Pred_type == "Optimal",]
plt_dat_Bias <- All_Acc[All_Acc$condition == "Bias" & All_Acc$Pred_type == "Optimal",]
plt_dat_Centre <- All_Acc[All_Acc$condition == "Bias" & All_Acc$Pred_type == "Centre",]

# make plot
plt <- ggplot(All_Acc, aes(get_VisDegs(separation/ppcm, Screen_dist),
                           Acc))#,
#fill = Pred_type))
plt <- plt + theme_bw()
plt <- plt + geom_area(data = plt_dat_Bias, aes(get_VisDegs(separation/ppcm, Screen_dist),
                                                Acc),
                       fill = "blue",
                       alpha = 0.2)
plt <- plt + geom_area(data = plt_dat_sym, aes(get_VisDegs(separation/ppcm, Screen_dist),
                                               Acc),
                       fill = "blue",
                       alpha = 0.2)
plt <- plt + geom_area(data = plt_dat_Centre, aes(get_VisDegs(separation/ppcm, Screen_dist),
                                                  Acc),
                       fill = "red",
                       alpha = 0.4)
plt <- plt + geom_line(data = All_Acc[All_Acc$Pred_type == "Expected",], 
                       aes(linetype = condition),
                       size = 1)
plt <- plt + theme(legend.position = "bottom")
# plt <- plt + geom_point()
# plt <- plt + geom_line(aes(linetype = condition))
plt <- plt + facet_wrap(~as.numeric(participant))
plt$labels$x <- "Delta (Visual Degrees"
plt$labels$y <- "Accuracy"
plt$labels$linetype <- "Expected Accuracy for:"
plt

# save 
ggsave("../../Figures/Experiment_4_prob/Estimated_Accuracy_both.png",
       height = 12,
       width = 18,
       units = "cm")

#### same as above but with lines ####
plt_sym <- All_Acc %>%
  filter(Pred_type != "Centre",
         condition == "symmetric") %>%
  ungroup() %>%
  mutate(participant = as.numeric(participant)) %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             Acc, colour = Pred_type)) + 
  geom_line() + 
  ggtitle("symmetric Condition") + 
  scale_colour_ptol() + 
  theme_bw() + 
  facet_wrap(~participant) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
plt_sym$labels$x <- "Delta (Visual Degrees)"
plt_sym$labels$y <- "Accuracy"
plt_sym$labels$colour <- "Line Type"
plt_sym

# save
ggsave("../../Figures/Experiment_4_prob/symmetric_Est_Accuracy.png",
       height = 12,
       width = 18,
       units = "cm")

# bias
plt_bias <- All_Acc %>%
  filter(Pred_type != "Centre",
         condition == "Bias") %>%
  ungroup() %>%
  mutate(participant = as.numeric(participant)) %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             Acc, colour = Pred_type)) + 
  geom_line() + 
  ggtitle("Bias Condition") + 
  scale_colour_ptol() + 
  theme_bw() + 
  facet_wrap(~participant) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
plt_bias$labels$x <- "Delta (Visual Degrees)"
plt_bias$labels$y <- "Accuracy"
plt_bias$labels$colour <- "Line Type"
plt_bias

# save
ggsave("../../Figures/Experiment_4_prob/biasom_Est_Accuracy.png",
       height = 12,
       width = 18,
       units = "cm")







