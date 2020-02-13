#### Power for exp 1 ####
# This will use pilot data from my 4th year project 
# and data from Clarke and Hunt (2016) to look at 
# what the data may look like. 

# This script will resample from both data sets N 
# number of times to look at how the chance of finding 
# a difference is effected by N 

#### Library ####
library(tidyverse)
library(tidybayes)

#### Functions ####
# get acc over distance
get_acc <- function(df, deltas){
  m <- glm(data = df,
           Accuracy ~ norm_delta:Participant,
           family = binomial)
  
  df_pred_acc <- tibble(Participant = rep(unique(df$Participant),
                                          each = length(deltas)),
                        norm_delta = rep(deltas,
                                         length(unique(df$Participant)))) %>%
    mutate(p = predict(m, data.frame(norm_delta = norm_delta, Participant = Participant), type = "response"))
  return(df_pred_acc)
}

# get switch points 
get_SP <- function(df, acc_level){
  output_df <- tibble(Participant = character(),
                      SP = numeric())
  for(subj in unique(df$Participant)){
    temp <- df %>% 
      filter(Participant == subj)
    
    acc <- temp$p
    # Extract switch points 
    SP <- temp$norm_delta[which(abs(acc - acc_level) == min(abs(acc - acc_level)))]
    
    output_df <- rbind(output_df, tibble(Participant = subj,
                                         SP = SP))
  }
  return(output_df)
}

#### Load ####
#### Load: Part 1 ####
results_files <- c("data/4th_year_prob/Part1/")

df_part1_bias <- tibble()

# loop 
for(f in dir(results_files)){
  d <- read.csv(paste(results_files, f, sep = ""), header = F)
  d$participant <- strsplit(f, '[_.]')[[1]][2]
  d$condition <- "Bias"
  
  # bind 
  df_part1_bias <- rbind(df_part1_bias, d)
}

colnames(df_part1_bias) <- c("Block",
                             "Delta",
                             "Accuracy",
                             "Participant",
                             "Condition")
df_part1_bias <- df_part1_bias %>%
  drop_na()

results_files <- c("data/clarke_and_hunt/Part1/")
df_part1_orig <- tibble()

# loop 
for(f in dir(results_files)){
  d <- read.csv(paste(results_files, f, sep = ""), header = F)
  subjno <- strsplit(f, '[_.]')
  d$participant <- paste(subjno[[1]][1], subjno[[1]][2], sep = "_")
  d$condition <- "Symmetric"
  
  # bind 
  df_part1_orig <- rbind(df_part1_orig, d)
}

colnames(df_part1_orig) <- c("Block",
                             "Delta",
                             "Accuracy",
                             "Participant",
                             "Condition")

df_part1_orig <- df_part1_orig %>% 
  drop_na()

# tidy 
rm(d, results_files, f, subjno)

#### Load: Part 2 ####
results_files <- c("data/4th_year_prob/Part2/")

df_bias <- tibble()
for(f in dir(results_files)){
  d <- read.csv(paste(results_files, f, sep = ""), header = F)
  d$participant <- strsplit(f, '[_.]')[[1]][2]
  d$condition <- "Bias"
  # bind 
  df_bias <- rbind(df_bias, d)
}

# tidy 
rm(f,d)

colnames(df_bias) <- c("Block", 
                       "Delta",
                       "Fixated_box",
                       "Accuracy",
                       "Target_box",
                       "Participant", 
                       "Condition")

df_bias <- df_bias %>% 
  drop_na()

# need a separate loop for orig data since there are more columns... which sucks....
results_files <- c("data/clarke_and_hunt/Part2/")

df_orig <- tibble()

for(f in dir(results_files)){
  d <- read.csv(paste(results_files, f, sep = ""), header = F)
  subjno <- strsplit(f, '[_.]')
  d$participant <- paste(subjno[[1]][1], subjno[[1]][2], sep = "_")
  d$condition <- "Symmetric"
  # bind 
  df_orig <- rbind(df_orig, d)
}

# tidy 
rm(d, subjno, f, results_files)

# rename 
colnames(df_orig) <- c("Block", 
                       "Delta", 
                       "Fixated_box",
                       "Accuracy",
                       "Participant",
                       "Condition")

df_orig <- df_orig %>% 
  drop_na()

#### PRE-processing ####
# general
# rescale delta so all are divided by 450 
# helps us with fitting logistic regression 
df_part1_bias <- df_part1_bias %>% 
  mutate(norm_delta = round(Delta/450, digits = 2))
df_part1_orig <- df_part1_orig %>% 
  mutate(norm_delta = round(Delta/450, digits = 2))
df_bias <- df_bias %>% 
  mutate(norm_delta = round(Delta/450, digits = 2))
df_orig <- df_orig %>% 
  mutate(norm_delta = round(Delta/450, digits = 2))

#### PRE: part 1 ####
# get logistic regression results to setup switch points 
deltas <- seq(0,2,0.01)
df_bias_pred_acc <- get_acc(df_part1_bias, deltas)
df_orig_pred_acc <- get_acc(df_part1_orig, deltas)

# work out switch points for participants 
df_bias_SP <- get_SP(df_bias_pred_acc, .9)
df_orig_SP <- get_SP(df_orig_pred_acc, .75)

# need to work out the "most likely box" for both the orig data and bias
# probably need switch points as well since we want to know "close" and "far"

# sort out centre vs side accuracy to get optimal 
# this will assume they look at the most likely box 

# sort out bias first 
pred_bias_left <- df_bias_pred_acc %>% 
  mutate(p_l = p,
         left_dist = norm_delta) %>% 
  select(Participant, p_l, left_dist)
pred_bias_right <- df_bias_pred_acc %>% 
  mutate(p_r = p,
         right_dist = norm_delta) %>% 
  select(Participant, p_r, right_dist)

# merge it all 
df_bias <- df_bias %>% 
  mutate(left_dist = norm_delta * 2,
         right_dist = 0) %>% 
  merge(df_bias_pred_acc) %>% 
  merge(pred_bias_right) %>% 
  merge(pred_bias_left) %>% 
  mutate(Centre = p,
         Side = (p_r * .8) + (p_l * .2)) %>% 
  select(-Target_box,
         -p, - p_r, -p_l,
         -left_dist, 
         -right_dist) %>% 
  merge(df_bias_SP) %>% 
  mutate(Dist_type = ifelse(norm_delta < SP, "Close", "Far"))

# tidy 
rm(pred_bias_left, pred_bias_right)

# sort out orig 
pred_orig_left <- df_orig_pred_acc %>% 
  mutate(p_l = p,
         left_dist = norm_delta) %>% 
  select(Participant, p_l, left_dist)
pred_orig_right <- df_orig_pred_acc %>% 
  mutate(p_r = p,
         right_dist = norm_delta) %>% 
  select(Participant, p_r, right_dist)

# merge it all 
df_orig <- df_orig %>% 
  mutate(left_dist = norm_delta * 2,
         right_dist = 0) %>% 
  merge(df_orig_pred_acc) %>% 
  merge(pred_orig_right) %>% 
  merge(pred_orig_left) %>% 
  mutate(Centre = p,
         Side = (p_r * .5) + (p_l * .5)) %>% 
  select(-p, - p_r, -p_l,
         -left_dist, 
         -right_dist) %>% 
  merge(df_orig_SP) %>% 
  mutate(Dist_type = ifelse(norm_delta < SP, "Close", "Far"))

# tidy  
rm(pred_orig_left, pred_orig_right)

#### PRE: part 2 ####
# just look at the max proportion to one of the sides for the conditions? 
df_bias_fix <- df_bias %>% 
  select(Participant, Condition, Dist_type, Fixated_box) 
df_orig_fix <- df_orig %>% 
  select(Participant, Condition, Dist_type, Fixated_box) 
df_fix <- rbind(df_bias_fix, df_orig_fix) %>% 
  mutate(Fixated_box = as.factor(Fixated_box)) %>%
  group_by(Participant, Dist_type, Condition) %>% 
  mutate(n_total = as.factor(n())) %>% 
  ungroup() %>% 
  group_by(Participant, Condition, Dist_type, Fixated_box, n_total, .drop = F) %>%
  summarise(n_each = n()) %>% 
  complete(n_total, fill = list(n_total = 1)) %>%
  mutate(prop = n_each/as.numeric(n_total)) %>% 
  ungroup() %>% 
  mutate(Side = ifelse(Fixated_box == 1, 0, 1)) %>% 
  group_by(Participant, Dist_type, Side) %>%
  mutate(max_prop = max(prop),
         box_type = ifelse(Side == 0, "Centre", ifelse(prop == max_prop, "Most", "Least")))

# make a plot of this 
df_fix %>% 
  ggplot(aes(Dist_type, prop, 
             colour = Condition,
             fill = Condition)) + 
  geom_boxplot(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  facet_wrap(~box_type)

#### Setup data for resampling #### 
sample_bias <- df_bias %>% 
  select(Participant, Condition, Dist_type, Fixated_box) %>% 
  mutate(Fixated_box = as.factor(Fixated_box)) %>% 
  merge(df_fix)

sample_orig <- df_orig %>%
  select(Participant, Condition, Dist_type, Fixated_box) %>% 
  mutate(Fixated_box = as.factor(Fixated_box)) %>% 
  merge(df_fix)

df_resample <- rbind(sample_orig, sample_bias) %>% 
  mutate(Most = ifelse(box_type == "Most", 1, 0))

#### resampling ####
# quick plot of the real data to see what it's like 
df_resample %>% 
  group_by(Participant, Condition, Dist_type) %>% 
  summarise(prop = mean(Most)) %>% 
  ggplot(aes(prop, 
             colour = Condition,
             fill = Condition)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~Dist_type)


# now let's do some resampling... 
# setup some parameters 
# do we want to add in Dist_type?
n_trials <- 300 # seq(100, 450, 50)
n_sub <- seq(2, 20, 1)
n_iter <- 5000
refresh <- n_iter/100
conditions <- unique(df_resample$Condition)
disttype <- unique(df_resample$Dist_type)

# setup dataframe
# df_sample <- tibble(iter = numeric(),
#                     Condition = character(),
#                     n_sub = numeric(),
#                     n_trials = numeric(),
#                     Most = numeric())
# 
# # run loop
# for(ii in 1:n_iter){
#   if(ii %% refresh == 0){
#     print(paste((ii/n_iter)*100, "%", sep = ""))
#   }
#   # loop conditions
#   for(cond in conditions){
#     ss_cond <- df_resample %>%
#       filter(Condition == cond)
#     
#     # loop n trials
#     for(trials in n_trials){
#       # loop n subjects
#       for(sub in n_sub){
#         # get random participants
#         subj <- sample(ss_cond$Participant, sub, replace = T)
#         
#         # get data
#         ss_sub <- ss_cond %>%
#           filter(Participant %in% subj)
#         
#         # set this for a stopping point
#         # final_sub <- tail(unique(ss_sub$Participant), n = 1)
#         
#         # initialise vector
#         samp <- c()
#         
#         # loop through participants
#         count <- 0
#         for(unique_sub in subj){
#           count <- count + 1
#           # who are we sampling from?
#           sampling <- ss_sub %>%
#             filter(Participant == unique_sub)
#           
#           # sample random trials
#           samp <- c(samp, sample(sampling$Most, trials, replace = T))
#           
#           # check if we're adding this to the data frame
#           if(count == length(subj)){
#             df_sample <- rbind(df_sample, tibble(iter = ii,
#                                                  Condition = cond,
#                                                  n_sub = sub,
#                                                  n_trials = trials,
#                                                  Most = mean(samp)))
#           }
#         }
#       }
#     }
#   }
# }
# 

# save a version since this takes ages to run... 
# save(df_sample, file = "scratch/df_sample")
load("scratch/df_sample")

df_sample_compare <- df_sample %>% 
  spread(Condition, Most) %>% 
  group_by(n_sub) %>%
  mutate(diff = Bias - Symmetric,
         above0 = ifelse(diff > 0, 1, 0),
         HDI_lower = hdi(diff)[1],
         HDI_upper = hdi(diff)[2],
         mu_diff = mean(diff)) %>% 
  drop_na() %>% 
  ungroup()

# do a qqplot of this 
qqplot(df_sample_compare$Bias, df_sample_compare$Symmetric)

# plot the diff values 
df_sample_compare %>% 
  mutate(n_sub = as.factor(n_sub)) %>%
  ggplot(aes(diff,
             colour = n_sub,
             fill = n_sub)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~n_sub)

# plot estimate change with n_sub 
df_sample_compare %>% 
  ggplot(aes(n_sub, mu_diff)) + 
  geom_line() +
  geom_ribbon(aes(ymax = HDI_upper,
                  ymin = HDI_lower),
              alpha = .3) + 
  theme_bw() +
  theme(axis.title = element_text(size = 8)) +
  scale_x_continuous("No. Participants") + 
  scale_y_continuous("Difference in fixations to more likely side (Bias-Symmetric)")

# save this 
ggsave("../../Figures/Experiment_4_Prob/power_plot.png",
       width = 5.6,
       height = 3.5)

# p diff to 0 
df_sample_compare %>% 
  group_by(n_sub) %>%
  summarise(mu_above_0 = mean(above0))

# plot this 
df_sample %>% 
  # group_by(Condition, iter, n_sub, n_trials) %>% 
  # summarise(prop = mean(Most)) %>% 
  ggplot(aes(Most, 
             fill = Condition,
             colour = Condition)) + 
  geom_density(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  facet_grid(n_sub)
# facet_grid(n_sub ~ n_trials)


#### Model original data #### 
source("extract_draws_function.R")
# try a bayesian regression model... 
model_data <- df_resample %>% 
  mutate(Ml_fix = Most, 
         bias_type = Condition, 
         dist_type = Dist_type,
         participant = Participant)


library(brms)
# m1 <- brm(Most ~ Condition + (1|Participant), 
#           data = df_resample, 
#           family = "bernoulli",
#           chains = 1, 
#           iter = 2000, 
#           warmup = 1000)

# add in dist type
# m2 <- brm(Ml_fix ~ (bias_type + dist_type)^2 + (bias_type + dist_type|Participant),
#           data = model_data,
#           family = "bernoulli",
#           chains = 1,
#           iter = 2000,
#           warmup = 1000)

# save
# save(m2, file = "scratch/model_output")
load("scratch/model_output")

# make some plots
draws <- draw_post(m2)$draws_df
draws <- draws %>% 
  mutate(prop = boot::inv.logit(estimate))

draws %>% 
  ggplot(aes(prop, 
             colour = Dist_type,
             fill = Dist_type)) + 
  geom_density(alpha = .3)

draws %>% 
  ggplot(aes(prop, 
             colour = Bias_type,
             fill = Bias_type)) + 
  geom_density(alpha = .3)

draws %>% 
  ggplot(aes(prop, 
             colour = Bias_type,
             fill = Bias_type)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~Dist_type)
