#### Two throw ####
# Maybe use something like the power for Justing and Neli? 
# we were expecting people to get better with this manipulation 
# but that's on expected accuracy...
# I suppose we could look at "placement" for close vs far? 
# So maybe that's a good idea? 

#### Library ####
library(tidyverse)
library(tidybayes)
#### Load in data #### 
# load in data from the power analysis 
# all we need is the placement/standing positions and then we can sample from there 
# I guess? 
load("data/df_Aberdeen_decisions")
load("data/df_part2_Throw")


# sort out data we need 
df_Av <- df_Aberdeen_decisions %>% 
  filter(truck_perf == "Variable") %>%
  # select(participant, 
  #        delta,
  #        placed_x) %>%
  mutate(condition = "Avatar") %>% 
  group_by(participant) %>% 
  mutate(standard_sep = as.numeric(as.factor(delta))) %>% 
  ungroup() %>% 
  mutate(norm_pos = abs(placed_x/delta)) %>% 
  select(participant, condition, standard_sep, norm_pos) %>% 
  filter(norm_pos <= 1)

df_Th <- df_part2_Throw %>% 
  mutate(condition = "Throwing",
         participant = Participant) %>% 
  group_by(participant) %>% 
  mutate(standard_sep = as.numeric(as.factor(HoopDelta))) %>% 
  ungroup() %>%
  mutate(norm_pos = abs(Subject_Position/HoopDelta)) %>%
  select(participant, condition, standard_sep, norm_pos) %>% 
  filter(norm_pos <= 1)

# combine? 
df_all <- rbind(df_Th, df_Av) %>% 
  mutate(standard_lab = factor(standard_sep, labels = c("90%", "~75%", "~25%", "10%")),
         participant = paste(participant, condition, sep = "_"))

#### Plots ####
# just a quick plot for the different groups 
df_all %>%
  filter(standard_sep %in% c("90%", "10%")) %>%
  ggplot(aes(norm_pos,
             colour = condition,
             fill = condition)) +
  geom_density(alpha = .3) + 
  facet_wrap(~standard_sep)

#### Resampling #### 
# now we can resmaple this data using
n_iter <- 5000
n_subs <- seq(2, 28, 2) 
n_trials <- c(12, 60)
dists <- c("90%", "10%")#c(1, 4)
refresh <- n_iter/100

# setup data 
# df_sample <- tibble(iter = numeric(),
#                     n_subs = numeric(),
#                     n_trials = numeric(),
#                     dist = character(),
#                     baseline = numeric(),
#                     comparison = numeric())


# make this a data.table instead 
n <- n_iter * length(n_subs) * length(n_trials) * length(dists)

# df_sample <- data.table::data.table(iter = rep(0, n),
#                                     n_subs = rep(0, n),
#                                     n_trials = rep(0, n),
#                                     dist_type = rep("", n),
#                                     baseline = rep(0, n),
#                                     comparison = rep(0, n))
# 
# # TODO make this loop quicker by using data.table()
# # split dataset for ease of access...
df_Av <- df_all %>%
  filter(condition == "Avatar")
df_Th <- df_all %>%
  filter(condition == "Throwing")
# 
# # start counter
# count <- 0
# # loop through
# for(ii in 1:n_iter){
#   if(ii %% refresh == 0){
#     print(paste((ii/n_iter)*100, "%", sep = ""))
#   }
#   # loop distances
#   for(dist in dists){
#     base_ss <- df_Th %>%
#       filter(standard_lab == dist)
#     comp_ss <- df_Av %>%
#       filter(standard_lab == dist)
#     for(trials in n_trials){
#       for(subs in n_subs){
#         # get random subjects
#         base_subs <- sample(df_Th$participant, subs, replace = T)
#         comp_subs <- sample(df_Av$participant, subs, replace = T)
# 
#         # loop through subjects
#         base_sample <- c()
#         comp_sample <- c()
#         for(sub in 1:subs){
#           base_temp <- base_ss %>%
#             filter(participant == base_subs[sub])
#           comp_temp <- comp_ss %>%
#             filter(participant == comp_subs[sub])
#           base_sample <- c(base_sample,
#                            sample(base_temp$norm_pos, trials, replace = T))
#           comp_sample <- c(comp_sample,
#                            sample(comp_temp$norm_pos, trials, replace = T))
#           if(sub == subs){
#             count <- count + 1
#             # bind to dataframe
#             # df_sample <- rbind(df_sample, tibble(iter = ii,
#             #                                      n_subs = subs,
#             #                                      n_trials = trials,
#             #                                      dist = dist,
#             #                                      baseline = mean(base_sample),
#             #                                      comparison = mean(comp_sample)))
#             # input into table
#             df_sample[count, iter := ii]
#             df_sample[count, n_subs := subs]
#             df_sample[count, n_trials := trials]
#             df_sample[count, dist_type := dist]
#             df_sample[count, baseline := mean(base_sample)]
#             df_sample[count, comparison := mean(comp_sample)]
#           }
#         }
#       }
#     }
#   }
# }

# might not need this...
# df_sample <- as_tibble(df_sample)
# save this 
# save(df_sample, file = "scratch/df_sample")
load("scratch/df_sample")

# plot to check 
df_sample %>%
  filter(n_subs > 20) %>%
  gather(baseline:comparison, 
         key = "condition",
         value = "value") %>% 
  ggplot(aes(value, 
             fill = condition,
             colour = condition)) + 
  geom_density(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  facet_grid(n_subs~dist_type)

# now do some HDI stuff...
df_sample %>% 
  filter(dist_type == "10%",
         n_trials == 60) %>%
  mutate(diff = baseline - comparison) %>%
  # gather(baseline:comparison,
  #        key = "condition", 
  #        value = "value") %>% 
  group_by(n_subs, n_trials, dist_type) %>% 
  summarise(mu = mean(diff),
            HDI_lower = hdi(diff)[1],
            HDI_upper = hdi(diff)[2]) %>% 
  ggplot(aes(n_subs, mu)) + 
  # colour = n_trials,
  # fill = n_trials)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = HDI_lower,
                  ymax = HDI_upper),
              alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  scale_y_continuous("Difference in mean position between 'Optimal' and Standard samples") +
  scale_x_continuous("Sample Size") +
  theme_bw() #+
  # facet_grid(dist_type~n_trials)

# save this 
ggsave("../../Figures/Experiment_3_Hoop_size/power_plot.png",
       width = 5.6,
       height = 3.5)





