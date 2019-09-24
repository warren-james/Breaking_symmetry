#### Figures ####
# sort some figures for the two throw experiment 

#### Library ####
library(tidyverse)

#### load data ####
load("scratch/df_part2")

# pre processing 
df_part2 %>% 
  group_by(Participant) %>% 
  mutate(temp = as.numeric(as.factor(HoopDelta)),
         slab_measures = factor(temp, labels = c("~90%", "~50% - 1", "~50%", "~50% + 1", "~50% + 2", "~10%")),
         participant_num = as.numeric(as.factor(Participant))) %>% 
  select(-temp) %>%
  ungroup() -> df_part2

#### PLOTS ####
# some density plots?
df_part2 %>%
  ggplot(aes(x = abspos,
             # y = slab_measures,
             colour = Num_throws,
             fill = Num_throws)) + 
  geom_histogram(position = "dodge") + 
  # ggridges::stat_density_ridges(quantiles = 2, #quantile_fun = mean,
  #                               quantile_lines = T,
  #                               alpha = .15) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() +
  facet_wrap(~slab_measures)

plt_dst2 <- norm_dat %>% 
  ggplot(aes(x = norm_dist, 
             y = slab_measures,
             colour = slab_measures,
             fill = slab_measures)) + 
  ggridges::stat_density_ridges(quantile_fun = mean,
                                quantile_lines = T,
                                alpha = .15,
                                linetype = "dashed") + 
  #jittered_points = T,position = "raincloud") + 
  ggridges::stat_density_ridges(quantile_lines = T,
                                quantiles = 2,
                                alpha = .15) +
  see::scale_color_pizza() +
  see::scale_fill_pizza() +
  see::theme_blackboard() +
  # see::scale_color_flat() + 
  # see::scale_fill_flat() + 
  # theme_bw() +
  scale_x_continuous("Normalised Distance", 
                     limits = c(-1, 1)) + 
  scale_y_discrete("")
plt_dst2$labels$colour <- "Hoop Delta"
plt_dst2$labels$fill <- "Hoop Delta"
plt_dst2



