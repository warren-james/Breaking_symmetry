#### Making figures ####

#### Library ####
library(tidyverse)

#### Load in data ####
load("scratch/df_part2_norm")

# preprocessing 
# sort out some labels for distances
# filter out trials outside of the range (0.8% of trials)
norm_dat %>% 
  group_by(participant) %>% 
  mutate(temp = as.numeric(as.factor(hoop_pos)),
         slab_measures = factor(temp, labels = c("~90%", "~50% - 1", "~50%", "~50% + 1", "~50% + 2", "~10%"))) %>% 
  select(-temp) %>%
  ungroup() %>% 
  filter(abs(norm_dist) <= 1) -> norm_dat

# check how much is removed by values greater than 1 
norm_dat %>% 
  mutate(outside = ifelse(abs(norm_dist) > 1, 1, 0)) %>% 
  group_by(outside) %>% 
  summarise(n = n()/length(norm_dat$trial))

#### Plotting ####
#### box plots ####
# try making distance a factor so it's the same across participants 
# kind of a nice plot... 
plt_dst <- norm_dat %>% 
  # group_by(slab_measures, participant) %>%
  group_by(slab_measures) %>% 
  mutate(mean_dist = mean(norm_dist)) %>%
  ggplot(aes(norm_dist)) + 
  # draw box for inside hoop region 
  # geom_rect(mapping = aes(xmin = -1,
  #                         xmax = 1,
  #                         ymin = 0, 
  #                         ymax = 20),
  #           fill = "grey",
  #           alpha = .1) +
  # geom_density(aes(fill = slab_measures,
  #                  colour = slab_measures),
  #              alpha = 0.3) + 
  geom_histogram(aes(fill = slab_measures,
                     colour = slab_measures),
                 alpha = 0.3,
                 binwidth = .15) +
  geom_vline(aes(xintercept = mean_dist),
             linetype = "dashed",
             alpha = 0.3) +
  theme_bw() + 
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("Big Hoop", "Centre", "Small Hoop")) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  #facet_wrap(~slab_measures, scales = "free") + 
  facet_wrap(~slab_measures) +
  see::scale_color_flat() + 
  see::scale_fill_flat()
plt_dst

# new plot, looks kind of cool
# mean is a solid line, median is dashed
plt_dst2 <- norm_dat %>% 
  ggplot(aes(x = norm_dist, 
             y = slab_measures,
             colour = slab_measures,
             fill = slab_measures)) + 
  # ggridges::stat_density_ridges(quantile_fun = mean,
  #                               quantile_lines = T,
  #                               alpha = .15,
  #                               linetype = "dashed") + 
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

# try boxplot versions
plt_box <- norm_dat %>% 
  ggplot(aes(y = norm_dist,
             x = slab_measures)) + 
  geom_boxplot(aes(fill = slab_measures,
                   colour = slab_measures),
               alpha = .3) + 
  theme_bw() + 
  see::scale_color_flat() + 
  see::scale_fill_flat() 
  # see::theme_abyss() +
  # see::scale_color_pizza() + 
  # see::scale_fill_pizza()
plt_box

