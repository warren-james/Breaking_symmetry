#### Figures ####
# sort some figures for the two throw experiment 

#### Library ####
library(tidyverse)

#### load data ####
load("scratch/df_part2")

# remove trials where participants stood outside the range?
df_part2 %>%  
  mutate(outside = ifelse(abspos > 1, 1, 0)) %>% 
  group_by(outside) %>% 
  summarise(n = n()/length(df_part2$Num_throws))

# only lose 2.6% of trials... 

# pre processing 
df_part2 %>% 
  group_by(Participant) %>% 
  mutate(temp = as.numeric(as.factor(HoopDelta)),
         slab_measures = factor(temp, labels = c("~90%", "~50% - 1",
                                                 "~50%", "~50% + 1",
                                                 "~50% + 2", "~10%")),
         participant_num = as.numeric(as.factor(Participant))) %>% 
  select(-temp) %>%
  ungroup() %>% 
  filter(abspos <= 1) -> df_part2

#### PLOTS ####
# some histograms?
plt_hist <- df_part2 %>%
  ggplot(aes(x = abspos,
             colour = Num_throws,
             fill = Num_throws)) + 
  geom_histogram(position = "dodge",
                 binwidth = .1) + 
  # ggridges::stat_density_ridges(quantiles = 2, #quantile_fun = mean,
  #                               quantile_lines = T,
  #                               alpha = .15) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() +
  facet_wrap(~slab_measures)
plt_hist

df_part2 %>% 
  ggplot(aes(x = abspos,
             colour = Num_throws,
             fill = Num_throws)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~slab_measures) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() 

# try some boxplots
plt_box <- df_part2 %>% 
  ggplot(aes(slab_measures,
             abspos,
             colour = Num_throws,
             fill = Num_throws)) + 
  geom_boxplot(alpha = .3) +
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  scale_y_continuous("", 
                     breaks = c(0, 1),
                     labels = c("Centre","Side")) + 
  theme_bw() 
plt_box$labels$x = "Slab Measures"
plt_box$labels$y = "Absolute position"
plt_box$labels$colour = "No. Throws"
plt_box$labels$fill = "No. Throws"
plt_box




