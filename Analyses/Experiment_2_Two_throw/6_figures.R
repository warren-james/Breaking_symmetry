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
         participant_num = as.numeric(as.factor(Participant)),
         l_dist = abs(-HoopDelta - Position),
         r_dist = abs(HoopDelta - Position)) %>% 
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

# save 
ggsave(plt_box, file = "../../Figures/Experiment_2_Two_throw/box_plt.png",
       height = 3, 
       width = 5.6)

#### Expected Accuracy ####
# load part 1 data 
load("scratch/df_part1")

# change participant column to match df_part2 
df_part1 <- df_part1 %>% 
  rename(Participant = participant)

# get predictions for accuracy 
m = glm(data=df_part1, acc~slab:Participant, binomial)

# setting up dataframe
participants <- unique(df_part2$Participant)
slabs <- seq(0, max(df_part2$HoopDelta*2), 1)
acc_sep = tibble(Participant = rep(participants, each = length(slabs)),
                 slab = rep(slabs, length(participants)))
#tidy
rm(participants, slabs)
# add in prediction
acc_sep <- acc_sep %>% 
  group_by(Participant, slab) %>%
  mutate(p = as.numeric(predict(m, data.frame(Participant = Participant,
                                              slab = slab),
                                type = "response")[1])) %>% 
  ungroup() 

# sort accuracy 
acc_c_l <- acc_sep %>% 
  rename(c_l_dist = slab,
         c_l_acc = p)
acc_c_r <- acc_sep %>% 
  rename(c_r_dist = slab,
         c_r_acc = p)
acc_s_l <- acc_sep %>% 
  rename(s_l_dist = slab,
         s_l_acc = p)
acc_s_r <- acc_sep %>% 
  rename(s_r_dist = slab,
         s_r_acc = p)
acc_e_l <- acc_sep %>% 
  rename(l_dist = slab,
         l_acc = p)
acc_e_r <- acc_sep %>% 
  rename(r_dist = slab,
         r_acc = p)

# combine datasets 
df_acc <- df_part2 %>% 
  mutate(c_l_dist = HoopDelta,
         c_r_dist = HoopDelta,
         s_l_dist = 0,
         s_r_dist = HoopDelta*2) %>% 
  merge(acc_c_l) %>% 
  merge(acc_c_r) %>% 
  merge(acc_s_l) %>% 
  merge(acc_s_r) %>% 
  merge(acc_e_l) %>% 
  merge(acc_e_r) %>% 
  rowwise() %>%
  mutate(Expected = mean(c(l_acc, r_acc)),
         Centre = mean(c(c_l_acc, c_r_acc)),
         Side = mean(c(s_l_acc, s_r_acc))) %>% 
  select(Participant, Num_throws, Position, HoopDelta, switchSlab,
         slab_measures, Expected, Centre, Side) %>% 
  ungroup()

# plot this 
df_acc %>% 
  group_by(Participant, slab_measures, Num_throws) %>% 
  summarise(Expected = mean(Expected),
            Centre = mean(Centre),
            Side = mean(Side)) %>%
  ungroup() %>% 
  group_by(slab_measures) %>%
  mutate(sep_factor = as.numeric(slab_measures),
         Best = pmax(Expected, Centre, Side),
         Worst = pmin(Expected, Centre, Side),
         ymin_Best = min(Best)-.01,
         ymax_Best = max(Best),
         ymin_Worst = min(Worst),
         ymax_Worst = max(Worst)) %>%
  ggplot(aes(sep_factor, Expected)) + 
  geom_ribbon(aes(x = sep_factor,
                  ymin = ymin_Worst,
                  ymax = ymax_Worst, 
                  fill = "red"),
              alpha = .35) + 
  geom_ribbon(aes(x = sep_factor,
                  ymin = ymin_Best,
                  ymax = ymax_Best,
                  fill = "green"),
              alpha = .35) +
  scale_y_continuous("Expected Accuracy", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous("Slab Measures", breaks = c(1:6),
                     labels = c("~90%", "~50% - 1", "~50%", "~50% + 1", "~50% + 2", "~10%")) +
  # ggsci::scale_color_lancet() + 
  # ggsci::scale_fill_lancet() +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  geom_line(aes(group = interaction(Participant, Num_throws)),
            size = .5,
            alpha = .2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  facet_wrap(~Num_throws) + 
  guides(fill = F)

# save this 
ggsave("../../Figures/Experiment_2_Two_throw/Accuracy_regions.png",
       height = 3,
       width = 5.6)
