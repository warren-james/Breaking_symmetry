#### Figures ####
# sort some figures for the two throw experiment 

#### Library ####
library(tidyverse)

#### load data ####
load("scratch/df_part2")

#### constants ####
save_route <- c("../../Figures/Experiment_2_Two_throw/")

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


# Get mean hoop positions for plots 
df_mu_hoop <- df_part2 %>% 
  group_by(slab_measures) %>% 
  summarise(mu_hoop_pos = mean(HoopDelta))

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

# ridges version 
df_part2 %>% 
  ggplot(aes(
    abspos,
    slab_measures,
    colour = Num_throws,
    fill = Num_throws
    )) + 
  ggridges::geom_density_ridges(alpha = .6,
                                stat = "binline",
                                bins = 10) + 
  facet_wrap(~Num_throws)

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
  scale_x_discrete(expression(paste("Hoop Delta (", Delta, ")", sep = "")),
                   labels = c("C", "-1", "0", "+1", "+2", "F")) +
  theme_bw() +
  # guides(colour = guide_legend("No. Throws"),
  #        fill = guide_legend("No. Throws")) +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    # legend.title = element_text(size = 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
plt_box

# save 
ggsave(plt_box, file = paste(save_route, "box_plt.png", sep = ""),
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
plt_ribbon <- df_acc %>% 
  merge(df_mu_hoop) %>%
  group_by(Participant, slab_measures, mu_hoop_pos, Num_throws) %>% 
  summarise(Expected = mean(Expected),
            Centre = mean(Centre),
            Side = mean(Side)) %>%
  ungroup() %>% 
  group_by(slab_measures, mu_hoop_pos) %>%
  mutate(sep_factor = as.numeric(slab_measures),
         Best = pmax(Expected, Centre, Side),
         Worst = pmin(Expected, Centre, Side),
         ymin_Best = min(Best)-.01,
         ymax_Best = max(Best),
         ymin_Worst = min(Worst),
         ymax_Worst = max(Worst)) %>%
  ggplot(aes(mu_hoop_pos, Expected)) + 
  geom_ribbon(aes(x = mu_hoop_pos,
                  ymin = ymin_Worst,
                  ymax = ymax_Worst, 
                  fill = "red"),
              alpha = .35) + 
  geom_ribbon(aes(x = mu_hoop_pos,
                  ymin = ymin_Best,
                  ymax = ymax_Best,
                  fill = "green"),
              alpha = .35) +
  scale_y_continuous("Expected Accuracy", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expression(paste("Hoop Delta (", Delta, ")", sep = "")),
                     breaks = unique(df_mu_hoop$mu_hoop_pos),
                     # labels = c("~90%", "~50% - 1", "~50%", "~50% + 1", "~50% + 2", "~10%")
                     labels = c("C", "-1", "0", "+1", "+2", "F")
                     ) +
  # ggsci::scale_color_lancet() + 
  # ggsci::scale_fill_lancet() +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  geom_line(aes(group = interaction(Participant, Num_throws)),
            size = .5,
            alpha = .2) + 
  theme_bw() +
  theme(
    strip.text = element_text(size= 8),
    # axis.text.x = element_text(angle = 90, vjust = .5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~Num_throws) + 
  guides(fill = F)
plt_ribbon

# save this 
ggsave(paste(save_route, "Accuracy_regions.png", sep = ""),
       height = 3,
       width = 5.6)

# put these together 
# plt_save <- gridExtra::grid.arrange(plt_box, plt_ribbon, ncol = 1)
plt_save <- cowplot::plot_grid(plt_box, plt_ribbon, labels = c("a)", "b)"), label_size = 8, ncol = 1)
ggsave(plt_save, file = paste(save_route, "combine.png", sep = ""),
       height = 4,
       width = 5.6)

# some descs 
df_acc %>% 
  # filter(slab_measures == "~10%") %>%
  ggplot(aes(Expected, 
             colour = Num_throws,
             fill = Num_throws)) + 
  geom_density(position = "dodge",
               alpha = .3) + 
  facet_wrap(~slab_measures)

df_acc %>% 
  # filter(slab_measures == "~10%") %>% 
  # group_by(Num_throws) %>% 
  group_by(slab_measures, Num_throws) %>%
  summarise(mu = mean(Expected)) %>% 
  spread(Num_throws, mu)

# raw version 
df_part2
