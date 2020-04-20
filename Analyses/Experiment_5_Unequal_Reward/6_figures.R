#### Make figures ####
# These are the figures for the unequal reward study 

#### Library ####
library(tidyverse)

#### load in data #### 
load("scratch/data/model_data")

#### Pre-processing #####
# sort the labels for their standing positions so we have some consitencies 
model_data <- model_data %>% 
  group_by(Participant) %>% 
  mutate(slab_measures = as.numeric(as.factor(Norm_Delta)),
         slab_measures = factor(slab_measures,
                                labels = c("90% (Close)", "75%", "25%", "10% (Far)")))  
  
#### Make plots ####
# density plot
model_data %>% 
  ggplot(aes(Norm_Dist,
             colour = Gamble_Type,
             fill = Gamble_Type)) + 
  geom_density(alpha = .3) +
  # geom_histogram(aes(y = ..density..),
  #                position = "dodge") +
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  facet_wrap(~dist_type)


# box plots 
plt_box <- model_data %>% 
  ggplot(aes(slab_measures, 
             Norm_Dist,
             colour = Gamble_Type,
             fill = Gamble_Type)) + 
  geom_boxplot(alpha = .3) +
  scale_x_discrete("Hoop Delta") +
  scale_y_continuous(breaks = c(0,1),
                     labels = c("Centre", "Side")) +
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        # legend.position = "right",
        legend.position = c(.2,.8),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        legend.box.margin = margin(t = -25, r = -25, b = -25, l = -25),
        legend.box = "vertical",
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 7)) +
  labs(colour = "Split Type",
       fill = "Split Type")
plt_box

# save
ggsave("../../Figures/Experiment_5_Unequal_Reward/box_plot_standing_pos.png", 
       height = 3,
       width = 5.6)

# prop of gamble types
# can either do percentages since there were some trials that were excluded from the analysis since 
# they were outside the range 
plt_prop_gamble_types <- model_data %>% 
  ungroup() %>%
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  group_by(Participant, Gamble_Type) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(Participant) %>% 
  mutate(n_total = sum(n),
         n_prop = n/n_total) %>% 
  ggplot(aes(Participant, n_prop, fill = Gamble_Type)) +
  see::scale_fill_flat() + 
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_y_discrete(limits = seq(0,1,.25)) + 
  theme(legend.position = "none",
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_blank())
plt_prop_gamble_types$labels$y <- "% of each split"
plt_prop_gamble_types$labels$fill <- "Split"
plt_prop_gamble_types

# save 
ggsave("../../Figures/Experiment_5_Unequal_Reward/Percent_split.png",
       height = 3, 
       width = 5.6)

# remake this as a hisotgram 
plt_hist <- model_data %>% 
  mutate(dist_type = ifelse(dist_type == "far", "Far", "Close")) %>%
  group_by(Participant, dist_type) %>% 
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(Participant, Gamble_Type, dist_type, count) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  # make a column for the "interaction"
  mutate(int = paste(Gamble_Type, dist_type, sep = "_")) %>% 
  select(Participant, count, n, int) %>% 
  complete(int, nesting(Participant), 
           fill = list(n = 0)) %>% 
  # separate
  separate(int, 
           into = c("Gamble_Type", "Dist_Type"),
           sep = "_") %>% 
  # need to use if else statement here for sanity
  mutate(prop = ifelse(n == 0, 0, n/count)) %>%
  filter(Gamble_Type != "Equal") %>%
  ggplot(aes(prop,
             colour = Dist_Type,
             fill = Dist_Type)) + 
  geom_histogram(position = "dodge",
                 binwidth = .1,
                 alpha = .3) +
  # geom_density(alpha = .3) +
  ggsci::scale_color_aaas() + 
  ggsci::scale_fill_aaas() +
  # see::scale_color_flat() + 
  # see::scale_fill_flat() + 
  scale_x_continuous("Proportion of Unequal splits",
                     breaks = seq(0,1,.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7)) +
  guides(fill = guide_legend("Distance"),
         colour = guide_legend("Distance"))
plt_hist

model_data %>%
  group_by(Participant) %>% 
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(Participant, Gamble_Type, count) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  complete(Gamble_Type, nesting(Participant, count),
           fill = list(n = 0)) %>%
  mutate(n = n/count) %>% 
  spread(Gamble_Type,
         n) %>% 
  ggplot() + 
  geom_histogram(aes(Equal),
                 fill = "blue",
                 alpha = .3,
                 binwidth = .1) + 
  geom_histogram(aes(Unequal),
                 fill = "red",
                 alpha = .3,
                 binwidth = .1) 
  


# combine the previous two plots 
# plt_save <- gridExtra::grid.arrange(plt_box, plt_prop_gamble_types, ncol = 2)
plt_save <- gridExtra::grid.arrange(plt_box, plt_hist, ncol = 2)
ggsave(plt_save, file = "../../Figures/Experiment_5_Unequal_Reward/combined_standing_prop_split.png",
       height = 3,
       width = 5.6)


# remake this but as a histogram 
model_data %>% 
  ggplot(aes(Norm_Dist, 
             colour = Gamble_Type,
             fill = Gamble_Type)) + 
  geom_histogram(alpha = .3,
                 position = "dodge") +
  facet_wrap(~dist_type) + 
  theme_bw() +
  see::scale_color_flat() + 
  see::scale_fill_flat()

# now for propotion opting for an unequal split 
# maybe a historgram of this? 
model_data %>% 
  group_by(Participant) %>% 
  summarise(unequal_prop = mean(Unequal)) %>% 
  ggplot(aes(unequal_prop)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density()
