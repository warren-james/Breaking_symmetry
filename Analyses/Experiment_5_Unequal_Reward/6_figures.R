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
                                labels = c("90%", "75%", "25%", "10%")))  
  
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
  scale_x_discrete("Slab Measures") +
  scale_y_continuous(breaks = c(0,1),
                     labels = c("Centre", "Side")) +
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        legend.position = "bottom") +
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
  scale_fill_ptol() + 
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_y_discrete(limits = seq(0,1,.25)) + 
  theme(legend.position = "bottom")
plt_prop_gamble_types$labels$y <- "% of each split"
plt_prop_gamble_types$labels$fill <- "Split"
plt_prop_gamble_types

# save 
ggsave("../../Figures/Experiment_5_Unequal_Reward/Percent_split.png",
       height = 3, 
       width = 5.6)

# combine the previous two plots 
plt_save <- gridExtra::grid.arrange(plt_box, plt_prop_gamble_types, ncol = 2)
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
