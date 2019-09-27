#### Gambling Study ####
# Plotting
# proportion of equal vs. unequal for each participant 
# Accuracy data 
# Standing positions 
# Total earnings

#### Library ####
library(tidyverse)
# library(rstan)
library(gridExtra)
library(forcats)
library(ggthemes)

#### Constants ####
slab_size <- 0.46

#### load in data ####
load("scratch/data/df_part2")
load("scratch/data/df_part1")

# relevel factors 

#### Summaries and Plotting ####
# proportion of equal vs unequal 
plt_prop_gamble_types <- df_part2 %>% 
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  group_by(Participant, Gamble_Type) %>%
  summarise(n = n()) %>%
  ggplot(aes(Participant, n, fill = Gamble_Type)) +
  scale_fill_ptol() + 
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw() + 
  scale_y_discrete(limits = seq(0,12,2)) + 
  theme(legend.position = "bottom")
plt_prop_gamble_types$labels$y <- "No. of each split"
plt_prop_gamble_types$labels$fill <- "Split"
plt_prop_gamble_types

# probably want to save these as well
# add that in later

# choices made 
plt_standing_pos <- df_part2 %>%
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  ggplot(aes(HoopDelta, Norm_Dist, colour = Gamble_Type)) +
  scale_colour_ptol() +
  geom_jitter() + 
  theme_bw() + 
  facet_wrap(~Participant, ncol = 4) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
plt_standing_pos$labels$x <- "Delta (Metres)"
plt_standing_pos$labels$y <- "Nomalised Standing Position"
plt_standing_pos$labels$colour <- "Split"
plt_standing_pos

# plot together
grid.arrange(plt_prop_gamble_types,plt_standing_pos, ncol = 2)
plt_together <- arrangeGrob(plt_prop_gamble_types, plt_standing_pos, ncol = 2)

# save this 
ggsave(file = "../../Figures/Experiment_5_Unequal_Reward/prop_and_position.png", plt_together)

#### combine the above plots together somehow... 
# sort data first 
prop_split <- plt_prop_gamble_types[["data"]] %>% 
  ungroup() %>% 
  select(-.group) %>%
  spread(Gamble_Type, n) %>%
  replace_na(list(Equal = 0, Unequal = 0)) %>%
  mutate(Equal = Equal/(Equal + Unequal),
         Unequal = 1 - Equal,
         order = Equal,
         HoopDelta = 21) %>% 
  gather(Equal:Unequal,
         key = "Split",
         value = "prop")

plt_standing_pos[["data"]] 

plt_combined <- plt_standing_pos + 
  geom_bar(data = prop_split, 
           aes(HoopDelta, prop,
               fill = Split,
               colour = Split),
           stat = "identity") + 
  see::scale_color_flat() + 
  see::scale_fill_flat() 
plt_combined

#### Proportion over distance ####
plt_prop_dist <- df_part2 %>%
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  group_by(Participant, Gamble_Type, HoopDelta) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(Participant,
           Gamble_Type,
           HoopDelta,
           fill = list(n = 0)) %>%
  spread(Gamble_Type, n) %>%
  filter(Equal + Unequal > 0) %>%
  gather(Equal:Unequal,
         key = Gamble_Type,
         value = n) %>%
  mutate(freq = n/3) %>%
  filter(Gamble_Type != "Equal") %>%
  ggplot(aes(HoopDelta*slab_size, freq)) +
  geom_line() + 
  theme_bw() +
  facet_wrap(~Participant) + 
  theme(strip.text.x = element_text(margin = margin(0.01,
                                                    0,
                                                    0.01,
                                                    0,
                                                    "mm")))
plt_prop_dist$labels$x <- "Delta (Metres)"
plt_prop_dist$labels$y <- "Frequency of Unequal Splits"
plt_prop_dist


# just plot everything with equal and unequal gamebles
plt_everything <- df_part2 %>%
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  filter(Norm_Dist < 1.001) %>%
  ggplot(aes(HoopDelta, Norm_Dist, colour = Gamble_Type)) +
  scale_colour_ptol() +
  geom_jitter() + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) + 
  theme_bw() + 
  theme(legend.position = "bottom")
plt_everything$labels$x <- "Delta (Metres)"
plt_everything$labels$y <- "Normalised Standing Position"
plt_everything


#### Plot Trials ####
plt_trials <- df_part2 %>%
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  unite(Colour_Gamble, c(Colour, Gamble_Type)) %>%
  mutate(Colour_Gamble = fct_reorder(Colour_Gamble, HoopDelta)) %>%
  ggplot(aes(Trial, Norm_Dist, colour = Colour_Gamble, shape = Colour_Gamble)) + 
  geom_point() + 
  theme_bw() + 
  scale_color_manual(name = "Gamble type and Distance",
                     values = rep(c("red", "yellow", "blue", "green"), each = 2)) + 
  scale_shape_manual(name = "Gamble type and Distance",
                     values = rep(c(15,17), 4)) + 
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
  see::theme_lucid() + 
  facet_wrap(~Participant)
plt_trials$labels$y <- "Normalised Standing Position"
plt_trials

# save 
ggsave(file = "../../Figures/Experiment_5_Unequal_Reward/Plot_each_trial.png")

#### Proportion over distance ####
# need to standardise the distances... 
# setup data
plt_dist_prop <- df_part2 %>% 
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  group_by(Participant, Colour, Gamble_Type) %>%
  summarise(n = n()) %>%
  mutate(n = n/3) %>%
  ungroup() %>%
  complete(Participant,
           Gamble_Type,
           Colour,
           fill = list(n = 0))
# reorder Colour Factor
plt_dist_prop$Colour <- fct_relevel(plt_dist_prop$Colour, "R", "Y", "B", "G")
# make plot 
plt_dist_prop <- plt_dist_prop %>%
  ggplot(aes(Colour, n, fill = Gamble_Type)) +
  scale_fill_ptol() + 
  geom_bar(stat = "identity", width = 0.5) +
  see::theme_lucid() + 
  facet_wrap(~Participant)
plt_dist_prop$labels$y <- ""
plt_dist_prop

# save 
ggsave(file = "../../Figures/Experiment_5_Unequal_Reward/Prop_gambles_dist.png")



