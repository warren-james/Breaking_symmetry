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

# just plot everything with equal and unequal gamebles
plt_everything <- df_part2 %>%
  mutate(Participant = as.factor(as.numeric(Participant))) %>%
  filter(Norm_Dist < 1.001) %>%
  ggplot(aes(HoopDelta, Norm_Dist, colour = Gamble_Type)) +
  geom_jitter() + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) + 
  theme_bw() + 
  theme(legend.position = "bottom")
plt_everything$labels$x <- "Delta (Metres)"
plt_everything$labels$y <- "Normalised Standing Position"
plt_everything


# With trial info?
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
  facet_wrap(~Participant)
plt_trials$labels$y <- "Normalise Standing Position"
plt_trials