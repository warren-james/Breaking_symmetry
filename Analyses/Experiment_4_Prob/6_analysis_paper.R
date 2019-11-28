#### Analysis for Probability study ####
# These are the analyses that will be included in the paper

#### Library ####
library(tidyverse)
library(lme4)
library(brms)

#### functions ####

#### load data ####
load("scratch/new_data/df_part2_fixed")

# processing 
df_model <- df_part2_fixed %>% 
  filter(separation != 640) %>% # remove furthest point for now
  select(participant, dist_type, bias_type, separation, st_box, accuracy) %>% 
  mutate(Ml_fix = ifelse(st_box == "most likely", 1, 0),
         C_fix = ifelse(st_box == "centre", 1, 0),
         ) # rescale separation to avoid 

#### analysis ####
#### Frequentist ####
#### Fixation to Ml side ####
# likely fixation ~ bias_type
m_fix_like <- glmer(Ml_fix ~ bias_type + (bias_type|participant), 
                    data = df_model,
                    family = "binomial")

# add in separation? 
m_fix_like_sep <- glmer(Ml_fix ~ (bias_type + dist_type)^2 + (1|participant),
                        data = df_model,
                        family = "binomial")

# add some random effects 
m_fix_like_sep.1 <- glmer(Ml_fix ~ (bias_type + dist_type)^2 + (dist_type + bias_type|participant), 
                    data = df_model,
                    family = "binomial")
# fully random 
m_fix_like_sep.2 <- glmer(Ml_fix ~ (bias_type + dist_type)^2 + ((bias_type + dist_type)^2|participant),
                          data = df_model,
                          family = "binomial")

# random: effects are independent 
m_fix_like_sep.3 <- glmer(Ml_fix ~ (bias_type + dist_type)^2 +
                            (1|participant) +
                            (bias_type - 1|participant) +
                            (dist_type - 1|participant),
                          data = df_model, 
                          family = "binomial")

# plot this? 
df_model$p = predict(m_fix_like_sep.3, type = "response")
df_model$p_fe = predict(m_fix_like_sep.3, re.form = NA, type = "response")
df_model %>% 
  mutate(participant = as.factor(as.numeric(participant))) %>%
  group_by(participant, bias_type, dist_type) %>% 
  summarise(Predicted = mean(p),
            Actual = mean(Ml_fix),
            FE = mean(p_fe)) %>%
  ungroup() %>% 
  group_by(bias_type, dist_type) %>%
  mutate(med_FE = median(Predicted)) %>%
  ggplot(aes(participant, Proportion,
             colour = dist_type,
             fill = dist_type)) +
  geom_point(aes(y = Actual),
             shape = 21) +
  geom_point(aes(y = Predicted),
             fill = "white",
             shape = 21) +
  geom_hline(aes(yintercept = FE,
                 colour = dist_type),
             linetype = "dashed") +
  facet_wrap(~bias_type) + 
  see::scale_color_flat() +
  see::scale_fill_flat() +
  theme_bw()

# need to extract fixed effects... not too hard 
df_model$p_fe <- predict(m_fix_like_sep.2, re.form = NULL, type = "response")

# an overall plot? 
df_model %>% 
  group_by(bias_type, dist_type, participant) %>% 
  summarise(Predicted = mean(p),
            Actual = mean(Ml_fix)) %>% 
  gather(Predicted:Actual, 
         key = "Type",
         value = "Proportion") %>% 
  ggplot(aes(dist_type, Proportion,
             colour = Type,
             fill = Type)) + 
  geom_boxplot(alpha = .3) + 
  facet_wrap(~bias_type) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(0,1,.2)) +
  see::scale_color_flat() + 
  see::scale_fill_flat() 



#### Accuracy ####
# Is this needed really?
# Maybe we can do what we did with the penguin paper?
#### Bayesian ####
#### Ml side ####
# most complex model we need?
bm_fix_like_dt <- brm(Ml_fix ~ (bias_type + dist_type)^2 + (dist_type + bias_type|participant),
                      data = df_model,
                      family = "bernoulli",
                      chains = 1,
                      iter = 1000,
                      warmup = 500)

#### Accuracy ####
