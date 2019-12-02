#### Analysis for Probability study ####
# These are the analyses that will be included in the paper

#### Library ####
library(tidyverse)
library(lme4)
library(brms)

#### functions ####
# should I write a function for plotting predictions?

#### load data ####
load("scratch/new_data/df_part2_fixed")

# processing 
df_model <- df_part2_fixed %>% 
  filter(separation != 640) %>% # remove furthest point for now
  select(participant, dist_type, bias_type, separation, st_box, accuracy) %>% 
  mutate(Ml_fix = ifelse(st_box == "most likely", 1, 0),
         S_fix = ifelse(st_box != "centre", 1, 0)) 

#### analysis ####
#### Frequentist ####
#### Fixation to Ml side ####
# likely fixation ~ bias_type
m_fix_like <- glmer(Ml_fix ~ bias_type + (bias_type|participant), 
                    data = df_model,
                    family = "binomial")

# add in separation? 
m_fix_like_sep <- glmer(Ml_fix ~ bias_type * dist_type + (1|participant),
                        data = df_model,
                        family = "binomial")

# add some random effects 
m_fix_like_sep.1 <- glmer(Ml_fix ~ bias_type * dist_type + (dist_type + bias_type|participant), 
                    data = df_model,
                    family = "binomial")
# fully random 
m_fix_like_sep.2 <- glmer(Ml_fix ~ bias_type * dist_type + (bias_type * dist_type|participant),
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
df_model$p = predict(m_fix_like_sep.2, type = "response")
df_model$p_fe = predict(m_fix_like_sep.2, re.form = NA, type = "response")
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


#### Fixation Side ####
# full model like the above 
# fails to converge... try bayes for now 
m_fix_S_sep.2 <- glmer(S_fix ~ bias_type * dist_type + (bias_type + dist_type|participant),
                       data = df_model,
                       family = "binomial",
                       control=glmerControl(optCtrl=list(maxfun=2e4)))

# this version converges... seems that dist_type wasn't contributing anything of value
m_fix_S_sep.2 <- glmer(S_fix ~ bias_type * dist_type + (bias_type|participant),
                       data = df_model,
                       family = "binomial")

# plot this 
df_model$p = predict(m_fix_S_sep.2, type = "response")
df_model$p_fe = predict(m_fix_S_sep.2, re.form = NA, type = "response")
df_model %>% 
  mutate(participant = as.factor(as.numeric(participant))) %>%
  group_by(participant, bias_type, dist_type) %>% 
  summarise(Predicted = mean(p),
            Actual = mean(S_fix),
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

#### Side ####
bm_fix_S_dt <- brm(S_fix ~ (bias_type + dist_type)^2 + (dist_type + bias_type|participant), 
                   data = df_model,
                   family = "bernoulli",
                   chains = 1,
                   iter = 1000,
                   warmup = 500)

df_model$p_b <- predict(bm_fix_S_dt, type = "response")
df_model$p_b_fe <- predict(bm_fix_S_dt, re.form = NA, type = "response")
df_model %>% 
  mutate(participant_num = as.numeric(as.factor(participant))) %>%
  group_by(participant, participant_num, dist_type, bias_type) %>%
  summarise(Actual = mean(S_fix),
            Predicted = mean(p_b[,1]),
            Predicted_lower = mean(p_b[,3]),
            Predicted_upper = mean(p_b[,4]),
            FE = mean(p_b_fe[,1]),
            FE_lower = mean(p_b_fe[,3]),
            FE_upper = mean(p_b_fe[,4])) %>% 
  ungroup() %>% 
  group_by(dist_type, bias_type) %>% 
  mutate(Predicted_lower = mean(Predicted_lower),
         Predicted_upper = mean(Predicted_upper),
         FE = mean(FE),
         FE_lower = mean(FE_lower),
         FE_upper = mean(FE_upper)) %>%
  ggplot(aes(participant,# Proportion,
             colour = dist_type,
             fill = dist_type)) +
  geom_point(aes(y = Actual),
             shape = 21) +
  geom_point(aes(y = Predicted),
             fill = "white",
             shape = 21) +
  geom_ribbon(aes(x = participant_num,
                  ymin = Predicted_lower,
                  ymax = Predicted_upper,
                  fill = dist_type),
              alpha = .1) + 
  geom_hline(aes(yintercept = FE,
                 colour = dist_type),
             linetype = "dashed") +
  facet_wrap(~bias_type) + 
  see::scale_color_flat() +
  see::scale_fill_flat() +
  theme_bw()
  
#### All together? ####
# need to figure out how to do this... 
# this takes a while to run so do this when I have my pc 
bm_allchoices <- brm(st_box ~ bias_type * dist_type + (bias_type + dist_type | participant),
                     data = df_model,
                     family = categorical(link = "logit"),
                     chains = 1,
                     iter = 1000,
                     warmup = 500, 
                     refresh = 2)
# save this 
save(bm_allchoices, file = "modelling/BRMS/model_output/bm_multinomial")

#### Accuracy ####
