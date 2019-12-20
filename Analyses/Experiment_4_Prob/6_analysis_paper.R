#### Analysis for Probability study ####
# These are the analyses that will be included in the paper

#### Library ####
library(tidyverse)
library(tidybayes)
library(lme4)
library(brms)

#### functions ####
# should I write a function for plotting predictions?
draw_post <- function(model) {
  draws_df <- model %>% 
    spread_draws(b_Intercept,
                 b_bias_typeSymmetric,
                 b_dist_typeFar,
                 `b_bias_typeSymmetric:dist_typeFar`) %>% 
    mutate(Biastype_Bias.Disttype_Close = b_Intercept,
           Biastype_Symmetric.Disttype_Close = b_Intercept + b_bias_typeSymmetric,
           Biastype_Bias.Disttype_Far = b_Intercept + b_dist_typeFar,
           Biastype_Symmetric.Disttype_Far = b_Intercept + b_bias_typeSymmetric +
             `b_bias_typeSymmetric:dist_typeFar`) %>% 
    select(.iteration,
           Biastype_Bias.Disttype_Close,
           Biastype_Symmetric.Disttype_Close,
           Biastype_Bias.Disttype_Far,
           Biastype_Symmetric.Disttype_Far) %>% 
    gather(c(Biastype_Bias.Disttype_Close:Biastype_Symmetric.Disttype_Far),
           key = "parameter",
           value = "estimate") %>% 
    separate(c(parameter),
             into = c("temp1", "temp2"),
             sep = "\\.") %>% 
    separate(temp1,
             into = c("remove", "Bias_type")) %>% 
    select(-remove) %>% 
    separate(temp2,
             into = c("remove", "Dist_type")) %>% 
    select(-remove) 
  # estimates
  plt_estimates <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>% 
    ggplot(aes(prop, colour = Bias_type, 
               fill = Bias_type)) +
    geom_density(alpha = .3) +
    # geom_histogram(position = "dodge") + 
    facet_wrap(~Dist_type)
  # difference 
  # groups
  diff_bias <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>% 
    select(-estimate) %>%
    spread(Bias_type,
           prop) %>%
    mutate(diff = Bias - Symmetric)
  # separation
  diff_dist <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>%
    select(-estimate) %>%
    spread(Dist_type,
           prop) %>%
    mutate(diff = Far - Close)
  
  # plotting this
  plt_diff <- diff_bias %>% 
    ggplot(aes(diff, colour = Dist_type,
               fill = Dist_type)) +
    geom_density(alpha = .3)
  prop_diff <- diff_bias %>%
    mutate(above0 = ifelse(diff > 0, 1, 0)) %>% 
    group_by(Dist_type) %>% 
    summarise(above0 = mean(above0))
  # get hdi 
  draws_hdi <- draws_df %>%
    mutate(prop = boot::inv.logit(estimate)) %>%
    group_by(Bias_type, Dist_type) %>% 
    summarise(lower = hdci(prop)[1,1],
              mean = mean(prop),
              upper = hdci(prop)[1,2],
              med = median(prop))
  # get hdi difference
  # bias
  hdi_bias_diff_overall <- diff_bias %>%
    summarise(lower = hdci(diff)[1,1],
              mu = mean(diff),
              upper = hdci(diff)[1,2])
  
  hdi_bias_diff_dist <- diff_bias %>%
    group_by(Dist_type) %>% 
    summarise(lower = hdci(diff)[1,1],
              mu = mean(diff),
              upper = hdci(diff)[1,2])
  
  # dist
  hdi_dist_diff_overall <- diff_dist %>% 
    summarise(lower = hdci(diff)[1,1],
              mu = mean(diff),
              upper = hdci(diff)[1,2])
  
  hdi_dist_diff_bias <- diff_dist %>%  
    group_by(Bias_type) %>%
    summarise(lower = hdci(diff)[1,1],
              mu = mean(diff),
              upper = hdci(diff)[1,2])
  
  Hdi_list <- list(hdi_bias_diff_dist,
                   hdi_bias_diff_overall,
                   hdi_dist_diff_overall,
                   hdi_dist_diff_bias)
  output <- list(draws_df,
                 plt_estimates,
                 diff,
                 plt_diff,
                 Hdi_list,
                 draws_hdi)
  
  names(output) <- c("draws_df",
                     "plt_estimates",
                     "difference_df",
                     "plt_difference",
                     "HDI_Diff",
                     "draws_HDI")
  return(output)
}

#### load data ####
load("scratch/new_data/df_part2_fixed")

# processing 
df_model <- df_part2_fixed %>% 
  # filter(separation != 640) %>% # remove furthest point for now
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
  mutate(mean_FE = mean(Predicted)) %>%
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
bm_fix_like_dt <- brm(Ml_fix ~ (bias_type + dist_type)^2 + (dist_type * bias_type|participant),
                      data = df_model,
                      family = "bernoulli",
                      prior = c(set_prior("student_t(3, -.7, 4)", class = "b"),
                                set_prior("student_t(3, 0, 4)", class = "b", coef = "bias_typeSymmetric"),
                                set_prior("student_t(3, 0, 4)", class = "b", coef = "bias_typeSymmetric:dist_typeFar"),
                                set_prior("student_t(3, 0, 4)", class = "b", coef = "dist_typeFar")),
                      chains = 1,
                      iter = 1000,
                      warmup = 500)
# save this 
save(bm_fix_like_dt, file = "modelling/BRMS/model_output/bm_fix_like_dt")

# get post
fix_like_post <- draw_post(bm_fix_like_dt)

# save 
save(fix_like_post, file = "modelling/BRMS/model_output/bm_fix_like_dt_draws")

# plots
fix_like_post[2]
fix_like_post[4]

# hdi of difference 
as.data.frame(fix_like_post[3]) %>% 
  group_by(Dist_type) %>% 
  summarise(lower = hdci(diff)[,1],
            mu = mean(diff),
            upper = hdci(diff)[,2])

#### Side ####
bm_fix_S_dt <- brm(S_fix ~ (bias_type + dist_type)^2 + (dist_type + bias_type|participant), 
                   data = df_model,
                   family = "bernoulli",
                   prior = c(set_prior("student_t(3, 0, 4)", class = "b"),
                             set_prior("student_t(3, 0, 4)", class = "b", coef = "bias_typeSymmetric"),
                             set_prior("student_t(3, 0, 4)", class = "b", coef = "bias_typeSymmetric:dist_typeFar"),
                             set_prior("student_t(3, 0, 4)", class = "b", coef = "dist_typeFar")),
                   
                   chains = 1,
                   iter = 1000,
                   warmup = 500)
# save this 
save(bm_fix_S_dt, file = "modelling/BRMS/model_output/bm_fix_S_dt")

# get post
fix_S_post <- draw_post(bm_fix_S_dt)

# save 
save(fix_S_post, file = "modelling/BRMS/model_output/bm_fix_S_dt_draws")


# plots
fix_S_post[2]
fix_S_post[4]

# testing
df_model$p_b <- predict(bm_fix_like_dt, type = "response")
df_model$p_b_fe <- predict(bm_fix_like_dt, re.form = NA, type = "response")
df_model %>% 
  mutate(participant_num = as.numeric(as.factor(participant))) %>%
  group_by(participant, participant_num, dist_type, bias_type) %>%
  summarise(Actual = mean(Ml_fix),
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
  # geom_ribbon(aes(x = participant_num,
  #                 ymin = Predicted_lower,
  #                 ymax = Predicted_upper,
  #                 fill = dist_type),
  #             alpha = .1) + 
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
