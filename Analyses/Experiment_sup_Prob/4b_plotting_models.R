#### Plotting model outputs ####

#### Library ####
library(tidyverse)
library(rstan)
library(Rlab)

# #### Functions ####
# post_preds_berno <- function(model, x_vals, m_matrix){
#   post <- rstan::extract(model)
#   
#   beta <- colMeans(post$beta)
#   
#   mu <- m_matrix %*% beta
#   
#   p <-plogis(mu)
#   
#   hpdi_dist <- data.frame(lower = numeric(),
#                           upper = numeric())
#   
#   for(ii in 1:ncol(m_matrix)){
#     for(iter in 1:1000){
#       
#     }
#     # temp <- as.numeric(HDInterval::hdi(rbern(1000, p[ii])))
#     # hpdi_dist <- rbind(hpdi_dist, data.frame(lower = temp[1],
#     #                                          upper = temp[2]))
#   }
#   
#   result <- list("p" = p, "hpdi_dist" = hpdi_dist)
#   return(result)
# }

#### Plotting ####
#### Plot: M1 - fl ~ bias_type ####
load("modelling/Stan/model_data/m_data_fix_trim")
load("modelling/Stan/model_outputs/m1_fl_berno")

post <- extract(m1_fl_berno)$beta
post <- as.tibble(post) %>%
  `colnames<-`(c("Bias", "Random")) %>%
  mutate(Bias = plogis(Bias),
        Random = plogis(Bias + Random)) %>%
  gather(Bias:Random,
         key = "Condition",
         value = "Proportion")

plt <- post %>%
  ggplot(aes(Proportion, 
             colour = Condition,
             fill = Condition)) + 
  geom_density(alpha = 0.3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  see::theme_lucid() + 
  scale_x_continuous(limits = c(0,1))
plt

hdpi <- post %>% 
  group_by(Condition) %>%
  summarise(Upper = HDInterval::hdi(Proportion)[2],
            Lower = HDInterval::hdi(Proportion)[1])

#### Plot: M2 - fl ~ (bias_type + delta)^2 ####
load("modelling/Stan/model_outputs/m2_fl_berno")

post <- extract(m2_fl_berno)$beta
post <- as.tibble(post) %>%
  `colnames<-`(c("Bias", "Random")) %>%
  mutate(Bias = plogis(Bias),
         Random = plogis(Bias + Random))


