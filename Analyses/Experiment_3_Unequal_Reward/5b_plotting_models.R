#### New script for plotting model output ####
# This will just sort out the brms models for now 

#### library ####
library(brms)
library(tidybayes)
library(tidyverse)

#### Functions ####
# probably make a function to extract the draws and make plots? 
draws_factor <- function(model){
  draws_df <- model %>% 
    spread_draws(b_Intercept,
                 b_dist_typefar,
                 b_Gamble_TypeUnequal,
                 `b_dist_typefar:Gamble_TypeUnequal`) %>% 
    mutate(GambleType_Equal.Disttype_Close = b_Intercept,
           GambleType_Unequal.Disttype_Close = b_Intercept + b_Gamble_TypeUnequal,
           GambleType_Equal.Disttype_Far = b_Intercept + b_dist_typefar,
           GambleType_Unequal.Disttype_Far = b_Intercept + b_Gamble_TypeUnequal + b_dist_typefar + 
             `b_dist_typefar:Gamble_TypeUnequal`) %>% 
    select(.iteration,
           GambleType_Equal.Disttype_Close,
           GambleType_Unequal.Disttype_Close,
           GambleType_Equal.Disttype_Far,
           GambleType_Unequal.Disttype_Far) %>% 
    gather(c(GambleType_Equal.Disttype_Close:GambleType_Unequal.Disttype_Far),
           key = "parameter",
           value = "estimate") %>% 
    separate(c(parameter),
             into = c("temp1", "temp2"),
             sep = "\\.") %>% 
    separate(temp1,
             into = c("remove", "Gamble_Type")) %>% 
    select(-remove) %>% 
    separate(temp2,
             into = c("remove", "Dist_Type")) %>% 
    select(-remove) 
  return(draws_df)
}

#### load data ####
load("scratch/data/model_data")

#### Position models ####
# factor for distance 
load("scratch/model_outputs/m_brms_ri")

# get draws
draws_df <- draws_factor(m_brms_ri)

# save this 
save(draws_df, file = "scratch/model_outputs/m_brms_ri_draws")

# plot something 
draws_df %>% 
  mutate(prop = boot::inv.logit(estimate)) %>% 
  ggplot(aes(prop, 
             fill = Gamble_Type,
             colour = Gamble_Type)) + 
  geom_density(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() +
  facet_wrap(~Dist_Type)


#### Acc models ####
# factor for distance 
load("scratch/model_outputs/m_brms_acc")

# get draws 
draws_df <- draws_factor(m_brms_acc)

# plot this 
draws_df %>% 
  mutate(Exp_acc = boot::inv.logit(estimate)) %>%
  ggplot(aes(Exp_acc,
             fill = Gamble_Type,
             colour = Gamble_Type)) + 
  geom_density(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() + 
  facet_wrap(~Dist_Type)

