Prob_draws <- function(model) {
  draws_df <- model %>% 
    spread_draws(b_Intercept,
                 b_bias_typeSymmetric,
                 b_dist_typeFar,
                 `b_bias_typeSymmetric:dist_typeFar`) %>% 
    mutate(Biastype_Bias.Disttype_Close = b_Intercept,
           Biastype_Symmetric.Disttype_Close = b_Intercept + b_bias_typeSymmetric,
           Biastype_Bias.Disttype_Far = b_Intercept + b_dist_typeFar,
           Biastype_Symmetric.Disttype_Far = b_Intercept + b_bias_typeSymmetric + b_dist_typeFar + 
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
