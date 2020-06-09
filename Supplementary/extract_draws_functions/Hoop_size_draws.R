# Hoop size extract draws function 
HS_draw_post <- function(model, data){
  close <- min(data$norm_hoop_pos)
  mid <- median(data$norm_hoop_pos)
  far <- max(data$norm_hoop_pos)
  
  draws_df <- model %>% 
    spread_draws(b_Intercept,
                 b_norm_hoop_pos) %>% 
    mutate(hoop_Close = b_Intercept + close * b_norm_hoop_pos,
           hoop_Mid = b_Intercept + mid * b_norm_hoop_pos,
           hoop_Far = b_Intercept + far * b_norm_hoop_pos) %>% 
    select(.iteration,
           hoop_Close,
           hoop_Mid,
           hoop_Far) %>% 
    gather(c(hoop_Close:hoop_Far),
           key = "parameter",
           value = "estimate") %>% 
    separate(parameter,
             into = c("remove", "Distance Type")) %>% 
    select(-remove) 
  # estimates
  plt_estimates <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate),
           `Distance Type` = factor(`Distance Type`, c("Close", "Mid", "Far"))) %>% 
    ggplot(aes(prop,
               colour = `Distance Type`,
               fill = `Distance Type`)) +
    geom_density(alpha = .3) + 
    see::scale_color_flat() + 
    see::scale_fill_flat() +
    scale_x_continuous(expression(paste("Normalised ", Delta))) + 
    theme_bw()
  # amount above .5
  prop_above.5 <- draws_df %>%
    mutate(above0_5 = ifelse(boot::inv.logit(estimate) > .5, 1, 0)) %>%
    summarise(above0_5 = mean(above0_5))
  # get hdi 
  draws_hdi <- draws_df %>%
    group_by(`Distance Type`) %>%
    mutate(prop = boot::inv.logit(estimate)) %>%
    summarise(lower = hdi(prop)[,1],
              mean = mean(prop),
              upper = hdi(prop)[,2],
              med = median(prop)) 
  # overall hdi 
  draws_hdi_overall <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>% 
    summarise(lower = hdi(prop)[,1],
              mean = mean(prop),
              upper = hdi(prop)[,2],
              med = median(prop)) 
  
  Hdi_pos <- list(draws_hdi,
                  draws_hdi_overall)
  output <- list(draws_df,
                 plt_estimates,
                 prop_above.5,
                 Hdi_pos)
  names(output) <- c("draws_df",
                     "plt_estimates",
                     "Prop_above.5",
                     "draws_HDI")
  return(output)
}
