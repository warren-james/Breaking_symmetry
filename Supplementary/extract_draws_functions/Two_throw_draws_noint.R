#### draws functions #### 
draw_post_delta <- function(model, data){
  close <- min(data$norm_delta)
  mid <- median(data$norm_delta)
  far <- max(data$norm_delta)
  
  draws_df <- model %>% 
    spread_draws(b_Num_throwsOne,
                 b_norm_delta,
                 b_Num_throwsTwo,
                 `b_norm_delta:Num_throwsTwo`) %>%
    mutate(One = b_Num_throwsOne,
           Norm_delta_OT = b_norm_delta,
           Two = b_Num_throwsTwo,
           Norm_delta_TT = `b_norm_delta:Num_throwsTwo`) %>% 
    select(-b_Num_throwsOne,
           -b_norm_delta,
           -b_Num_throwsTwo,
           -`b_norm_delta:Num_throwsTwo`) %>%
    mutate(hoop_Close.One = One + (close * Norm_delta_OT),
           hoop_Mid.One = One + (mid * Norm_delta_OT),
           hoop_Far.One = One + (far * Norm_delta_OT),
           hoop_Close.Two = Two + (close * Norm_delta_TT),
           hoop_Mid.Two = Two + (mid * Norm_delta_TT),
           hoop_Far.Two = Two + (far * Norm_delta_TT)) %>%
    select(.iteration,
           hoop_Close.One,
           hoop_Mid.One,
           hoop_Far.One,
           hoop_Close.Two,
           hoop_Mid.Two,
           hoop_Far.Two) %>% 
    gather(c(hoop_Close.One:hoop_Far.Two),
           key = "parameter",
           value = "estimate") %>% 
    separate(parameter,
             into= c("Dist_type", "Num_throws"),
             sep = "\\.") %>% 
    separate(Dist_type, 
             into = c("remove", "Dist_type"),
             sep = "_") %>% 
    select(-remove)
  # mean_vals 
  mu_vals <- draws_df %>% 
    group_by(Dist_type, Num_throws) %>% 
    summarise(mu = mean(estimate),
              med = median(estimate)) %>% 
    mutate(mu = boot::inv.logit(mu),
           med = boot::inv.logit(med))
  # estimates
  plt_estimates <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>% 
    ggplot(aes(prop,
               colour = Num_throws,
               fill = Num_throws)) +
    geom_density(alpha = .3) + 
    see::scale_color_flat() + 
    see::scale_fill_flat() + 
    scale_x_continuous("Normalised Delta") + 
    theme_bw() + 
    coord_cartesian(expand = 0) +
    geom_vline(data = mu_vals,
               aes(xintercept = mu,
                   colour = Num_throws),
               linetype = "dashed") +
    facet_wrap(~Dist_type)
  
  # get diff
  # overall
  diff_overall <- draws_df %>% 
    group_by(.iteration, Num_throws) %>% 
    summarise(estimate = boot::inv.logit(mean(estimate))) %>%
    spread(Num_throws, estimate) %>%
    ungroup() %>% 
    mutate(diff = One - Two)
  
  above0_overall <- diff_overall %>% 
    mutate(above0 = ifelse(diff > 0,1,0)) %>% 
    summarise(above0 = mean(above0))
  
  plt_diff_over <- diff_overall %>% 
    ggplot(aes(diff)) + 
    geom_density(colour = "blue",
                 fill = "blue",
                 alpha = .3)
  
  # by dist_type
  diff_dist_type <- draws_df %>% 
    mutate(estimate = boot::inv.logit(estimate)) %>%
    spread(Num_throws, estimate) %>%
    ungroup() %>% 
    mutate(diff = One - Two)
  
  plt_diff_dist <- diff_dist_type %>% 
    ggplot(aes(diff, 
               colour = Dist_type,
               fill = Dist_type)) + 
    geom_density(alpha = .3)
  
  above0_dist <- diff_dist_type %>%
    group_by(Dist_type) %>%
    mutate(above0 = ifelse(diff > 0,1,0)) %>% 
    summarise(above0 = mean(above0))
  
  # get hdi of diff
  # overall
  draws_hdi_overall <- draws_df %>%
    group_by(Num_throws) %>%
    mutate(prop = boot::inv.logit(estimate)) %>%
    summarise(lower = hdi(prop)[,1],
              mean = mean(prop),
              upper = hdi(prop)[,2],
              med = median(prop))
  
  draws_hdi_diff_overall <- diff_overall %>% 
    summarise(lower = hdi(diff)[,1],
              mean = mean(diff),
              upper = hdi(diff)[,2])
  
  # dist_type
  draws_hdi_dist <- draws_df %>%
    group_by(Num_throws, Dist_type) %>%
    mutate(prop = boot::inv.logit(estimate)) %>%
    summarise(lower = hdi(prop)[,1],
              mean = mean(prop),
              upper = hdi(prop)[,2],
              med = median(prop))
  
  draws_hdi_diff_dist <- diff_dist_type %>% 
    group_by(Dist_type) %>%
    summarise(lower = hdi(diff)[,1],
              mean = mean(diff),
              upper = hdi(diff)[,2])
  
  plts <- list(plt_estimates, 
               plt_diff_over,
               plt_diff_dist)
  hdis <- list(draws_hdi_overall,
               draws_hdi_diff_overall,
               draws_hdi_dist,
               draws_hdi_diff_dist)
  above0 <- list(above0_overall,
                 above0_dist)
  
  output <- list(draws_df,
                 plts,
                 hdis,
                 above0)
  names(output) <- c("Draws_df",
                     "Plots",
                     "HDIs",
                     "Above_0")
  return(output)
}

# draw_post_acc
draw_post_acc <- function(model, data, acc_type){
  draws_df <- model %>% 
    spread_draws(b_Intercept,
                 b_Num_throwsTwo) %>%
    mutate(One_throw = b_Intercept,
           Two_throw = b_Num_throwsTwo + b_Intercept) %>% 
    select(-b_Intercept,
           -b_Num_throwsTwo) %>%
    select(.iteration,
           One_throw,
           Two_throw) %>% 
    gather(c(One_throw, Two_throw),
           key = "Num_throws",
           value = "estimate")
  # mean_vals 
  mu_vals <- draws_df %>% 
    group_by(Num_throws) %>% 
    summarise(mu = mean(estimate),
              med = median(estimate)) %>% 
    mutate(mu = boot::inv.logit(mu),
           med = boot::inv.logit(med))
  # estimates
  plt_estimates <- draws_df %>% 
    mutate(prop = boot::inv.logit(estimate)) %>% 
    ggplot(aes(prop,
               colour = Num_throws,
               fill = Num_throws)) +
    geom_density(alpha = .3) + 
    see::scale_color_flat() + 
    see::scale_fill_flat() + 
    scale_x_continuous(acc_type, labels = scales::percent_format(accuracy = 1)) + 
    theme_bw() + 
    coord_cartesian(expand = 0) +
    geom_vline(data = mu_vals,
               aes(xintercept = mu,
                   colour = Num_throws),
               linetype = "dashed") 
  
  # get diff
  # overall
  diff_overall <- draws_df %>% 
    group_by(.iteration, Num_throws) %>% 
    summarise(estimate = boot::inv.logit(mean(estimate))) %>%
    spread(Num_throws, estimate) %>%
    ungroup() %>% 
    mutate(diff = One_throw - Two_throw)
  
  above0_overall <- diff_overall %>% 
    mutate(above0 = ifelse(diff > 0,1,0)) %>% 
    summarise(above0 = mean(above0))
  
  plt_diff_over <- diff_overall %>% 
    ggplot(aes(diff)) + 
    geom_density(colour = "blue",
                 fill = "blue",
                 alpha = .3)
  
  # get hdi of diff
  # overall
  draws_hdi_overall <- draws_df %>%
    group_by(Num_throws) %>%
    mutate(prop = boot::inv.logit(estimate)) %>%
    summarise(lower = hdi(prop)[,1],
              mean = mean(prop),
              upper = hdi(prop)[,2],
              med = median(prop))
  
  draws_hdi_diff_overall <- diff_overall %>% 
    summarise(lower = hdi(diff)[,1],
              mean = mean(diff),
              upper = hdi(diff)[,2])
  
  # lists to output 
  plts <- list(plt_estimates, 
               plt_diff_over)
  hdis <- list(draws_hdi_overall,
               draws_hdi_diff_overall)
  above0 <- list(above0_overall)
  
  output <- list(draws_df,
                 plts,
                 hdis,
                 above0)
  names(output) <- c("Draws_df",
                     "Plots",
                     "HDIs",
                     "Above_0")
  return(output)
  
}

