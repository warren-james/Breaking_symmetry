# Rewards draws 
draws_factor <- function(model){
  draws_df <- model %>% 
    spread_draws(b_close_equal,
                 b_close_unequal,
                 b_far_equal,
                 b_far_unequal) %>% 
    mutate(GambleType_Equal.Disttype_Close = b_close_equal,
           GambleType_Unequal.Disttype_Close = b_close_unequal,
           GambleType_Equal.Disttype_Far =  b_far_equal,
           GambleType_Unequal.Disttype_Far = b_far_unequal) %>% 
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
