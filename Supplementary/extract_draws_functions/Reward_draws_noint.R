# Rewards draws 
draws_factor <- function(model){
  draws_df <- model %>% 
    spread_draws(b_dist_typeclose,
                 b_dist_typefar,
                 b_Gamble_TypeUnequal,
                 `b_dist_typefar:Gamble_TypeUnequal`) %>% 
    mutate(GambleType_Equal.Disttype_Close = b_dist_typeclose,
           GambleType_Unequal.Disttype_Close = b_Gamble_TypeUnequal + b_dist_typeclose,
           GambleType_Equal.Disttype_Far =  b_dist_typefar,
           GambleType_Unequal.Disttype_Far = b_dist_typefar + `b_dist_typefar:Gamble_TypeUnequal`) %>% 
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
