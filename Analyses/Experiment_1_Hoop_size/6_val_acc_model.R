library(tidyverse)

load("scratch/df_part2_raw") 
dat %>% 
  as_tibble(dat) %>%
  mutate(
    n_size = case_when(
      colour == "B" ~ as.numeric(B_N_Size),
      colour == "Y" ~ as.numeric(Y_N_Size),
      colour == "R" ~ as.numeric(R_N_Size)),
    size = if_else(direction == "North", n_size, (n_size + 1) %% 2),
    size = if_else(size ==0 , 2, size),
    hoop_pos = if_else(direction == "South", -hoop_pos, hoop_pos),
    delta = abs(hoop_pos - subject_position)) %>%
  select(participant, direction, subject_position, size, hoop_pos, accuracy, delta) -> dat


ggplot(dat, aes(delta, accuracy, colour = size)) + geom_point() + 
  facet_wrap(~participant)



