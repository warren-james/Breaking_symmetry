library(tidyverse)

beanbagdat <- read.csv("data/Part_1_measures.txt", sep = "\t")

# num_particiapnts
num_particiapnts <- length(unique(paste(beanbagdat$experimenter, 
                                        beanbagdat$participant)))

#### collapse across directions ####
# sort data 
beanbagdat <- beanbagdat %>% 
  mutate(participant = paste(experimenter, participant, sep = "_")) %>%
  group_by(participant, slab, hoop_size) %>% 
  summarise(inhoop = sum(inhoop)) %>%
  ungroup() %>%
  mutate(acc = inhoop/18,
         off_set = log((1-0.01)/0.01),
         participant_num = factor(participant,
                                  labels = c(seq(1, num_particiapnts, 1))))

#### get accuracy over distance, for each participant and hoop size ####
m = glm(data = beanbagdat, acc ~ slab:hoop_size:participant, binomial, offset = off_set)
beanbagdat$p = predict(m, type="response")

load("scratch/df_part2_norm") 

# TODO: 
# Need to fix this to make the plot make sense... 
# at the moment, I'm a bit confused about what we're doing... 
# Need to add something in the norm dat to say what size target we're aiming for on each trial
df_part2 <- norm_dat %>% 
  as_tibble() %>%
  mutate(
    hoop_size = tolower(TargetSize),
    slab = case_when(TargetSize == "Large" ~ abs(subject_position - large_pos), 
                             TRUE ~ abs(subject_position - small_pos))
  ) %>% 
  # mutate(
  #   northSize = case_when(
  #     colour == "B" ~ as.character(B_N_Size),
  #     colour == "Y" ~ as.character(Y_N_Size),
  #     colour == "R" ~ as.character(R_N_Size)
  #   ), 
  #   southSize = ifelse(northSize == "small", "large", "small"), 
  #   hoop_size = ifelse(direction == "South", southSize, northSize), 
  #   hoop_pos = ifelse(hoop_size == "small", small_pos, large_pos), 
  #   slab = abs(subject_position - hoop_pos)
  # ) %>% 
  group_by(participant, slab, hoop_size) %>% 
  summarise(acc = mean(accuracy),
            n = n())

df_preds <- expand_grid(participant = unique(df_part2$participant),
                        slab = unique(df_part2$slab),
                        hoop_size = c("small", "large"),
                        off_set = log((1-0.01)/0.01))
df_preds$p <- predict(m, type = "response", newdata = df_preds)

df_part2 %>% 
  ggplot(aes(slab, acc,
             colour = hoop_size)) + 
  geom_point(
    # aes(alpha = log(n))
    ) + 
  geom_line(data = df_preds, 
            aes(slab, p)) +
  # scale_alpha(range = c(.1, 1)) +
  facet_wrap(~participant) +
  theme_bw()
