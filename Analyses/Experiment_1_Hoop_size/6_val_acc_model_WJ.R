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

df_part2 <- norm_dat %>% 
  as_tibble() %>%
  mutate(
    hoop_size = tolower(TargetSize),
    slab = case_when(TargetSize == "Large" ~ abs(subject_position - large_pos), 
                             TRUE ~ abs(subject_position - small_pos))
  ) %>% 
  group_by(participant, slab, hoop_size) %>% 
  summarise(acc = mean(accuracy),
            n = n()) %>% 
  mutate(off_set = log((1-0.01)/0.01))

df_preds <- expand_grid(participant = unique(df_part2$participant),
                        slab = unique(df_part2$slab),
                        hoop_size = c("small", "large"),
                        off_set = log((1-0.01)/0.01))
df_preds$p <- predict(m, type = "response", newdata = df_preds)
df_preds$resp <- predict(m, newdata = df_preds)

df_part2 %>% 
  ggplot(aes(slab, acc,
             colour = hoop_size)) + 
  geom_point(
    # aes(alpha = log(n))
    ) + 
  geom_line(data = df_preds, 
            aes(slab, p)) + 
  # geom_smooth(method = "glm",
  #             method.args = list(family = "binomial"),
  #             se = FALSE, 
  #             linetype = "longdash",
  #             fullrange = T) +
  # scale_alpha(range = c(.1, 1)) +
  facet_wrap(~participant) +
  theme_bw()

# NB: Below isn't really needed, but I've left it in just in case you want to look at it
# from: http://r-statistics.co/Logistic-Regression-With-R.html
library(InformationValue)
predicted <- plogis(predict(m, norm_dat %>% 
                              as_tibble() %>%
                              mutate(
                                off_set = log((1-0.01)/0.01),
                                hoop_size = tolower(TargetSize),
                                slab = case_when(TargetSize == "Large" ~ abs(subject_position - large_pos),
                                                 TRUE ~ abs(subject_position - small_pos))
                              ) %>%
                              select(participant, off_set, hoop_size, slab, accuracy)))
optCut <- optimalCutoff(norm_dat$accuracy, predicted)[1]
misClassError(norm_dat$accuracy, predicted, optCut)
# Lower values are better 


# now fit model to session 2 data and see what they look like 
m2 = glm(data = df_part2, acc ~ slab:hoop_size:participant, binomial, offset = off_set)
predicted2 <- plogis(predict(m2, norm_dat %>% 
                               as_tibble() %>%
                               mutate(
                                 off_set = log((1-0.01)/0.01),
                                 hoop_size = tolower(TargetSize),
                                 slab = case_when(TargetSize == "Large" ~ abs(subject_position - large_pos),
                                                  TRUE ~ abs(subject_position - small_pos))
                               ) %>%
                               select(participant, off_set, hoop_size, slab, accuracy)))
optCut2 <- optimalCutoff(norm_dat$accuracy, predicted2)[1]
misClassError(norm_dat$accuracy, predicted2, optCut2)
plotROC(norm_dat$accuracy, predicted2)
plotROC(norm_dat$accuracy, predicted)
