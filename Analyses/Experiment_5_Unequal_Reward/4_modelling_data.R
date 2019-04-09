#### Gambling Study - Doing some analysis ####
# Big aim:
# Make a STAN model of standing positiont for each person 
# and look for difference in standing position by Delta and gamble type 

# Could also maybe use a constant value of prop Unequal gambles?

#### Libraries ####
library(tidyverse)
library(rstan)
library(brms)

#### Load data ####
load("scratch/data/df_part2")
load("scratch/data/df_part1")

#### get accuracy cuvre data ####
# setup empty frame 
acc_dat <- data.frame(Participant = character(),
                      HoopDelta = numeric(),
                      Est_Accuracy = numeric())

# separations we want 
separations <- c(0:30)

# run loop
for(P in levels(df_part1$Participant)) {
  # get subset
  ss <- df_part1[df_part1$Participant == P,]
  
  # run glm for each participant 
  m <- glm(data = ss,
           Accuracy ~ Slab,
           family = binomial)
  
  # get predictions
  p <- predict(m, data.frame(Slab = separations), type = "response")
  p <- as.numeric(p)
  
  # add into data frame
  acc_dat <- rbind(acc_dat, data.frame(Participant = P,
                                       HoopDelta = separations,
                                       Est_Accuracy = p))
}


#### sort data for model ####
# add in accuracy data to df_part2 
df_part2 <- merge(df_part2, acc_dat)

# Scale Delta so 0 is closest hoop and 1 is farthest 
# This might make the scaling weird though?
# Could make this scale based on estimated accuracy for that target?
# remove any norm_dist > 1 
# Also, Norm_dist needs to be offset so 0<Norm_Dist<1
model_data <- df_part2 %>%
  group_by(Participant) %>%
  mutate(max_delta = max(HoopDelta),
         Norm_Delta = HoopDelta/max_delta,
         Unequal = as.numeric(as.factor(Gamble_Type))-1) %>%
  filter(Norm_Dist <= 1) %>%
  mutate(Norm_Dist = (Norm_Dist + 1e-4)/(1+ 2e-4)) %>%
  select(Participant, Norm_Delta, Est_Accuracy, Gamble_Type, Norm_Dist, Unequal) %>%
  mutate(med_dist = median(Norm_Delta))

# add a dist_type predictor
model_data$dist_type <- "close"
model_data$dist_type[model_data$Norm_Delta > model_data$med_dist] <- "far"

# deselect med_dist 
model_data <- model_data %>% 
  select(-med_dist)

# save this 
save(model_data, file = "scratch/data/model_data")

#### Models ####
#### m1 - Norm_Dist ~ Norm_Delta ####
# setup stan_df
stan_df <- list(
  N = nrow(model_data),
  Norm_Dist = model_data$Norm_Dist,
  Delta = model_data$Norm_Delta
)

# run model 
# this model is really bad at the moment... need to sort out how to actually do a 
# beta distribution in stan... 
m1 <- stan(
  file = "scratch/models/m1.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save this atrocious model 
save(m1, file = "scratch/model_outputs/m1_ouput")


#### m2 - Norm_Dist ~ Norm_Delta + Gamble_type ####
# same model as before... but add in gamble type 
stan_df <- list(
  N = nrow(model_data),
  Norm_Dist = model_data$Norm_Dist,
  Unequal = model_data$Unequal,
  Delta = model_data$Norm_Delta
)

# run model 
# this model is really bad at the moment... need to sort out how to actually do a 
# beta distribution in stan... 
m2 <- stan(
  file = "scratch/models/m2.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save this atrocious model 
save(m2, file = "scratch/model_outputs/m2_ouput")


#### m3 - Norm_Dist ~ (Norm_Delta + Unequal)^2 ####
# add in the interactions... Again, still a dumb model with normal dist 
stan_df <- list(
  N = nrow(model_data),
  Norm_Dist = model_data$Norm_Dist,
  Unequal = model_data$Unequal,
  Delta = model_data$Norm_Delta
)

# run model 
# this model is really bad at the moment... need to sort out how to actually do a 
# beta distribution in stan... 
m3 <- stan(
  file = "scratch/models/m3.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save this atrocious model 
save(m3, file = "scratch/model_outputs/m3_ouput")


#### m4 - Norm_Dist ~ (Norm_Delta + Unequal)^2 w/ random intercepts per Participant ####

#### m5 - Norm_Dist ~ (Norm_Delta + Unequal)^2 w/ random ints and slopes per Participant ####

#### trying with beta dist ####
# Not sure if this works the way I think... should check this out though
stan_df <- list(
  N = nrow(model_data),
  Norm_Dist = model_data$Norm_Dist,
  Unequal = model_data$Unequal,
  Delta = model_data$Norm_Delta
)

m6 <- stan(
  file = "scratch/models/m6.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save 
save(m6, file = "scratch/model_outputs/m6_output")

# tried adding intercepts... I think anyway... 
m6.1 <- stan(
  file = "scratch/models/m6_1.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

save(m6.1, file = "scratch/model_outputs/m6.1_output")



#### Try some BRMS models ####
# brms model 
m_brms <- brm(Norm_Dist ~ Norm_Delta*Gamble_Type,
              data = model_data, family = "beta",
              iter = 4000,
              chains = 2,
              cores = 2)

# save this 
save(m_brms, file = "scratch/model_outputs/m_brms")


# same again with dist_type 
m_brms_v2 <- brm(Norm_Dist ~ dist_type*Gamble_Type,
                 data = model_data, family = "beta",
                 iter = 2000,
                 chains = 1,
                 cores = 1)

# save
save(m_brms_v2, file = "scratch/model_outputs/m_brms_v2")
