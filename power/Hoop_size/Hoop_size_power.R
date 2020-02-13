#### Hoop size power ####
# look at how much variation there is and then use this to
# see how small a difference we could detect given various N's 

# check this link to increase the speed of the loops

#### Library ####
library(tidyverse)
library(tidybayes)

#### Functions ####
# squash range
squash <- function(y, max, min, squash){
  y <- y * ((max-squash) - (min + squash)) + (min + squash)
  return(y)
}

# functions for getting parameters from a beta dist
mu_beta <- function(a, b){
  a/(a + b)
}
phi_beta <- function(a, b){
  1/((a*b)/(((a+b)^2)*(a+b+1)))
}
var_beta <- function(a, b){
  (a*b)/(((a+b)^2)*(a+b+1))
}

# get shape parameters 
get_shape_beta <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# need to get the reverse of this 
# so specify a and b given mu and phi 
# That way I can make adjustments to mu keeping phi the same 
# and then look at how much "power" we have to detect a difference 
# of size D


#### read in data ####
results_files <- c("data/Part2/")

df_part2 <- tibble()
# loop through 
for(f in dir(results_files)){
  d <- read.csv(paste(results_files, f, sep = ""), sep = "\t")
  subj <- strsplit(f, 'results.txt')[[1]][1]
  d$Participant <- subj
  
  # bind 
  df_part2 <- rbind(df_part2, d)
}

# get hoop pos 
df_hoop_pos <- read.csv("data/Hoop_positions/part2hoopPos.txt", sep = "\t") %>% 
  mutate(Color = ifelse(Hoop == "Red", "R", ifelse(Hoop == "Yellow", "Y", "B"))) %>% 
  select(-Hoop)

# merge them 
df_part2 <- merge(df_part2, df_hoop_pos)

# tidy
rm(d, f, results_files, subj, df_hoop_pos)

#### preprocessing ####
df_part2 <- df_part2 %>% 
  group_by(Participant) %>%
  mutate(norm_pos = (SubjectPosition - 20)/Delta,
         temp = as.numeric(as.factor(Delta)),
         slab_measures = factor(temp, labels = c("~90%", "~50% - 1", "~50%", "~50% + 1", "~50% + 2", "~10%"))) %>% 
  select(-temp) %>% 
  filter(abs(norm_pos) < 1.01) %>%
  mutate(beta_pos = (norm_pos + 1)/2,
         beta_pos = squash(beta_pos, 1, 0, 1e-4)) %>%
  ungroup()


#### make some plots ####
# overall 
df_part2 %>% 
  summarise(mu = mean(norm_pos), 
            sdev = sd(norm_pos))

# get some descriptives 
df_part2 %>% 
  group_by(slab_measures) %>% 
  summarise(mu = mean(norm_pos), 
            sdev = sd(norm_pos))

df_part2 %>% 
  ggplot(aes(norm_pos, 
             colour = slab_measures, 
             fill = slab_measures)) + 
  geom_histogram(aes(y = ..density..),
                 alpha = .3, 
                 binwidth = .01) +
  geom_density(alpha = .2) +
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  facet_wrap(~slab_measures)

# overall version 
df_part2 %>% 
  ggplot(aes(beta_pos)) + 
  geom_histogram(aes(y = ..density..),
                 alpha = .3,
                 binwidth = 0.05) + 
  geom_density() 


#### try something else ####
# try by obtaining parameters to fit a series of beta dists and then sample from these
# because this is what we're doing with the real data 
shape_1 <- fitdistrplus::fitdist(df_part2$beta_pos, "beta")

mu1 <- mu_beta(shape_1$estimate[1], shape_1$estimate[2])
var1 <- var_beta(shape_1$estimate[1], shape_1$estimate[2])
# mu1 <- mean(df_part2$beta_pos)
# var1 <- var(df_part2$beta_pos)
shape_1 <- get_shape_beta(mu1, var1)
mu2 <- seq(0.05, .2, 0.05)
x <- seq(0,1,0.01)

# quick plotting 
plt_check_dists <- tibble(x_vals = rep(x, length(c(mu2, 0))),
                          diff = rep(c(mu2, 0), each = length(x)),
                          base = mu1, 
                          var = var1,
                          alpha = get_shape_beta(base + diff, var)$alpha,
                          beta = get_shape_beta(base + diff, var)$beta,
                          p = dbeta(x_vals, alpha, beta)) %>% 
  ggplot(aes(x_vals, p,
             colour = as.factor(diff))) + 
  geom_point() +
  theme_bw()
plt_check_dists

# looks alright... 
# now loop through these distributions taking samples and see how the mean looks

# setup parameters 
n_iter <- 5000
n_subs <- seq(3, 24, 1)
n_trials <- 72 
refresh <- n_iter/100

# changing this to a data.table for speed 
# need to establish the data table filled with 0's 
# so how long? 
n <- n_iter * length(n_subs) * length(mu2) * length(n_trials)
# df_sample <- data.table::data.table(iter = rep(0, n),
#                                     n_subs = rep(0, n),
#                                     n_trials = rep(0, n),
#                                     difference = rep(0, n),
#                                     baseline = rep(0, n),
#                                     comparison = rep(0, n))
# # setup df 
# # df_sample <- tibble(iter = numeric(),
# # n_subs = numeric(),
# # n_trials = numeric(),
# # difference = numeric(),
# # baseline = numeric(),
# # comparison = numeric())
# 
# start <- Sys.time()
# # loop 
# count <- 0
# for(ii in 1:n_iter){
#   if(ii %% refresh == 0){
#     print(paste((ii/n_iter)*100, "%", sep = ""))
#   }
#   base_shape <- get_shape_beta(mu1, var1)
#   base_alpha <- base_shape$alpha
#   base_beta <- base_shape$beta
#   for(trials in n_trials){
#     for(diff in mu2){
#       comp_shape <- get_shape_beta(mu1 + diff, var1)
#       comp_alpha <- comp_shape$alpha
#       comp_beta <- comp_shape$beta
#       for(sub in n_subs){
#         # up count here 
#         count <- count + 1
#         base_sample <- c()
#         comp_sample <- c()
#         for(s in 1:sub){
#           base_sample <- c(base_sample, rbeta(n_trials, base_alpha, base_beta))
#           comp_sample <- c(comp_sample, rbeta(n_trials, comp_alpha, comp_beta))
#         }
#         # df_sample <- rbind(df_sample, tibble(iter = ii,
#         #                                      n_subs = sub,
#         #                                      n_trials = trials,
#         #                                      difference = diff,
#         #                                      baseline = mean(base_sample),
#         #                                      comparison = mean(comp_sample)))
#         # add to table 
#         df_sample[count, iter := ii]
#         df_sample[count, n_subs := sub]
#         df_sample[count, n_trials := trials]
#         df_sample[count, difference := diff]
#         df_sample[count, baseline := mean(base_sample)]
#         df_sample[count, comparison := mean(comp_sample)]
#       }
#     }
#   }
# }
# ending <- Sys.time()
# ending - start


# save this 
# save(df_sample, file = "scratch/sampled_data")
load("scratch/sampled_data")

# convert to a tibble
df_sample <- as_tibble(df_sample)
# plot this 
plt_samples <- df_sample %>%
  filter(difference == 0.05) %>%
  gather(baseline:comparison,
         key = "Mu_type",
         value = "value") %>% 
  ggplot(aes(value, 
             colour = Mu_type,
             fill = Mu_type)) + 
  geom_density(alpha = .3) + 
  theme_bw() +
  facet_grid(difference~n_subs)
plt_samples

# plot together
gridExtra::grid.arrange(plt_check_dists, plt_samples)

# Do hdi stuff 
plt_hdi <- df_sample %>% 
  filter(difference == 0.05) %>% 
  mutate(diff = comparison - baseline) %>% 
  group_by(n_subs) %>% 
  summarise(mu_diff = mean(diff),
            HDI_lower = hdi(diff)[1],
            HDI_upper = hdi(diff)[2]) %>% 
  ggplot(aes(n_subs, mu_diff)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = HDI_lower, 
                  ymax = HDI_upper),
              alpha = .3)
plt_hdi






