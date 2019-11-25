#### Making figures ####
# For the two hoop size experiment 

#### Library ####
library(tidyverse)

#### Load in data ####
load("scratch/df_part2_norm")

#### functions ####
mu <- function(a, b){
  a/(a + b)
}

phi <- function(a, b){
  1/((a*b)/(((a+b)^2)*(a+b+1)))
}

skew <- function(a, b){
  ((2*(b - a))*sqrt(a + b + 1))/((a + b + 2)*sqrt(a*b))
}

variance <- function(a, b){
  (a*b)/(((a+b)^2)*(a+b+1))
}

# preprocessing 
# check how much is removed by values greater than 1 
norm_dat %>% 
  mutate(outside = ifelse(abs(norm_dist) > 1, 1, 0)) %>% 
  group_by(outside) %>% 
  summarise(n = n()/length(norm_dat$trial))

# filter out trials outside of the range (0.8% of trials)
# sort out some labels for distances
norm_dat %>% 
  group_by(participant) %>% 
  mutate(temp = as.numeric(as.factor(hoop_pos)),
         slab_measures = factor(temp, labels = c("~90%", "~50% - 1", "~50%", "~50% + 1", "~50% + 2", "~10%"))) %>% 
  select(-temp) %>%
  ungroup() %>% 
  filter(abs(norm_dist) <= 1) -> norm_dat



#### Plotting ####
#### box plots ####
# try making distance a factor so it's the same across participants 
# kind of a nice plot... 
plt_dst <- norm_dat %>% 
  # group_by(slab_measures, participant) %>%
  group_by(slab_measures) %>% 
  mutate(mean_dist = mean(norm_dist)) %>%
  ggplot(aes(norm_dist)) + 
  geom_histogram(aes(fill = slab_measures,
                     colour = slab_measures),
                 alpha = 0.3,
                 binwidth = .15) +
  geom_vline(aes(xintercept = mean_dist),
             linetype = "dashed",
             alpha = 0.3) +
  theme_bw() + 
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("Big Hoop", "Centre", "Small Hoop")) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  #facet_wrap(~slab_measures, scales = "free") + 
  facet_wrap(~slab_measures) +
  see::scale_color_flat() + 
  see::scale_fill_flat()
plt_dst

# now do overall instead of by separation 
plt_dst_overall <- norm_dat %>% 
  ggplot(aes(norm_dist)) + 
  geom_histogram() + 
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("Big", "Centre", "Small")) + 
  theme_bw() + 
  theme(axis.title.x = element_blank())
plt_dst_overall

# new plot, looks kind of cool
# mean is a solid line, median is dashed
plt_dst2 <- norm_dat %>% 
  ggplot(aes(x = norm_dist, 
             y = slab_measures,
             colour = slab_measures,
             fill = slab_measures)) + 
  # ggridges::stat_density_ridges(quantile_fun = mean,
  #                               quantile_lines = T,
  #                               alpha = .15,
  #                               linetype = "dashed") + 
                                #jittered_points = T,position = "raincloud") + 
  ggridges::stat_density_ridges(quantile_lines = T,
                                quantiles = 2,
                                alpha = .15) +
  see::scale_color_pizza() +
  see::scale_fill_pizza() +
  see::theme_blackboard() +
  # see::scale_color_flat() + 
  # see::scale_fill_flat() + 
  # theme_bw() +
  scale_x_continuous("Normalised Distance", 
                     limits = c(-1, 1)) + 
  scale_y_discrete("")
plt_dst2$labels$colour <- "Hoop Delta"
plt_dst2$labels$fill <- "Hoop Delta"
plt_dst2 

# try boxplot versions
plt_box <- norm_dat %>% 
  ggplot(aes(y = norm_dist,
             x = slab_measures)) + 
  geom_boxplot(aes(fill = slab_measures,
                   colour = slab_measures),
               alpha = .3) + 
  scale_y_continuous(labels = c("Big", "Centre", "Small"),
                     breaks = c(-1, 0, 1)) +
  theme_bw() + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 
  # see::theme_abyss() +
  # see::scale_color_pizza() + 
  # see::scale_fill_pizza()
plt_box

# save 
ggsave("../../Figures/Experiment_3_Hoop_size/box_position.png",
       width = 5.6,
       height = 3.5)

#### Fitting a distribution ####
# need to rescale things for a beta distribution 
# to fit a beta dist
data_fitting <- norm_dat %>% 
  select(participant, slab_measures, norm_dist) %>% 
  mutate(rescaled = (norm_dist+1)/2,
         rescaled = ifelse(rescaled == 1, 0.9999, rescaled)) %>% 
  group_by(slab_measures) %>% 
  summarise(ests = list(fitdistrplus::fitdist(rescaled,
                                              "beta")$estimate)) %>%
  unnest %>% 
  mutate(param = rep(letters[1:2], length(ests)/2)) %>% 
  spread(key = param, value = ests)

# lines to draw
x_vals <- seq(0,1,.05)
groups <- unique(data_fitting$slab_measures)
a <- data_fitting$a
b <- data_fitting$b

data_lines <- tibble(slab_measures = rep(groups, each = length(x_vals)),
                     a = rep(a, each = length(x_vals)),
                     b = rep(b, each = length(x_vals)),
                     x = rep(x_vals, length(groups)),
                     density = dbeta(x, a, b))

# plot this over the histograms?
norm_dat %>% 
  mutate(rescaled = (norm_dist + 1)/2,
         rescaled = ifelse(rescaled == 1, 0.9999, rescaled)) %>%
  ggplot(aes(rescaled,
             # ggplot(aes(norm_dist,
             fill = slab_measures,
             colour = slab_measures)) +
  geom_segment(aes(x = 0.5, y = 0,
                   xend = 0.5, yend = 4),
               linetype = "dashed") +
  geom_histogram(aes(y = ..density..),
                 binwidth = .1,
                 alpha = .3) + 
  geom_line(data = data_lines, 
            aes(x, density,
            # aes(x, density*31, 
                colour = slab_measures)) + 
  facet_wrap(~slab_measures) + 
  scale_x_continuous(breaks = c(0, .5, 1),
                     labels = c("Big", "Centre", "Small")) + 
  # see::theme_modern() + 
  theme_bw() + 
  theme(legend.position = "none",
  #       axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  geom_text(data = data_lines, 
            aes(1, 3.8,
                label = paste("hat(mu)==", round(mu(a,b), 2))),
            parse = T, hjust = 1,
            size = 3) +
  geom_text(data = data_lines, 
            aes(1, 3.3,
                label = paste("hat(phi)==", round(phi(a,b), 1))),
            parse = T, hjust = 1,
            size = 3) + 
  scale_y_continuous("Density", sec.axis = sec_axis(trans = (~ . * 31), name = "Count")) # + 
  # stat_bin(binwidth= .1, geom="text", aes(label=..count..))

# save 
ggsave("../../Figures/Experiment_3_Hoop_size/histogram_beta_fit.png",
       width = 5.6,
       height = 5)

# look at the mean and precision
data_lines %>%
  group_by(slab_measures) %>%
  summarise(a = mean(a),
            b = mean(b)) %>%
  mutate(mu = mu(a, b),
         phi =  phi(a, b),
         skew = skew(a, b))

# means are all above 0.5 which means they're more likely to move to 
# the small hoop in this setup 

# try a normal?
data_fitting_norm <- norm_dat %>% 
  group_by(slab_measures) %>% 
  summarise(ests = list(MASS::fitdistr(norm_dist,
                                       "normal")$estimate)) %>% 
  unnest %>% 
  mutate(param = rep(c("mu", "sigma"), length(ests)/2)) %>% 
  spread(key = param,
         value = ests)

# sort out data lines 
x_vals <- seq(-1,1,.05)
groups <- unique(data_fitting_norm$slab_measures)
mu <- data_fitting_norm$mu
sigma <- data_fitting_norm$sigma

data_lines_norm <- tibble(slab_measures = rep(groups, each = length(x_vals)),
                          mu = rep(mu, each = length(x_vals)),
                          sigma = rep(sigma, each = length(x_vals)),
                          x = rep(x_vals, length(groups)),
                          density = dnorm(x, mu, sigma))

norm_dat %>% 
  ggplot(aes(norm_dist,
             colour = slab_measures,
             fill = slab_measures)) + 
  geom_histogram(binwidth = .15,
                 alpha = .3,
                 aes(y = ..density..)) + 
  geom_line(data = data_lines_norm,
            aes(x, density)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  facet_wrap(~slab_measures) + 
  geom_text(data = data_lines_norm, 
            aes(1, 2.5,
                label = paste("hat(mu) ==", round(mu, 2))), 
            parse = T, hjust = 1) +
  geom_text(data = data_lines_norm, 
            aes(1, 2,
                label = paste("hat(sigma) ==", round(sigma, 2))), 
            parse = T, hjust = 1)

#### squared error ####
sq_dat <- norm_dat %>% 
  select(participant, hoop_pos, shift, subject_position, slab_measures) %>% 
  mutate(centre = 0,
         error_centre = subject_position^2,
         error_shift = (subject_position-shift)^2)
# box plot  
sq_dat %>% 
  gather(c(error_centre, error_shift),
         key = "Error_type",
         value = "Error") %>% 
  ggplot(aes(slab_measures,
             Error,
             colour = Error_type,
             fill = Error_type)) +
  geom_boxplot(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat()

# do the same again but with normalised data 
sq_dat_norm <- norm_dat %>% 
  select(participant, hoop_pos, shift, subject_position, norm_dist, slab_measures) %>% 
  mutate(centre = 0,
         norm_shift = shift/hoop_pos,
         error_Centre = norm_dist^2,
         error_Shift = (norm_dist - norm_shift)^2)
# make box plot 
sq_dat_norm %>% 
  gather(c(error_Centre, error_Shift), 
         key = "Error_type",
         value = "Error") %>% 
  separate(Error_type,
           c("remove",
             "Error_type"),
           "_") %>% 
  select(-remove) %>%
  ggplot(aes(slab_measures, Error, 
             colour = Error_type,
             fill = Error_type)) + 
  geom_boxplot(alpha = .1) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() 
  

