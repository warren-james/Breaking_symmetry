#### Script to extract Data ####
# 1st year PhD project
# Probability Matching in the detection task
# Difference in this version is that participants are aiming
# to detect 1 of 10 letters rather than whether a dot is
# up or down. 

# This second script is to look at performance in the second half
# of the experiment

#### Notes: ####
# letters are coded as numbers by the matlabd script
# we can change this if we have to but I will keep them as numbers for now
# but for future reference:
# 1 = C, 2 = D, 3 = H, 4 = K, 5 = N, 6 = O, 7 = R, 8 = S, 9 = V, 10 = Z

# fixated box goes from 1:3
# 1 = middle, 2 = left, 3 = right
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# NB: Need to add standardised version this so it's #
#        most likely vs least likely boxes          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# cross_side & target_side
# -1 = left, 1 = right

# condition is 1 or 2
# 1 = 50/50 first, 2 = bias first

# bias
# 1 = left bias, 2 = bias right

# cross_side refers to what side the cross was on... might be useful to look at as this 
# is now some information that people could use even though it has nothing to do with 
# the experiment itself... Though for this we need to keep the output of where 
# people looked intact (fixated_box)

#### libraries needed ####
library(tidyverse)
library(psyphy)
library(lme4)
library(ggthemes)

#### any functions #### 
# To make the proportions plots
prop_plt <- function(dataframe, title, sep_type, lfa){
  # set up the plot
  if(sep_type == "pixels"){
    plt <- ggplot(dataframe,
                  aes(separation,
                      prop))
  } else if(sep_type == "Visual Degrees") {
    plt <- ggplot(dataframe,
                  aes(get_VisDegs(separation/ppcm, Screen_dist),
                      prop))
  }
  # make the title
  plt <- plt +
    ggtitle(paste("Proportion of saccades to each side in the", title, "condition")) +
  # stack areas
    geom_area(aes(colour = prop_boxes,
                  fill = prop_boxes),
              position = "stack")
  # input switch point line
  if(sep_type == "pixels"){
    plt <- plt + geom_vline(aes(xintercept = switch_point),
                            linetype = "dashed")
  } else if(sep_type == "Visual Degrees"){
    plt <- plt + geom_vline(aes(xintercept = get_VisDegs(switch_point/ppcm, Screen_dist)),
                            linetype = "dashed")
  }
  # label axes
  plt <- plt + labs(x = paste("Delta (", sep_type, ")", sep = ""), y = "Proportion of fixations",
                    fill = "Box Fixated", colour = "Box Fixated") 
  # facet_wrap (if there are enough participants)
  if(length(unique(prop_sides$participant))>1){
    plt <- plt + facet_wrap(~as.numeric(participant))
  }
  plt <- plt + theme_minimal() +
    theme(legend.position = "bottom", 
          strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm"))) +
    see::scale_colour_flat() + 
    see::scale_fill_flat() + 
    scale_y_continuous(breaks = c(0, 0.5, 1))
  if(missing(lfa) == FALSE){
  plt <- plt + geom_bar(data = lfa, aes(separation, prop, 
                                        fill = prop_boxes),
                        stat = "identity")
  plt <- plt + scale_x_continuous(breaks = c(2.5, 5, 7.5, 11),
                                  labels = c("2.5", "5", "7.5", "FP"))
  }
  return(plt)
}

# Gets Visual Degrees
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Any constants ####
Screen_dist <- 54.4
ppcm <- 1920/54
  
#### load in the dataset #### 
# create a tibble for dataset 
df <- tibble(
  trial = numeric(),
  block = numeric(),
  separation = numeric(),
  fixated_box = numeric(),
  cross_side = numeric(),
  target_side = numeric(),
  response_letter = numeric(), 
  actual_letter = numeric(),
  bias_left = numeric())

# create column names for the data we have
import_names <- c(
  "trial",
  "block",
  "separation",
  "fixated_box",
  "cross_side",
  "target_side",
  "response_letter",
  "actual_letter",
  "bias_left")

# set path
results_files <- c("data/results/Part_2/")

# temp 
# results_files <- c("75_75_2_2_part2.txt","70_70_1_2_part2.txt")
count <- 1 
# read in each data file
for (f in dir(results_files)){
  d <- read.csv(
    paste(results_files, f, sep=""), header = F)
  
  # change column names
  names(d) <- import_names
  
  # get parts of the string for part no. and session number
  temp <- strsplit(f, '[_]')[[1]]
  
  # now input this information
  # participant
  d$participant <- as.factor(count)
  count <- count + 1
  # condition
  d$condition <- temp[3]
  # bias
  d$bias <- temp[4]
  
  # bind to df
  df <- bind_rows(df, d)
}

# tidy 
rm(d, f, import_names, results_files, temp)

# reorder df
df <- select(df, 
             participant,
             condition,
             bias,
             block,
             trial,
             separation,
             bias_left,
             cross_side,
             target_side,
             fixated_box,
             actual_letter,
             response_letter)

# rename condition and bias levels to mean something 
# condition
df$condition <- ifelse(df$condition == 1, "symmetric first", "bias_first")

# bias
df$bias <- ifelse(df$bias == 1, "left bias", "right bias")

# add in an accuracy column
df$accuracy <- ifelse(df$response_letter == df$actual_letter, 1, 0)

# add in column about bias_type
df$bias_type <- ifelse(df$bias_left == .5, "symmetric", "biased")

# rename for spatial information... 0r make -1,0,1 so it's consistent?
df$lcr <- ifelse(df$fixated_box == 1, 0, 
                ifelse(df$fixated_box == 2, -1, 1))

# get whether they fixated the common or uncommon side
# centre as this stays the same
df$standard_boxes <- "centre"

# right bias
df$standard_boxes[df$fixated_box == 2 & df$bias == "right bias"] <- "least likely"
df$standard_boxes[df$fixated_box == 3 & df$bias == "right bias"] <- "most likely"

# left bias
df$standard_boxes[df$fixated_box == 2 & df$bias == "left bias"] <- "most likely"
df$standard_boxes[df$fixated_box == 3 & df$bias == "left bias"] <- "least likely"

# when should we get rid of NA's?
# people may have seen where the target was and so could update their beliefs about
# which side is more likely?
# for now, remove them here
df <- df[complete.cases(df),]

#### Get switch points data ####
load("scratch/new_data/switch_points")

# add in the extra columns 
df <- merge(df, switch_points)

# make saved version 
df_part2 <- df 
save(df_part2, file = "scratch/new_data/df_part2")

# tidy 
rm(df_part2)

#### Making plots ####
# setup data
prop_sides <- df %>% 
  mutate(lcr = as.factor(lcr),
         prop_boxes = lcr) %>%
  group_by(participant, separation, bias, bias_type, prop_boxes) %>%
  summarise(n = n()) %>%
  complete(prop_boxes, fill = list(n = 0)) %>%
  mutate(prop = n / sum(n))

# use switch_points to get switch points for both bias types 
switch_bias <- select(switch_points, 
                      participant,
                      Eighty_Twenty)
# change colnames
colnames(switch_bias) <- c("participant",
                           "switch_point")
# add bias_type
switch_bias$bias_type <- "biased"

# again for symmetric
switch_sym <- select(switch_points, 
                      participant,
                      Fifty_Fifty)
# change colnames
colnames(switch_sym) <- c("participant",
                           "switch_point")
# add bias_type
switch_sym$bias_type <- "symmetric"

# change back to numeric?
prop_sides$prop_boxes <- as.numeric(prop_sides$prop_boxes)-2

# get separate datasets 
prop_sides_sym <- prop_sides[prop_sides$bias_type == "symmetric",]
prop_sides_bias <- prop_sides[prop_sides$bias_type == "biased",]

# add in switch points 
prop_sides_sym <- merge(prop_sides_sym, switch_sym)
prop_sides_bias <- merge(prop_sides_bias, switch_bias)

# rename levels in prop_boxes for each set 
prop_sides_bias$prop_boxes[prop_sides_bias$prop_boxes == 0] <- "Centre"
# left bias 
prop_sides_bias$prop_boxes[prop_sides_bias$prop_boxes == -1 &
                             prop_sides_bias$bias == "left bias"] <- "Common side"
prop_sides_bias$prop_boxes[prop_sides_bias$prop_boxes == 1 &
                             prop_sides_bias$bias == "left bias"] <- "Uncommon side"

# right bias
prop_sides_bias$prop_boxes[prop_sides_bias$prop_boxes == -1 &
                             prop_sides_bias$bias == "right bias"] <- "Uncommon side"
prop_sides_bias$prop_boxes[prop_sides_bias$prop_boxes == 1 &
                             prop_sides_bias$bias == "right bias"] <- "Common side"

# for symmetric condition we just want left, right, and centre
prop_sides_sym$prop_boxes[prop_sides_sym$prop_boxes == 0] <- "Centre"
prop_sides_sym$prop_boxes[prop_sides_sym$prop_boxes == 1] <- "Right"
prop_sides_sym$prop_boxes[prop_sides_sym$prop_boxes == -1] <- "Left"


# tidy 
rm(switch_bias, switch_sym)

# can edit prop_boxes to be appropriately named now as well...

# make plot(s)
# symmetric plt
# prop_plt(prop_sides_symmetric, "symmetric", "pixels")
# ggsave("scratch/plots/Part_2_prop_symmetric_pixels.pdf", height = 10, width = 10)

prop_plt(prop_sides_sym, "symmetric", "Visual Degrees")
# ggsave("../../Figures/Experiment_4_Prob/Part_2_prop_symmetric_vdegs.png",
#        height = 12,
#        width = 18,
#        units = "cm")

# bias plt 
# prop_plt(prop_sides_bias, "biased", "pixels")
# ggsave("scratch/plots/Part_2_prop_biased_pixels.pdf")

# prop_plt(prop_sides_bias, "biased", "Visual Degrees")
# ggsave("../../Figures/Experiment_4_Prob/Part_2_prop_biased_vdegs.png",
#        height = 12,
#        width = 18,
#        units = "cm")

#### Same plots as above, remove lfa ####
prop_sides_sym %>% 
  filter(separation != 640) %>%
  prop_plt("symmetric", "Visual Degrees")

prop_sides_bias %>% 
  filter(separation != 640) %>%
  prop_plt("bias", "Visual Degrees")

#### new plot with stacked bar ####
# this needs to be max value  +1
prop_sides_lfa_sym <- prop_sides_sym %>%
  filter(separation == 640) %>% 
  mutate(separation = 11)

prop_sides_lfa_bias <- prop_sides_bias %>%
  filter(separation == 640) %>% 
  mutate(separation = 11)

prop_sides_sym %>% 
  filter(separation != 640) %>%
  prop_plt("Symmetrical", "Visual Degrees", prop_sides_lfa_sym)

# save
ggsave("../../Figures/Experiment_4_Prob/Part_2_wbar_sym.png",
       height = 12,
       width = 18,
       units = "cm")

prop_sides_bias %>% 
  filter(separation != 640) %>%
  prop_plt("Bias", "Visual Degrees", prop_sides_lfa_bias)

# save
ggsave("../../Figures/Experiment_4_Prob/Part_2_wbar_bias.png",
       height = 12,
       width = 18,
       units = "cm")




#### Make plots of just centre vs side by condition ####
# add in centre vs side column 
prop_sides$cen_sid <- ifelse(prop_sides$prop_boxes == 1, "Side", "Centre")

# get summary 
cen_side <- prop_sides %>%
  group_by(participant, separation, bias_type, cen_sid) %>%
  summarise(n = sum(n)) %>%
  mutate(prop = n / sum(n))

cen_side <- merge(cen_side, switch_points)

# make the plots like the rest
centre <- cen_side[cen_side$cen_sid == "Centre",]
side <- cen_side[cen_side$cen_sid == "Side",]

dot_plt <- side %>% 
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             prop,
             colour = bias_type)) + 
  geom_point(aes(shape = bias_type)) + 
  scale_shape_manual(values = c(3, 4)) + 
  geom_vline(aes(xintercept = get_VisDegs(Eighty_Twenty/ppcm, Screen_dist)),
             linetype = "dashed",
             colour ="blue" ) + 
  geom_vline(aes(xintercept = get_VisDegs(Fifty_Fifty/ppcm, Screen_dist)),
             linetype = "dashed",
             colour ="red" ) + 
  facet_wrap(~participant) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  see::scale_color_flat() 
dot_plt <- dot_plt + theme(legend.position="bottom")
dot_plt$labels$y <- "Proportion of Fixations to the side boxes"
dot_plt$labels$x <- "Delta (Visual Degrees)"
dot_plt$labels$colour <- "Bias Type"
dot_plt$labels$shape <- "Bias Type"
dot_plt
# save this 
# ggsave("scratch/plots/SideVSCentre.pdf", height = 10, width = 10)

#### Get bias to one side ####
# so make it so 0 means no bias (both equal) and one means exclusively one or the other?
df_just_side <- df[df$lcr != 0,]

bias_sides <- df_just_side %>%
  group_by(participant, bias_type, lcr) %>% 
  summarise(num = n()) %>%
  mutate(prop = num/sum(num))

bias_sides_2 <- bias_sides[-c(4)] %>%
  group_by(participant) %>%
  spread(lcr, prop)

bias_sides_2[is.na(bias_sides_2)] <- 0 


# these give the exact same results
bias_sides_2$score <- abs(bias_sides_2$`1` - bias_sides_2$`-1`)
bias_sides_2$highest <- pmax(bias_sides_2$`1`, bias_sides_2$`-1`)
bias_sides_2$lowest <- pmin(bias_sides_2$`1`, bias_sides_2$`-1`)

# make it a factor
bias_sides_2$participant <- as.factor(bias_sides_2$participant)
bias_sides_2$bias_type <- as.factor(bias_sides_2$bias_type)

# really simple t test of these results
ttest <- t.test(bias_sides_2$score~bias_sides_2$bias_type, paired = T)
ttest

# fix it for proportions...
bias_sides_3 <- bias_sides[-c(5)] %>%
  group_by(participant) %>%
  spread(lcr, num)

bias_sides_3[is.na(bias_sides_3)] <- 0

bias_sides_3$highest <- pmax(bias_sides_3$`1`, bias_sides_3$`-1`)
bias_sides_3$lowest <- pmin(bias_sides_3$`1`, bias_sides_3$`-1`)

temp_data <- matrix(c(mean(bias_sides_3$highest[bias_sides_3$bias_type == "biased"]),
                      mean(bias_sides_3$highest[bias_sides_3$bias_type == "symmetric"]),
                      mean(bias_sides_3$lowest[bias_sides_3$bias_type == "biased"]),
                      mean(bias_sides_3$lowest[bias_sides_3$bias_type == "symmetric"])),
                      ncol = 2)

colnames(temp_data) <- c("highest", "lowest")
rownames(temp_data) <- c("biased", "symmetric")

# all these show that they are significant... Maybe
# ask Alasdair about it?
prop.test(temp_data)

chisq.test(temp_data)

mcnemar.test(temp_data)

# temp <- glm(highest ~ bias_type,
#             data = bias_sides_2, 
#             family = binomial())

# this is significant so get some descriptives 
descs_bias <- bias_sides_2 %>%
  group_by(bias_type) %>%
  summarise(meanshigh = mean(highest),
            sdshigh = sd(highest),
            meanscore = mean(score),
            sdscore = sd(score))

# remove person 5 to test this again. 
desc_minus5 <- bias_sides_2[bias_sides_2$participant != "5",] %>%
  group_by(bias_type) %>%
  summarise(means = mean(highest))

#### Some GLM stuff ####
# Just so we have some numbers to write in... even though 
# it's not really the best idea to do this 
# model with no condition/bias_type
m1 <- glm(prop ~ separation, 
          data = side, 
          family = binomial())
summary(m1)

m2 <- glm(prop ~ separation + as.factor(bias_type),
          data = side,
          family = binomial())
summary(m2)

# Neither of these is significant which suggests that on average participants did not
# modify their fixations with relation to separation and there was no improvement 
# when adding in condition... (this is easily told by looking at the plots though)

# what about just condition?
m3 <- glm(prop ~ as.factor(bias_type),
          data = side,
          family = binomial)
summary(m3)

# We could also try looking at whether people were sensitive to the bias side 
# Maybe by getting a value for how much more often they looked at the "biased" 
# side in both conditions?

#### NEW GLMR stuff ####
# spoke to Alasdair and he thinks this is the best approach 
# need a subset of the data 
glm_dat <- select(df_just_side,
                  participant,
                  bias,
                  block,
                  trial,
                  separation,
                  cross_side,
                  bias_type,
                  lcr,
                  standard_boxes)

# add in some columns for fixated left being 0 or 1 and bias box being left 
glm_dat$fixated_left <- 0
glm_dat$fixated_left[glm_dat$lcr == -1] <- 1

glm_dat$fixated_common <- 0
glm_dat$fixated_common[glm_dat$standard_boxes == "most likely"] <- 1

glm_dat$bias_box_left <- 0
glm_dat$bias_box_left[glm_dat$bias == "left bias"] <- 1

# make bias_type a factor
glm_dat$bias_type <- as.factor(glm_dat$bias_type)
glm_dat$bias_box_left <- as.factor(glm_dat$bias_box_left)


# run model 
m4 <- glmer(fixated_common ~ bias_type + (1|participant),
            data = glm_dat, 
            family = binomial())
summary(m4)
# this is significant

# add symmetric slopes by condition
m4.1 <- glmer(fixated_common ~ bias_type + (1 + bias_type|participant),
              data = glm_dat,
              family = binomial())
summary(m4.1)



# just to satisfy some curiosity
m5 <- glmer(fixated_common ~ bias_type + bias_box_left + (1|participant),
            data = glm_dat,
            family = binomial())
summary(m5)
# nope, doesn't add anything

# what about fixation cross side...
m6 <- glmer(fixated_common ~ bias_type + cross_side + (1|participant),
            data = glm_dat,
            family = binomial())
summary(m6)

# maybe add in the interaction as it should only be predictive for th 50-50
m7 <- glmer(fixated_common ~ (bias_type + cross_side)^2 + (1|participant),
            data = glm_dat,
            family = binomial())
summary(m7)











#### Simulations of biases ####
# Not sure if this is needed... but it's cool to look at
# To simulate what it should look like to check if the bias was put into 
# the experiment well or not
# setup biases 
biases <- c(0.8,0.2,0.5)

sim_results <- tibble(bias = numeric(),
                      average = numeric())

names <- c("bias",
           "average")

for(i in biases){
  for(x in 1:10000){
    average <- mean(sample(c(0,1), size = 640, replace = T, prob = c(i, 1-i)))
    sim_results <- rbind(sim_results, data.frame(bias = i,
                                                 average = average))
  }
}

# tidy 
rm(average, biases, i, x, names)

# now make a plot of this and we can get the information from each participant to 
# compare to these plots 
# make bias a factor 
sim_results$bias <- as.factor(sim_results$bias)

plt_sim <- ggplot(sim_results, aes(average, colour = bias))
plt_sim <- plt_sim + geom_density(alpha = 0.2)




