#### Script to extract Data ####
# 1st year PhD project
# Probability Matching in the detection task
# Difference in this version is that participants are aiming to detect 
# 1 of 10 letters rather than whether a dot is up or down. 
# This script is to look at the *part 1* (testing visual accuity)

#### Notes: ####
# letters are coded as numbers by the matlabd script
# we can change this if we have to but I will keep them as numbers for now
# but for future reference:
# 1 = C, 2 = D, 3 = H, 4 = K, 5 = N, 6 = O, 7 = R, 8 = S, 9 = V, 10 = Z

# pix per degree set to 35.561 or 36 rounded
# distances used based on Clarke and Hunt (2016)
# Josephine's version 100,200,250,290,330,370,410,455


#### libraries needed ####
library(tidyverse)
library(psyphy)

#### Any functions ####
# Gets Visual Degrees
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Constants ####
Screen_dist <- 54.4
ppcm <- 1920/54

#### load in the dataset #### 
# create a tibble for dataset 
df <- tibble(
  block = numeric(),
  separation = numeric(), 
  response_letter = numeric(), 
  actual_letter = numeric(),
  accuracy = numeric())
 
# create column names for the data we have
import_names <- c(
  "block",
  "separation",
  "response_letter",
  "actual_letter",
  "accuracy")

# set up directory for loop 
results_files <- dir("data/results/Part_1/")

#results_files <- c("75_75_part1.txt")
count <- 1
# read in each data file
for (f in results_files){
  d <- read.csv(
    paste("data/results/Part_1/", f, sep=""), header = F)
  
  # change column names
  names(d) <- import_names
  
  # get parts of the string for part no. and session number
  temp <- strsplit(f, '[_]')[[1]]
  
  # now input this information
  d$participant <- as.factor(count)
  count <- count + 1
  
  d$part <- substring(temp[3],5,5)
  
  # bind to df
  df <- bind_rows(df, d)
}

# tidy
rm(temp, f, d, import_names, results_files)

# re-order the dataset 
df <- select(df,
             participant,
             part,
             block,
             separation,
             response_letter,
             actual_letter,
             accuracy)

# add in accuracy column 
#df$accuracy <- 0
#df$accuracy[df$response_letter == df$actual_letter] <- 1

# save processed data file
save(df, file = "scratch/new_data/Part_1_data")

# remove Na
df <- df[complete.cases(df),]

# save with Na removed 
save(df, file = "scratch/new_data/Part_1_data_nar")

#### Plot: just to check ####
plt <- df %>% 
  group_by(participant, separation) %>% 
  summarise(accuracy = mean(accuracy)) %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist), accuracy)) + 
  geom_point() + 
  geom_smooth(method = glm,
              method.args = list(family = binomial(mafc.logit(10))),
              se = F, fullrange = T) + 
  facet_wrap(~participant) + 
  theme_bw() + 
  theme(strip.text.x = element_blank())
plt$labels$x <- "Delta (Visual Degrees)"
plt$labels$y <- "Accuracy"
plt

ggsave("../../Figures/Experiment_4_Prob/Part_1_curves.png",
       height = 3.5,
       width = 5.6)

#### Get switch points ####
df_acc <- df
m = glm(data=df, accuracy~separation:participant,
        family= binomial(mafc.logit(10))) # for one participant
        #:participant, family=binomial) # for more than one
df_acc$p = predict(m, type = "response")


#### Extract switching points ####
# Should compare these to Josephine's calculations as well 
# Remember, for this experiment, we need two switch points
# 1) 55% for the 50-50 condition 
# 2) 82% for the 80-20 condition
# Also, check the 75% to compare to Josephine's to see if we get the same

# set distances to test 
sep <- c(64:1000,1280)

# data frame for accuracy accross separations
acc_sep <- tibble(
  participant = numeric(),
  separation = numeric(),
  accuracy = numeric()
)

# might need one for their two switch points?
switch_points <- tibble(
  participant = numeric(),
  Fifty_Fifty = numeric(),
  Eighty_Twenty = numeric(),
  Switching_point = numeric()
)

#loop
for (p in unique(df_acc$participant)){
  # general linear model
  ss = df_acc[df_acc$participant == p,]
  m = glm(data = ss, accuracy~separation,
          family = binomial(mafc.logit(10)))
  for(i in sep){
    y = predict(m, data.frame(separation = i), type = "response")[1]

    # add into new data frame
    acc_sep <- rbind(acc_sep, data.frame(participant = p,
                                         separation  = i,
                                         accuracy = y))
  }
  # get subset of acc_sep
  temp <- acc_sep[acc_sep$participant == p,]
  
  # Extract switch points 
  F_F = temp$separation[max(which(temp$accuracy > 0.55))]
  E_T = temp$separation[max(which(temp$accuracy > 0.82))]
  SP = temp$separation[max(which(temp$accuracy > 0.685))]
  
  
  # Add into dataframe
  switch_points <- rbind(switch_points, data.frame(participant = p,
                                                   Fifty_Fifty = F_F,
                                                   Eighty_Twenty = E_T,
                                                   Switching_point = SP))
}

# tidy 
rm(temp, F_F, E_T, SP, p, ss, m, i, y, sep)

# save the switching points 
save(switch_points, file = "scratch/new_data/switch_points")
save(acc_sep, file = "scratch/new_data/acc_sep")

# tidy
rm(list = ls())
