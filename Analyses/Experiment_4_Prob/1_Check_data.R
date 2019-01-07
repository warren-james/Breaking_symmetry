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

# participant 91 was using white letters that were 0.3VD and this didn't
# drop performance to chance levels so may need to go smaller or lower contrast
# maybe a lower contrast as that's what was in the original paper
# participant 90 grey letters(grey2 in stimuli) and same VD
# 89 used grey3
# 88 used grey3 with 0.5VD
# 87 used grey3 with 0.4VD
# 86 used grey3 with 0.35VD **** This seems like the best one ****
# 80 & 79 & 78 & 77 tests so don't worry 

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

# read in each data file
for (f in results_files){
  d <- read.csv(
    paste("data/results/Part_1/", f, sep=""), header = F)
  
  # change column names
  names(d) <- import_names
  
  # get parts of the string for part no. and session number
  temp <- strsplit(f, '[_]')[[1]]
  
  # now input this information
  d$participant <- temp[2]
  
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
# setup dataframe 
temp <- group_by(df, participant, separation)
plt_dat <- summarise(temp, mean_acc = mean(accuracy))

# tidy
rm(temp)

# make the plot
plt <- ggplot(plt_dat, aes(separation, mean_acc)) + geom_point()
# Just to check if there's enough people to facet_wrap
if(length(unique(df$participant)) > 1){
  plt <- plt + facet_wrap(~participant)
}
plt

rm(plt, plt_dat)

#### Get switch points ####
# Try using accuracy column?
# This way works as well... so we should be able to do this in matlab as well?
# gets the exact same results anyway
df_acc <- df
m = glm(data=df, accuracy~separation:participant,
        family= binomial(mafc.logit(10))) # for one participant
        #:participant, family=binomial) # for more than one
df_acc$p = predict(m, type = "response")

# get summary for the plot
plot_dat <- group_by(df_acc, participant, separation)
plot_dat <- summarise(plot_dat, accuracy = mean(accuracy),
                      p = mean(p))


# make plot 
plt <- ggplot(plot_dat, aes(separation, accuracy))
plt <- plt + geom_point()
plt <- plt + geom_smooth(method=glm,
                         method.args = list(family = "binomial"), 
                         aes(y=p),
                         fullrange=T, se=F)
if(length(unique(df_acc$participant)) > 1){
  plt <- plt + facet_wrap(~participant)
}
plt

# first set up the dataset (similar to beanbag study)
df_va <- group_by(df, participant, separation)
df_va <- summarise(df_va, trials = length(accuracy),
                   num_correct = sum(accuracy),
                   accuracy = sum(accuracy)/length(accuracy))

# set the offset 
e <- 0.01
df_va$off_set = log((1-e)/e)

# tidy 
rm(e)

# get predictions
m = glm(data = df_va, cbind(num_correct, trials-num_correct)~
          separation:participant, family = binomial(mafc.logit(10)))
df_va$p = predict(m, type = "response")

# tidy 
rm(m)

# tidy data 
plot_dat2 <- select(df_va,
                    participant, 
                    separation, 
                    accuracy,
                    p)

# make plot 
plt2 <- ggplot(plot_dat2, aes(separation, accuracy))
plt2 <- plt2 + geom_point()
plt2 <- plt2 + geom_smooth(method=glm,
                           method.args = list(family = "binomial"), 
                           aes(y=p),
                           fullrange=T, se=F)
if(length(unique(df_va$participant)) > 1){
  plt2 <- plt2 + facet_wrap(~participant)
}
plt2
# write a save part here
# ggsave("scratch/plots/Part_1_Plots.pdf", height = 10, width = 10)

# Make version with VD
plt3 <- ggplot(plot_dat2, aes(get_VisDegs(separation/ppcm, Screen_dist), accuracy))
plt3 <- plt3 + geom_point()
plt3 <- plt3 + theme_bw()
plt3 <- plt3 + geom_smooth(method = glm,
                           method.args = list(family = "binomial"),
                           aes(y = p), 
                           fullrange = T, se = F)
if(length(unique(df_va$participant)) > 1){
  plt3 <- plt3 + facet_wrap(~participant)
}
plt3 <- plt3 + labs(x = "Delta (in Visual Degrees)",
                    y = "Accuracy")
plt3
# ggsave("scratch/plots/Part_1_Plots_VD.pdf", height = 10, width = 10)


#### Extract switching points ####
# Should compare these to Josephine's calculations as well 
# Remember, for this experiment, we need two switch points
# 1) 55% for the 50-50 condition 
# 2) 82% for the 80-20 condition
# Also, check the 75% to compare to Josephine's to see if we get the same

# set distances to test 
sep <- c(64:1000,1280)
# unused at the moment

# part 2?
# sep <- c(103,203,252,295,334,373,416,455,500,640)

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
for (p in unique(df_va$participant))
{
  # general linear model
  ss = df_va[which(df_va$participant==p),]
  m = glm(data = ss, cbind(num_correct, trials-num_correct)~
            separation,
          family = binomial(mafc.logit(10)),
          offset = ss$off_set)
  for(i in c(1:1000,1280)){
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
