#### Awareness and bean bags ####
# Updated version of my level 4 project 
# This script is to extract and look at the part 1 data 
#
# Written by Warren James

#### Notes ####
# For the Direction Column, 1 correpsonds with North and 2 with South
# Positive numbers are towards the south, and negative towards the North

#### set options ####
options(digits = 4)

#### load in libraries #### 
library(tidyverse)
library(psyphy) # might be needed 

#### Any constants ####
Hoop_size <- 0.46

#### Any functions ####

#### read in data ####
# create dataset
df <- read_csv("data/Part2Measures.csv")

#### sort data ####
# reorder levels
# df$Colour <- factor(df$Colour,
#                     levels(df$Colour)[c(1,3,2)])
# This bit doesn't work for tibbles

# rename headers 
colnames(df) <- c("Participant",
                  "Trial",
                  "Direction",
                  "Colour",
                  "Position",
                  "Accuracy",
                  "R",
                  "Y",
                  "B")


# add in hoop_position 
df$Hoop_Pos <- 0

# Make sure this is a number for later
for (row in 1:nrow(df))
{
  df$Hoop_Pos[row] = as.numeric(df[row,as.character(df$Colour[row])])
}

# tidy 
rm(row)

# Sort the North and South Hoop_Pos 
df$North_Pos <- df$Hoop_Pos*-1
df$South_Pos <- df$Hoop_Pos

# relevel Participant column
df$Participant <- factor(df$Participant, levels = c("2","4","6","8",
                                                    "10","12","14",
                                                    "16","18","20",
                                                    "22","24","1",
                                                    "3","5","7","9",
                                                    "11","13","15",
                                                    "17","19","21",
                                                    "23"))
levels(df$Participant) = c("A1", "A2", "A3", "A4", "A5", "A6", "A7",
                           "A8", "A9", "A10", "A11", "A12", "B1",
                           "B2", "B3", "B4", "B5", "B6", "B7", "B8",
                           "B9", "B10", "B11", "B12")

#### Load in data from part 1 ####
load("scratch/dataframes/acc_sep")
load("scratch/dataframes/slabs_to_test")

# do some renaming 
colnames(slabs_to_test) <- c("Participant",
                             "Actual_Switch_Slab")

colnames(acc_sep) <- c("Participant",
                       "Distance",
                       "Expected_Accuracy")

#### Look at Participants Estimates ####
# Load in the data 
est_dat <- read_csv("data/self_acc.csv")

# add in trials 
est_dat$trials <- 12

# relevel participants so they match 
est_dat$Participant <- factor(est_dat$Participant, levels = c("2","4","6","8",
                                                              "10","12","14",
                                                              "16","18","20",
                                                              "22","24","1",
                                                              "3","5","7","9",
                                                              "11","13","15",
                                                              "17","19","21",
                                                              "23"))
levels(est_dat$Participant) = c("A1", "A2", "A3", "A4", "A5", "A6", "A7",
                                "A8", "A9", "A10", "A11", "A12", "B1",
                                "B2", "B3", "B4", "B5", "B6", "B7", "B8",
                                "B9", "B10", "B11", "B12")

# Get Estimated switch_points 
Est_slabs_to_test = data.frame(Participant=character(),
                               Estimate_Switch_Slab = numeric())

# Run loop
for (x in levels(est_dat$Participant)){
  # subset data
  ss = est_dat[est_dat$Participant==x,]
  # get glm 
  m = glm(data=ss, cbind(Acc, trials-Acc)~Slab,
          #offset=ss$off_set,
          binomial)
  # get predictions
  p = predict(m, data.frame(Slab=seq(0:34)), type="response")
  p = as.numeric(p)
  # switch point
  slab50 = which(abs(p-0.50)==min(abs(p-0.50)))
  # add to dataset
  Est_slabs_to_test = rbind(Est_slabs_to_test, data.frame(Participant=x,
                                                          Estimate_Switch_Slab=slab50))
}

# tidy 
rm(m, ss, p, slab50, x)

# combine switch points datasets 
switch_points <- merge(Est_slabs_to_test,slabs_to_test)

# tidy 
rm(Est_slabs_to_test, slabs_to_test)

#### Make normalised distance dataset ####
df_norm <- df 

# select only the needed columns 
df_norm <- select(df_norm,
                  Participant,
                  Trial,
                  Position,
                  Hoop_Pos)

# normalise standing pos
df_norm$Norm_Pos <- abs(df_norm$Position/df_norm$Hoop_Pos)

# Create metres column 
df_norm$Hoop_m <- df_norm$Hoop_Pos*0.46

# add switch points to frame 
df_norm <- merge(df_norm, switch_points)

#### Plots ####
#### Plots: Standing Positions ####
plt_stnpos <- ggplot(df_norm, aes(Hoop_m, Norm_Pos))
plt_stnpos <- plt_stnpos + geom_point()
plt_stnpos <- plt_stnpos + geom_vline(aes(xintercept = Actual_Switch_Slab*0.46-0.1),
                                      colour = "red")
plt_stnpos <- plt_stnpos + geom_vline(aes(xintercept = Estimate_Switch_Slab*0.46+0.1),
                                      colour = "blue")
plt_stnpos <- plt_stnpos + theme_bw()
plt_stnpos <- plt_stnpos + scale_y_continuous(name="Participant Position",
                                              limits=c(0,1))
plt_stnpos <- plt_stnpos + scale_x_continuous(name="Distance (metres)", limits= c(0,8))
plt_stnpos <- plt_stnpos + facet_wrap(~Participant)
plt_stnpos

# save this plot 
ggsave("../../Figures/Experiment_1_Awareness/Part_2_Standing_pos.png", height = 10, width = 10)

#### Plots: Actual vs. Estimated Accuracy ####

