#### Awareness and bean bags ####
# Updated version of my level 4 project 
# This script is to extract and look at the part 1 data 
#
# Written by Warren James

#### set options ####
options(digits = 4)

#### load in libraries #### 
library(tidyverse)
library(psyphy) # might be needed 

#### Any notes/constants ####
Hoop_size <- 0.46

#### Any functions ####

#### read in data ####
# create dataset
df <- read.csv("data/Part1_session_data.csv")

# collapse across direction 
df <- group_by(df, Participant, Slab)
df <- summarise(df, inhoop = sum(Acc))

# add in num of trials 
df$trials <- 24

# Get accuracy 
df$accuracy <- df$inhoop/df$trials

# add in offset 
e <- 0.01
df$off_set <- log((1-e)/e)

# rename
names(df) = c("participant","slab", "inhoop", "trials", "accuracy", "off_set") 

# tidy 
rm(e)

# make participant a factor 
df$participant <- as.factor(df$participant)

#### Fit the Curve ####
m = glm(data=df, cbind(inhoop, trials - inhoop)~slab:participant, binomial, offset=off_set)
df$p  = predict(m, type="response")

# tidy 
rm(m)

#### recode participants ####
# rename so they correspond to their group 
df$participant <- factor(df$participant, levels = c("2","4","6","8","10","12","14","16","18","20","22","24","1","3","5","7","9","11","13","15","17","19","21","23"))

levels(df$participant) = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12")

# save file 
df_part1 <- df
save(df_part1, file = "scratch/dataframes/df_part1")

# tidy 
rm(df_part1)

#### Get switch points ####
#### May need to check this against the original data as it doesn't seem to match up properly? ####
slabs_to_test = data.frame(participant=character(),
                           slab50 = numeric())

# setup sequence of slabs
slabs <- c(0:34)

# setup data frame 
acc_sep <-tibble(participant = character(), 
                 slab = numeric(),
                 accuracy = numeric())


for (x in levels(df$participant)){
  # subset data
  ss = df[df$participant==x,]
  # get glm 
  m = glm(data=ss, cbind(inhoop, trials-inhoop)~slab,
          #offset=ss$off_set,
          binomial)
  # get predictions
  p = predict(m, data.frame(slab=seq(0:34)), type="response")
  p = as.numeric(p)
  # switch point
  slab50 = which(abs(p-0.50)==min(abs(p-0.50)))
  # add to dataset
  slabs_to_test = rbind(slabs_to_test, data.frame(participant=x,
                                                  slab50=slab50))
  for(i in slabs){
    y = p[i + 1]
    acc_sep <- rbind(acc_sep, data.frame(participant = x,
                                         slab = i,
                                         accuracy = y))
  }
}

# tidy 
rm(m, ss, p, slab50, x, i, y, slabs)

# save 
save(slabs_to_test, file = "scratch/dataframes/slabs_to_test")
save(acc_sep, file = "scratch/dataframes/acc_sep")

# tidy?
rm(slabs_to_test, acc_sep)


#### Make plots of First Session Accuracy ####
# To convert to metres, put *Hoop_size after slab in the first line 
plt <- ggplot(df, aes(slab*Hoop_size,
                      accuracy))
plt <- plt + geom_point()
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(name="Accuracy")
plt <- plt + scale_x_continuous(name="Distance (Metres)")#,
                                #limit=c(0,18),
                                #breaks = c(0,5,10,15))
plt <- plt + geom_smooth(method=glm,
                         method.args = list(family = "binomial"), 
                         aes(y=p),
                         fullrange=T, se=F)
plt <- plt + facet_wrap(~participant, nrow = 4)
plt
# save this plot to the common folder
ggsave("../../Figures/Experiment_1_Awareness/Part_1_Plots.png", height = 10, width = 10)

# tidy 
rm(list = ls())



