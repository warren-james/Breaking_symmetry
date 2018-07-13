#### Level 3 group second semster 2016/17 ####
# This experiment was the two throw experiment 
# This script is to extract switch_distances for each participant
#
# Written by Warren James

#### Load libraries ####
library(tidyverse)
library(psyphy)

#### Any Functions ####

#### Notes #####

#### Any constants ####
Hoop_size <- 0.46

#### create the data set ####
# import data
beanbagdat = read.csv("data/Part1_session_data.csv")

# set names of variables
names(beanbagdat) = c("participant", "direction", "slab", "inhoop") 

# tell R that "participant" is a categorical factor
beanbagdat$participant = factor(beanbagdat$participant)

# tell R how many bean bags were thrown in
beanbagdat$trials = 12

# calculate accuracyÂ
beanbagdat$acc = beanbagdat$inhoop/beanbagdat$trials
e = 0.01
beanbagdat$off_set = log((1-e)/e)

# tidy 
rm(e)

# fit model 
m = glm(data=beanbagdat, cbind(inhoop, trials-inhoop)~slab:participant+0, binomial, offset=off_set)
beanbagdat$p  = predict(m, type="response")

# tidy 
rm(m)

#### creates the plots for each participant ####
# create plot
plt <- ggplot(beanbagdat, aes(x=Hoop_size*slab, y=acc))
plt <- plt + geom_point(position=position_jitter(width=.4,height=.0))
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(name="Accuracy")
plt <- plt + facet_wrap(~participant, nrow=3)
plt <- plt + scale_x_continuous(name="Delta (metres)",
                                limit=c(0,0.46*23),
                                breaks=c(2,4,6,8))
plt <- plt + geom_smooth(colour="black", method=glm,
                         method.args = list(family = "binomial"),
                         aes(y=p), fullrange=T, se=F)
# save
# ggsave("throwingTaskPart1.png", width=10, height=10)
ggsave("../../Figures/Experiment_2_Two_Throw/Part_1_Plots.png",
       height = 10, width = 10)

#### save df_part1 ####
df_part1 <- beanbagdat
save(df_part1, file = "scratch/df_part1")


#### with slab instead of metres #### 
# plt2 = ggplot(beanbagdat, aes(x=slab, y=acc)) + geom_point(position=position_jitter(width=.4,height=.0))
# plt2 = plt2 + scale_y_continuous(name="Proportion Correct")# + theme_minimal()
# plt2 = plt2 + facet_wrap(~participant, nrow=2) + theme_minimal()+ scale_x_continuous(name="Slab number", limit=c(0,25), breaks=c(5,10,15,20))
# plt2 = plt2 + geom_smooth(colour="black", method=glm, method.args = list(family = "binomial"), aes(y=p), fullrange=T, se=F)
# 
# 

#### creates the slabs to test dataframe ####
slabs_to_test = data.frame(participant=character(), slab10=numeric(),slab50=numeric(), slab90=numeric())
for (x in levels(beanbagdat$participant))
{
  print(x)
  ss = beanbagdat[which(beanbagdat$participant==x),]
  m = glm(data=ss, cbind(inhoop, trials-inhoop)~slab+0, offset=ss$off_set, binomial)
  p = predict(m, data.frame(slab=seq(1:25)), type="response")
  p = as.numeric(p)
  easy = 0.9
  hard = 0.1
  slab10 = which(abs(p-hard)==min(abs(p-hard)))
  slab50 = which(abs(p-0.50)==min(abs(p-0.50)))
  slab90 = which(abs(p-easy)==min(abs(p-easy)))
  slabs_to_test = rbind(slabs_to_test, data.frame(participant=x, slabhard=slab10, slab50=slab50, slabeasy=slab90))
}

slabs_to_test$minus1 <- slabs_to_test$slab50 - 1
slabs_to_test$plus1 <- slabs_to_test$slab50 + 1
slabs_to_test$plus2 <- slabs_to_test$slab50 + 2

# 2 groups; set 1 = 90, m-1, and m+1
#           set 2 = m, m+2, and 10
# slabs_to_test

# tidy 
rm(ss, easy, hard, p, slab10, slab50, slab90, x)

# save this file 
save(slabs_to_test, file = "scratch/slabs_to_test")



# not really needed just now #
# m = glm(data=beanbagdat, cbind(inhoop, trials-inhoop)~slab+participant, binomial)
# 
# a6 <- predict(m, data.frame(slab = 6, participant=levels(beanbagdat$participant)), type="response")



#### for loop for accuracy over distances ####
# Need to fix this at some point
# shouldn't be too hard to do.
noslabs <- 1:25
a <- matrix(ncol = 18, nrow = 25)
for (ii in noslabs){
  a[ii,] <- predict(m, data.frame(slab = ii, participant=levels(beanbagdat$participant)), type="response")
}

# a is now a matrix of all the distances and the participant's accuracy for that distance...