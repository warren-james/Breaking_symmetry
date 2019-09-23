#### level 3 - 2nd Semester 2017/18 ####
# Part 1 Accuracy measure script 
# Script for the Hoop Size version of the throwing task
# This script reads in the data for participants' accuracy 
# in the first session. 

# Written by Warren James


#### Libraries needed ####
library(tidyverse)

#### Constants ####
Hoop_size <- 0.46

#### Created functions ####
# This is used to get average distance between large and small hoop
# accuracy.
get_avg <- function(df, index, participant){
  sum(df[,index][df[,1] == participant])/2
}

#### read in dataset ####
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
m = glm(data=beanbagdat, acc~slab:hoop_size:participant, binomial, offset=off_set)
beanbagdat$p = predict(m, type="response")

# tidy up
rm(m)

#### make the plots ####
plt <- beanbagdat %>% 
  ggplot(aes(slab*Hoop_size,
             acc, 
             colour = hoop_size)) + 
  geom_point(position = position_jitter(width = .1, height = 0)) + 
  theme_bw() + 
  see::scale_color_flat() + 
  facet_wrap(~participant, nrow = 4) + 
  geom_smooth(method = glm, 
              method.args = list(family = "binomial"),
              aes(y = p),
              fullrange = T, se = F) + 
  theme(legend.position = "bottom") + 
  
plt <- ggplot(beanbagdat, aes(x=slab*Hoop_size,
                              y=acc,
                              colour = hoop_size))
plt <- plt + geom_point(position=position_jitter(width=.1,height=.0))
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(name="Accuracy")
plt <- plt + facet_wrap(~participant_num, nrow=4)
plt <- plt + scale_x_continuous(name="Delta (Metres)",
                                limit=c(0,12),
                                breaks = c(3,6,9,12))
plt <- plt + geom_smooth(method=glm,
                         method.args = list(family = "binomial"), 
                         aes(y=p),
                         fullrange=T, se=F)
plt <- plt + scale_colour_discrete(name = "Hoop Size", 
                                   labels=c("Large", "Small"))
# plt <- plt + ggtitle("Session 1 accuracy measures")
# plt <- plt + theme(plot.title = element_text(hjust = 0.5))
plt <- plt + theme(legend.position = "bottom")
plt

# save plot 
# ggsave("../../Figures/Experiment_3_Hoop_size/Session_1_plot.png",
#        height = 16,
#        width = 18,
#        units = "cm")


# NB To convert to metres, multiply slab by 0.46
# You will also want to change the breaks() argument to reflect it being metres

# save beanbagdat for part 2 
save(beanbagdat, file = "temp/beanbagdat")

#### extract slabs to test for both sizes ####
slabs_to_test = data.frame(participant=character(),
                           hoop_size=character(),
                           slab10=numeric(),
                           slab50=numeric(),
                           slab90=numeric())


for (x in levels(beanbagdat$participant)){
  
  for (i in levels(beanbagdat$hoop_size))
    {
      ss = beanbagdat[beanbagdat$participant==x & beanbagdat$hoop_size==i,]
      m = glm(data=ss, cbind(inhoop, trials-inhoop)~slab+0,
              offset=ss$off_set, binomial)
      p = predict(m, data.frame(slab=seq(1:30)), type="response")
      p = as.numeric(p)
      easy = 0.9
      hard = 0.1
      slab10 = which(abs(p-hard)==min(abs(p-hard)))
      slab50 = which(abs(p-0.50)==min(abs(p-0.50)))
      slab90 = which(abs(p-easy)==min(abs(p-easy)))
      minus1 = slab50 - 1
      plus1 = slab50 + 1
      plus2 = slab50 + 2
      slabs_to_test = rbind(slabs_to_test, data.frame(participant=x,
                                                      hoop_size=i,
                                                      slabhard=slab10,
                                                      slab50=slab50,
                                                      slabeasy=slab90,
                                                      plus1=plus1,
                                                      plus2=plus2,
                                                      minus1=minus1))
    }
}

# tidy 
rm(m, ss, easy, hard, i, minus1, p, plus1, plus2, slab10, slab50, slab90, x)

#### get average hoop separations ####
# loop to get average distance
for(i in unique(slabs_to_test$participant)){
  # for the indexing
  a <- 3
  
  slabhard=get_avg(slabs_to_test, a, i)
  a <- a + 1
  
  slab50=get_avg(slabs_to_test, a, i)
  a <- a + 1
  
  slabeasy=get_avg(slabs_to_test, a, i)
  a <- a + 1
  
  plus1=get_avg(slabs_to_test, a, i)
  a <- a + 1
  
  plus2=get_avg(slabs_to_test, a, i)
  a <- a + 1
  
  minus1=get_avg(slabs_to_test, a, i)
  
  slabs_to_test = rbind(slabs_to_test,
                        data.frame(participant=i,
                                   hoop_size="avg",
                                   slabhard=slabhard,
                                   slab50=slab50,
                                   slabeasy=slabeasy,
                                   plus1=plus1,
                                   plus2=plus2,
                                   minus1=minus1))
}
rm(a, i, minus1, plus1, plus2, slab50, slabeasy, slabhard)

# Save this to be used in next script
save(slabs_to_test, file = "temp/slabs_to_test")
 
#### Set up the distances for part 2 ####
# this is how it should be 
# Block 1: easy, m, m+ 2
# Block 2: m-1, m+1, 10

# setup columns to keep
b1_keeps <- c("participant", "slabeasy", "slab50", "plus2")
b2_keeps <- c("participant", "minus1", "plus1", "slabhard")

# keep only average dist data
Block_1 <- slabs_to_test[slabs_to_test$hoop_size == "avg",]
Block_2 <- slabs_to_test[slabs_to_test$hoop_size == "avg",]

# keep only needed columns 
Block_1 <- Block_1[b1_keeps]
Block_2 <- Block_2[b2_keeps]

# tidy 
rm(b1_keeps, b2_keeps)

# redo names for students 
colnames(Block_1) <- c("participant", "easy", "m", "m+2")
colnames(Block_2) <- c("participant", "m-1", "m+1", "hard")

#### For part 2 measures: random directions ####
# creat empty dataframe 
throw_direction <- data.frame(participant = character(),
                              block = numeric(),
                              direction = numeric())

# Loop to give every trial per participant a random direction
for(i in unique(beanbagdat$participant)){
  for(z in 1:2)
  for(x in 1:45){
    N_S <- sample(1:2, 1)
    throw_direction <- rbind(throw_direction, 
                             data.frame(participant = i,
                                        block = z,
                                        direction = N_S))
  }
}

# tidy
rm(i, N_S, x, z)

# add appropriate labels
throw_direction$direction <- as.factor(throw_direction$direction)
throw_direction$directionNS[throw_direction$direction == "1"] <- "North"
throw_direction$directionNS[throw_direction$direction == "2"] <- "South"


#### Save file of random directions ####
# run this once all data is collected
# create excel file for throw order 
# write.table(throw_direction, "temp/Directions.txt", row.names = FALSE, sep = "\t")

