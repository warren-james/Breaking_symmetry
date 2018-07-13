#### Level 3 group second semster 2016/17 ####
# This experiment was the two throw experiment 
# This script is to get expected accuracy given strategy in the 
# Two Throw condition and also make the plots 
# Written by Warren James

#### load libraries ####
library(tidyverse)
library(psyphy)
library(reshape2)

#### Any Functions ####

#### Notes #####

#### Any constants ####
Hoop_size <- 0.46

#### make the data ####
# add the line below to make it so you can change the strings to numbers
#, stringsAsFactors=FALSE
dat = read.csv("data/Level3_data_TwoThrow.csv", header = TRUE, stringsAsFactors=FALSE) 

# colours
dat$Colour[dat$Colour == "Blue"] <- "blue"
dat$Colour[dat$Colour == "Blue "] <- "blue"
dat$Colour[dat$Colour == "blue "] <- "blue"
dat$Colour[dat$Colour == "red "] <- "red"
dat$Colour[dat$Colour == " red"] <- "red"
dat$Colour[dat$Colour == "Red"] <- "red"
dat$Colour[dat$Colour == "rred"] <- "red"
dat$Colour[dat$Colour == "Yellow"] <- "yellow"
dat$Colour[dat$Colour == "yellow "] <- "yellow"

dat$Colour[dat$Colour == "blue"] <- "B"
dat$Colour[dat$Colour == "red"] <- "R"
dat$Colour[dat$Colour == "yellow"] <- "Y"
#dat$Colour[dat$Colour == ""] <- NA

# North.Accuracy
dat$North.Accuracy[dat$North.Accuracy == "Hit "] <- 1
dat$North.Accuracy[dat$North.Accuracy == "Hit"] <- 1
dat$North.Accuracy[dat$North.Accuracy == "hit"] <- 1
dat$North.Accuracy[dat$North.Accuracy == "hi"] <- 1
dat$North.Accuracy[dat$North.Accuracy == "hit "] <- 1
dat$North.Accuracy[dat$North.Accuracy == "HIT"] <- 1
dat$North.Accuracy[dat$North.Accuracy == "Miss"] <- 0
dat$North.Accuracy[dat$North.Accuracy == "MIS"] <- 0
dat$North.Accuracy[dat$North.Accuracy == "MIss"] <- 0
dat$North.Accuracy[dat$North.Accuracy == "miss"] <- 0
dat$North.Accuracy[dat$North.Accuracy == "MISS"] <- 0
dat$North.Accuracy[dat$North.Accuracy == ""] <- NA

#South Accuracy
dat$South.Accuracy[dat$South.Accuracy == "Hit "] <- 1
dat$South.Accuracy[dat$South.Accuracy == "Hit"] <- 1
dat$South.Accuracy[dat$South.Accuracy == "hit"] <- 1
dat$South.Accuracy[dat$South.Accuracy == "hi"] <- 1
dat$South.Accuracy[dat$South.Accuracy == "hit "] <- 1
dat$South.Accuracy[dat$South.Accuracy == "HIT"] <- 1
dat$South.Accuracy[dat$South.Accuracy == "Miss"] <- 0
dat$South.Accuracy[dat$South.Accuracy == "MIS"] <- 0
dat$South.Accuracy[dat$South.Accuracy == "MIss"] <- 0
dat$South.Accuracy[dat$South.Accuracy == "miss"] <- 0
dat$South.Accuracy[dat$South.Accuracy == "MISS"] <- 0


# Participants
dat$Participant[dat$Participant == "ZW3 "] <- "ZW3"

dat <- dat[complete.cases(dat),]

#### change position to absolute values #### 
dat[,4] <- dat[,4] - 20

#### give a hoop placement value based on colour ####
for (row in 1:nrow(dat))
{
  dat$HoopDelta[row] = dat[row,as.character(dat$Colour[row])]
}
rm(row)

#### some extra setup ####
dat$Participant = as.factor(dat$Participant)
dat$abspos = with(dat, abs(Position/HoopDelta))

#### load switch_points ####
load("scratch/slabs_to_test")

switch_points <- select(slabs_to_test,
                        participant,
                        slab50)

# rename this 
colnames(switch_points) <- c("Participant",
                             "switchSlab")

# merge them 
dat <- merge(dat,switch_points)

# save as part2_TT
df_part2_TT <- dat
save(df_part2_TT, file = "scratch/df_part2_TT")

# tidy 
rm(df_part2_TT)

#### makes the plots ####
#### NOTE: Ask Amelia whether to plot above 1 to show the people that stood outside the hoops? ####
plt <- ggplot(dat, aes(x=HoopDelta*Hoop_size, y=abspos)) + geom_point()
plt <- plt + geom_vline(aes(xintercept=switchSlab*Hoop_size), colour='red')
plt <- plt + facet_wrap(~Participant, nrow=3)
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(name="Participant Position",
                                limits=c(0,1.5))
plt <- plt + scale_x_continuous(name="Distance (Metres)",
                                limits = c(0, 10),
                                breaks = c(0,2,4,6,8,10))
plt 
ggsave("../../Figures/Experiment_2_Two_throw/Part2_TT_w-outliers.png",
       width = 8,
       height = 10,
       units = "in")

#### need to get the slabs to test bit into this somehow so I can do the analysis ####
slabs_to_test <- melt(slabs_to_test, id="participant")

colnames(slabs_to_test) <- c("Participant", "Slabtype", "HoopDelta")


#### get separate distances ####
dat$Sdelta <- -1*(dat$HoopDelta)

dat$Ndist <- dat$HoopDelta - dat$Position

dat$Sdist <- dat$Position - dat$Sdelta


#### actual accuracy #### 
tactacc <- ddply(dat, c("Participant"), summarise,
                Nacc = mean(as.numeric(North.Accuracy)),
                Sacc = mean(as.numeric(South.Accuracy)))


# by distance; pretty sure this is right...
tactacc1 <- ddply(dat, c("Participant", "HoopDelta"), summarise,
                 Nacc = mean(as.numeric(North.Accuracy)),
                 Sacc = mean(as.numeric(South.Accuracy)))

tactacc1 <- merge(tactacc1, slabs_to_test, by = c("Participant", "HoopDelta"))
tactacc1 <- tactacc1[,c(1,3,4,5)]


#average acc
avgdat <- tactacc1

avgdat$avgacc <- (avgdat$Nacc + avgdat$Sacc)/2


#### optimal strategy ####
# create new dataframe... 
optdat <- dat 

# get accuracies
m = glm(data=beanbagdat, cbind(inhoop, trials-inhoop)~slab+participant, binomial)

noslabs <- 1:25
a <- matrix(ncol = 18, nrow = 25)
for (ii in noslabs){
  a[ii,] <- predict(m, data.frame(slab = ii, participant=levels(beanbagdat$participant)), type="response")
}


tempdat <- a
pts <- c( "CP1","CP2","CP3","EM1","EM2","EM3","EM4","EMC1","EMC2","EMC3","IS1","IS2","IS3","SJ1","SJ2","ZW1", 
          "ZW2","ZW3")
colnames(tempdat) <- pts
tempdat <- t(tempdat)
tempdat <- melt(tempdat, id="Participant")
colnames(tempdat) <- c("Participant", "HoopDelta", "PAcc")

optdat <- merge(optdat, tempdat, by=c("Participant", "HoopDelta"))


optdat <- merge(optdat, slabs_to_test, by = c("Participant", "HoopDelta"))

optdat <- optdat[order(optdat[,1],optdat[,3]),]

optdat$optpos <- 0
optdat$optpos[optdat$HoopDelta >= optdat$switchSlab] <- 1
optdat$optacc <- optdat$PAcc
optdat$optacc[optdat$optpos == 1] <- 0.5

# total accuracy
oacc <- ddply(optdat, c("Participant"), summarise,
              Nacc = mean(as.numeric(optacc)))

# by distances
oacc1 <- ddply(optdat, c("Participant","HoopDelta"), summarise,
               Nacc = mean(as.numeric(optacc)))
              


oacc1 <- merge(oacc1, slabs_to_test, by = c("Participant", "HoopDelta"))
oacc1 <- oacc1[,c(1,3,4)]
oacc1 <- oacc1[,c(1,3,2)]

# wide version 
wide_oacc1 <- dcast(oacc1, Participant ~ Slabtype, value = oacc1[,3])


#### distance stuff ####
distdat <- ddply(dat, c("Participant"), summarise,
                 dist = mean(abspos))

distdat1 <- ddply(dat, c("Participant", "HoopDelta"), summarise,
                  dist = mean(abspos))
distdat1 <- merge(distdat1, slabs_to_test, by = c("Participant", "HoopDelta"))
distdat1 <- distdat1[,c(1,3,4)]

distdat1 <- distdat1[,c(1,3,2)]
wide_distdat1 <- dcast(distdat1, Participant ~ Slabtype, value = distdat1[,3])


#### avg stuff ####
avg <- avgdat[,c(1,4,5)]
wide_avg <- dcast(avg, Participant ~ Slabtype, value = avg[,3])


#### write the data files ####
write.table(wide_distdat1, "tempdat/two_twothrowdist.txt", sep = "\t")

write.table(wide_avg, "tempdat/two_averageacc.txt", sep = "\t")

write.table(wide_oacc1, "tempdat/two_optimalstrat.txt", sep = "\t")

