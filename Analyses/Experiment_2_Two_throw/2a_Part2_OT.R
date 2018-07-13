#### Level 3 group second semster 2016/17 ####
# This experiment was the two throw experiment 
# This script is to get expected accuracy given strategy in the 
# One Throw condition and also make the plots 
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
dat = read.csv("data/Level3_data_OneThrow.csv", header = TRUE, stringsAsFactors=FALSE) 


#### tidy up the data ####
# colours
dat$Colour[dat$Colour == "Blue"] <- "blue"
dat$Colour[dat$Colour == "Blue "] <- "blue"
dat$Colour[dat$Colour == "blue "] <- "blue"
dat$Colour[dat$Colour == "red "] <- "red"
dat$Colour[dat$Colour == " red"] <- "red"
dat$Colour[dat$Colour == "Red"] <- "red"
dat$Colour[dat$Colour == "rred"] <- "red"
dat$Colour[dat$Colour == "Yellow"] <- "yellow"

dat$Colour[dat$Colour == "blue"] <- "B"
dat$Colour[dat$Colour == "red"] <- "R"
dat$Colour[dat$Colour == "yellow"] <- "Y"
dat$Colour[dat$Colour == ""] <- NA

# hit or miss
dat$Hit.Miss[dat$Hit.Miss == "Hit "] <- 1
dat$Hit.Miss[dat$Hit.Miss == "hit"] <- 1
dat$Hit.Miss[dat$Hit.Miss == "hit "] <- 1
dat$Hit.Miss[dat$Hit.Miss == "HIT"] <- 1
dat$Hit.Miss[dat$Hit.Miss == "Miss"] <- 0
dat$Hit.Miss[dat$Hit.Miss == "MIS"] <- 0
dat$Hit.Miss[dat$Hit.Miss == "MIss"] <- 0
dat$Hit.Miss[dat$Hit.Miss == "miss"] <- 0
dat$Hit.Miss[dat$Hit.Miss == "MISS"] <- 0
#dat$Hit.Miss[dat$Hit.Miss != 1 & dat$Hit.Miss != 0] <- NA

# north south
dat$North.South[dat$North.South == "north"] <- "N"
dat$North.South[dat$North.South == "North"] <- "N"
dat$North.South[dat$North.South == "north "] <- "N"
dat$North.South[dat$North.South == "south"] <- "S"
dat$North.South[dat$North.South == "South"] <- "S"
dat$North.South[dat$North.South == "south "] <- "S"
#at$North.South[dat$North.South == ""] <- NA

# Participants
dat$Participant[dat$Participant == "ZW3 "] <- "ZW3"

# remove any NA stuff
dat <- dat[complete.cases(dat),]

#### change position to absolute values #### 
dat[,5] <- dat[,5] - 20



#### give a hoop placement value based on colour ####
for (row in 1:nrow(dat))
{
  dat$HoopDelta[row] = dat[row,as.character(dat$Colour[row])]
}

# tidy
rm(row)

# rename columns
names(dat)[6] = 'Accuracy'
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

# save as part2_OT
df_part2_OT <- dat
save(df_part2_OT, file = "scratch/df_part2_OT")

# tidy 
rm(df_part2_OT)

#### makes the plots ####
#### NOTE: Ask Amelia whether to plot above 1 to show the people that stood outside the hoops? ####
plt <- ggplot(dat, aes(x=HoopDelta*Hoop_size, y=abspos)) + geom_point()
plt <- plt + geom_vline(aes(xintercept=switchSlab*Hoop_size), colour='red')
plt <- plt + facet_wrap(~Participant, nrow=3)
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(name="Participant Position", limits=c(0,1.5))
plt <- plt + scale_x_continuous(name="Distance (metres)",
                                limits = c(0, 10),
                                breaks = c(0,2,4,6,8,10))
plt 
ggsave("../../Figures/Experiment_2_Two_throw/Part2_OT_w-outliers.png",
       width = 8,
       height = 10,
       units = "in")








#### Use slabs_to_test to get accuracy over several distances? ####
slabs_to_test <- melt(slabs_to_test, id="participant")

colnames(slabs_to_test) <- c("Participant", "Slabtype", "HoopDelta")

#test <- optdat
#test <- merge(test, slabs_to_test, by=c("Participant", "HoopDelta"))

#test <- test[order(test[,1],test[,3]),]






#### work out their accuracy; actual ####
# total
actacc <- ddply(dat, c("Participant"), summarise,
                acc = mean(as.numeric(Accuracy)))

# by distance; pretty sure this is right...
actacc1 <- ddply(dat, c("Participant", "HoopDelta"), summarise,
                 acc = mean(as.numeric(Accuracy)))

actacc1 <- merge(actacc1, slabs_to_test, by = c("Participant", "HoopDelta"))
actacc1 <- actacc1[,c(1,3,4)]


#### optimal strategy ####
# create new dataframe... 
optdat <- dat 


# get their accuracies
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


# merges the two datasets
optdat <- merge(optdat, tempdat, by=c("Participant", "HoopDelta"))

# make it so there a sensible levels...
optdat <- merge(optdat, slabs_to_test, by = c("Participant", "HoopDelta"))


#reorders the dataset 
optdat <- optdat[order(optdat[,1],optdat[,3]),]

#sets the standing position and their accuracy
optdat$optpos <- 0
optdat$optpos[optdat$HoopDelta >= optdat$switchSlab] <- 1
optdat$optacc <- optdat$PAcc
optdat$optacc[optdat$optpos == 1] <- 0.5


# total accuracy
oacc <- ddply(optdat, c("Participant"), summarise,
                acc = mean(as.numeric(optacc)))

# by distances
oacc1 <- ddply(optdat, c("Participant","HoopDelta"), summarise,
              acc = mean(as.numeric(optacc)))


oacc1 <- merge(oacc1, slabs_to_test, by = c("Participant", "HoopDelta"))
oacc1 <- oacc1[,c(1,3,4)]



#### centre strategy ####
# total
centacc <- ddply(optdat, c("Participant"), summarise,
              acc = mean(as.numeric(PAcc)))

# by distances
centacc1 <- ddply(optdat, c("Participant","HoopDelta"), summarise,
                 acc = mean(as.numeric(PAcc)))

centacc1 <- merge(centacc1, slabs_to_test, by = c("Participant", "HoopDelta"))
centacc1 <- centacc1[,c(1,3,4)]



#### actual strategy accounting for guesswork ####
# make new dataset...
#### ask Amelia... not sure about how to do this bit since standing position is different now...
actdat <- dat
actdat$Position <- abs(actdat$Position)
actdat$HoopDelta2 <- actdat$HoopDelta - actdat$Position

tempdat2 <- tempdat
colnames(tempdat2) <- c("Participant", "HoopDelta2", "PAcc")

slabs_to_test2 <- slabs_to_test
colnames(slabs_to_test2) <- c("Participant", "Slabtype", "HoopDelta2")

actdat <- merge(actdat, tempdat2, by=c("Participant", "HoopDelta2"))

# make it so there a sensible levels...
actdat <- merge(actdat, slabs_to_test, by = c("Participant", "HoopDelta"))


actdat <- actdat[order(actdat[,1],actdat[,4]),]


# total accuracy
actacca <- ddply(actdat, c("Participant"), summarise,
              acc = mean(as.numeric(PAcc)))

# by distances
actacca1 <- ddply(actdat, c("Participant","HoopDelta"), summarise,
               acc = mean(as.numeric(PAcc)))
actacca1 <- merge(actacca1, slabs_to_test, by = c("Participant", "HoopDelta"))
actacca1 <- actacca1[,c(1,3,4)]




#### make wide versions of the data for level 3's ####

# optimal strategy
oacc1 <- oacc1[,c(1,3,2)]
wide_oacc1 <- dcast(oacc1, Participant ~ Slabtype, value = oacc1[,3])


# centre strat
centacc1 <- centacc1[,c(1,3,2)]
wide_centacc1 <- dcast(centacc1, Participant ~ Slabtype, value = centacc1[,3])


# actual strat 
actacc1 <- actacc1[,c(1,3,2)]
wide_actacc1 <- dcast(actacc1, Participant ~ Slabtype, value = actacc1[,3])


# accounting for chance 
actacca1 <- actacca1[,c(1,3,2)]
wide_actacca1 <- dcast(actacca1, Participant ~ Slabtype, value = actacca1[,3])




#### absolute distance stuff
distdat <- ddply(dat, c("Participant"), summarise,
                 dist = mean(abspos))

distdat1 <- ddply(dat, c("Participant", "HoopDelta"), summarise,
                  dist = mean(abspos))
distdat1 <- merge(distdat1, slabs_to_test, by = c("Participant", "HoopDelta"))
distdat1 <- distdat1[,c(1,3,4)]

distdat1 <- distdat1[,c(1,3,2)]
wide_distdat1 <- dcast(distdat1, Participant ~ Slabtype, value = distdat1[,3])




#### makes text files ####

write.table(wide_oacc1, "tempdat/one_stratoptimal.txt", sep = "\t")


write.table(wide_actacc1, "tempdat/one_stratactual.txt", sep = "\t")


write.table(wide_centacc1, "tempdat/one_stratcentre.txt", sep = "\t")


write.table(wide_distdat1, "tempdat/one_AbsoluteDistance.txt", sep = "\t")


write.table(wide_actacca1, "tempdat/one_stratactualwithchance.txt", sep = "\t")

