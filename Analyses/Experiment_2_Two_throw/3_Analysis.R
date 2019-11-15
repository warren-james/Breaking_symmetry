library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)
library(ez)


#### distance first ####
# read in the two data sets 
dist_1 <- read.csv("dat/One_Throw_distance.csv", sep = ",", header = TRUE)
 
dist_2 <- read.csv("dat/Two_Throw_distance.csv", sep = ",", header = TRUE)

dist_1 <- dist_1[,c(1,4,5,3,6,7,2)]
dist_2 <- dist_2[,c(1,4,5,3,6,7,2)]


# need to reshape them
dist_1 <- melt(dist_1, id = "Participant")
colnames(dist_1) <- c("Participant", "Slabtype", "Distance")

dist_2 <- melt(dist_2, id = "Participant")
colnames(dist_2) <- c("Participant", "Slabtype", "Distance")

# add marker for 1 or two throws
dist_1$throws <- 1
dist_2$throws <- 2

# merge the data sets using rbind
bothdist <- rbind(dist_1, dist_2)

#### distance Analysis ####
# 2 x 2; near vs far, one vs two
# need to just keep slabeasy and slabhard for this bit
distanovadat <- bothdist[bothdist$Slabtype == "slabhard" | bothdist$Slabtype == "slabeasy",] 
                         
testANOVA = ezANOVA(data = distanovadat, dv = Distance, wid = Participant, within = .(throws, Slabtype), type = 3, detailed = TRUE)

# There wasn't a sig difference here at all...

#### accuracy part ####
# setup the data files 
optimal_1 <- read.csv("dat/One_Throw_Optimal.csv", sep = ",", header = TRUE)

optimal_2 <- read.csv("dat/Two_Throw_OptimalAccuracy.csv", sep = ",", header = TRUE)

avg_1 <- read.csv("dat/One_Throw_ActualAccuracy.csv", sep = ",", header = TRUE)

avg_2 <- read.csv("dat/Two_Throw_AverageAccuracy.csv", sep = ",", header = TRUE)


# make overall accuracy column
optimal_1$avg <- rowMeans(optimal_1[,2:7], dims = 1)

optimal_2$avg <- rowMeans(optimal_2[,2:7], dims = 1)

avg_1$avg <- rowMeans(avg_1[,2:7], dims = 1)

avg_2$avg <- rowMeans(avg_2[,2:7], dims = 1)


#### accuracy analysis #### 
# just two separate t-tests to comare their accuracy 

# 1 throw t test 
onettest <- t.test(optimal_1$avg,avg_1$avg, paired = TRUE)

# 2 throw t test 
twottest <- t.test(optimal_2$avg, avg_2$avg, paired = TRUE)

# averages against each other 
attest <- t.test(avg_2$avg, avg_1$avg, paired =TRUE)

# both of these tests produce a significant results... so it might be a good idea to look into the data a bit more?



#### some plots ####
#### accuracy ####
# make datasets 
optimal_1 <- optimal_1[,c(1,8)]
optimal_2 <- optimal_2[,c(1,8)]
avg_1 <- avg_1[,c(1,8)]
avg_2 <- avg_2[,c(1,8)]

optimal_1$throws <- 1
optimal_2$throws <- 2 
optimal_1$opt <- "opt"
optimal_2$opt <- "opt"

avg_1$throws <- 1
avg_2$throws <- 2 
avg_1$opt <- "avg"
avg_2$opt <- "avg"

newdat <- do.call("rbind", list(optimal_1,optimal_2,avg_1,avg_2))
newdat$throws <- as.factor(newdat$throws)

gaccdat <- ddply(newdat, c("throws", "opt"), summarise,
                 N = length(avg),
                 mm = mean(avg),
                 sddist = sd(avg),
                 sedist = sddist/sqrt(N),
                 upper = mm + sedist,
                 lower = mm - sedist)


# plot 
plt = ggplot(gaccdat, aes(opt, mm, colour = opt, fill = opt))
plt = plt + geom_bar(stat = "identity")
plt = plt + geom_errorbar(aes(ymin= lower, ymax = upper))
plt = plt + facet_wrap(~throws)


#### distance ####
gdistdat <- ddply(bothdist, c("Slabtype", "throws"),summarise,
                  N = length(Distance),
                  mdist = mean(Distance),
                  sddist = sd(Distance),
                  sedist = sddist/sqrt(N),
                  upper = mdist + sedist,
                  lower = mdist - sedist)

gdistdat$throws <- as.factor(gdistdat$throws)

# plot 
plt2 = ggplot(gdistdat, aes(Slabtype, mdist, colour = throws))
plt2 = plt2 + geom_point()
plt2 = plt2 + geom_errorbar(aes(ymin = lower, ymax = upper))


#### each participant's distance ####
bothdist$throws <- as.factor(bothdist$throws)

bothdist$Slabtype <- revalue(bothdist$Slabtype, c("slabeasy"="90%","minus1"="m-1","slab50"="m",
                             "plus1"="m+1","plus2"="m+2","slabhard"="10%"))

plt3 = ggplot(bothdist, aes(Slabtype, Distance, colour = throws))
plt3 = plt3 + geom_point(position=position_jitter(width=.4,height=.0))
#plt3 = plt3 + geom_vline(aes(xintercept=switchSlab), colour='red')
plt3 = plt3 + scale_y_continuous(name="Normalised standing position", breaks = c(0,0.25,0.5,0.75,1))
plt3 = plt3 + scale_x_discrete(name = "Slab Type")
plt3 = plt3 + labs(colour = "No. of throws")
plt3 = plt3 + theme_bw()
plt3 = plt3 + facet_wrap(~Participant, nrow = 3)



# by throws 
plt4 = ggplot(bothdist, aes(Slabtype, Distance, colour = Participant))
plt4 = plt4 + geom_point()
plt4 = plt4 + facet_wrap(~throws)


#### Do plt3 but with Delta metres as the x-axis ####
# read in new data...
dat1 = read.csv("data/Level3_data_OneThrow.csv", header = TRUE, stringsAsFactors=FALSE) 


#### tidy up the data ####
# colours
dat1$Colour[dat1$Colour == "Blue"] <- "blue"
dat1$Colour[dat1$Colour == "Blue "] <- "blue"
dat1$Colour[dat1$Colour == "blue "] <- "blue"
dat1$Colour[dat1$Colour == "red "] <- "red"
dat1$Colour[dat1$Colour == " red"] <- "red"
dat1$Colour[dat1$Colour == "Red"] <- "red"
dat1$Colour[dat1$Colour == "rred"] <- "red"
dat1$Colour[dat1$Colour == "Yellow"] <- "yellow"

dat1$Colour[dat1$Colour == "blue"] <- "B"
dat1$Colour[dat1$Colour == "red"] <- "R"
dat1$Colour[dat1$Colour == "yellow"] <- "Y"
dat1$Colour[dat1$Colour == ""] <- NA

# hit or miss
dat1$Hit.Miss[dat1$Hit.Miss == "Hit "] <- 1
dat1$Hit.Miss[dat1$Hit.Miss == "hit"] <- 1
dat1$Hit.Miss[dat1$Hit.Miss == "hit "] <- 1
dat1$Hit.Miss[dat1$Hit.Miss == "HIT"] <- 1
dat1$Hit.Miss[dat1$Hit.Miss == "Miss"] <- 0
dat1$Hit.Miss[dat1$Hit.Miss == "MIS"] <- 0
dat1$Hit.Miss[dat1$Hit.Miss == "MIss"] <- 0
dat1$Hit.Miss[dat1$Hit.Miss == "miss"] <- 0
dat1$Hit.Miss[dat1$Hit.Miss == "MISS"] <- 0
#dat$Hit.Miss[dat$Hit.Miss != 1 & dat$Hit.Miss != 0] <- NA

# north south
dat1$North.South[dat1$North.South == "north"] <- "N"
dat1$North.South[dat1$North.South == "North"] <- "N"
dat1$North.South[dat1$North.South == "north "] <- "N"
dat1$North.South[dat1$North.South == "south"] <- "S"
dat1$North.South[dat1$North.South == "South"] <- "S"
dat1$North.South[dat1$North.South == "south "] <- "S"
#at$North.South[dat$North.South == ""] <- NA

# Participants
dat1$Participant[dat1$Participant == "ZW3 "] <- "ZW3"

# remove any NA stuff
dat1 <- dat1[complete.cases(dat1),]

# change position to absolute values # 
dat1[,5] <- dat1[,5] - 20

# add in throws column 
dat1$throws <- 1

for (row in 1:nrow(dat1))
{
  dat1$HoopDelta[row] = dat1[row,as.character(dat1$Colour[row])]
}
rm(row) # removes "row" as a variable... to keep it tidy

names(dat1)[6] = 'Accuracy'
dat1$Participant = as.factor(dat1$Participant)
dat1$abspos = with(dat1, abs(Position/HoopDelta))



#### now for two throws ####
dat2 = read.csv("data/Level3_data_TwoThrow.csv", header = TRUE, stringsAsFactors=FALSE) 

# colours
dat2$Colour[dat2$Colour == "Blue"] <- "blue"
dat2$Colour[dat2$Colour == "Blue "] <- "blue"
dat2$Colour[dat2$Colour == "blue "] <- "blue"
dat2$Colour[dat2$Colour == "red "] <- "red"
dat2$Colour[dat2$Colour == " red"] <- "red"
dat2$Colour[dat2$Colour == "Red"] <- "red"
dat2$Colour[dat2$Colour == "rred"] <- "red"
dat2$Colour[dat2$Colour == "Yellow"] <- "yellow"
dat2$Colour[dat2$Colour == "yellow "] <- "yellow"

dat2$Colour[dat2$Colour == "blue"] <- "B"
dat2$Colour[dat2$Colour == "red"] <- "R"
dat2$Colour[dat2$Colour == "yellow"] <- "Y"
#dat$Colour[dat$Colour == ""] <- NA

# North.Accuracy
dat2$North.Accuracy[dat2$North.Accuracy == "Hit "] <- 1
dat2$North.Accuracy[dat2$North.Accuracy == "Hit"] <- 1
dat2$North.Accuracy[dat2$North.Accuracy == "hit"] <- 1
dat2$North.Accuracy[dat2$North.Accuracy == "hi"] <- 1
dat2$North.Accuracy[dat2$North.Accuracy == "hit "] <- 1
dat2$North.Accuracy[dat2$North.Accuracy == "HIT"] <- 1
dat2$North.Accuracy[dat2$North.Accuracy == "Miss"] <- 0
dat2$North.Accuracy[dat2$North.Accuracy == "MIS"] <- 0
dat2$North.Accuracy[dat2$North.Accuracy == "MIss"] <- 0
dat2$North.Accuracy[dat2$North.Accuracy == "miss"] <- 0
dat2$North.Accuracy[dat2$North.Accuracy == "MISS"] <- 0
dat2$North.Accuracy[dat2$North.Accuracy == ""] <- NA

#South Accuracy
dat2$South.Accuracy[dat2$South.Accuracy == "Hit "] <- 1
dat2$South.Accuracy[dat2$South.Accuracy == "Hit"] <- 1
dat2$South.Accuracy[dat2$South.Accuracy == "hit"] <- 1
dat2$South.Accuracy[dat2$South.Accuracy == "hi"] <- 1
dat2$South.Accuracy[dat2$South.Accuracy == "hit "] <- 1
dat2$South.Accuracy[dat2$South.Accuracy == "HIT"] <- 1
dat2$South.Accuracy[dat2$South.Accuracy == "Miss"] <- 0
dat2$South.Accuracy[dat2$South.Accuracy == "MIS"] <- 0
dat2$South.Accuracy[dat2$South.Accuracy == "MIss"] <- 0
dat2$South.Accuracy[dat2$South.Accuracy == "miss"] <- 0
dat2$South.Accuracy[dat2$South.Accuracy == "MISS"] <- 0


# Participants
dat2$Participant[dat2$Participant == "ZW3 "] <- "ZW3"


#### change position to absolute values #### 
dat2[,4] <- dat2[,4] - 20

# add in throws column 
dat2$throws <- 2

#### give a hoop placement value based on colour ####
for (row in 1:nrow(dat2))
{
  dat2$HoopDelta[row] = dat2[row,as.character(dat2$Colour[row])]
}
rm(row)

dat2$Participant = as.factor(dat2$Participant)
dat2$abspos = with(dat2, abs(Position/HoopDelta))


#### get switch slab ####

s1dat = read.csv("data/Part1_session_data.csv")
s1dat$trials = 12
s1dat$acc = s1dat$Acc/s1dat$trials
s1dat$off_set = log((1-0.01)/0.01)	


#### set up for the graphs; gives the switching points #### 
dat1$switchSlab = 0 
for (pp in levels(dat1$Participant))
{
  # fit model from part 1 accuracy data
  ss = s1dat[which(s1dat$Participant==pp),]
  m = glm(data=ss, cbind(Acc, trials-Acc)~Slab+0, offset=ss$off_set, binomial)
  
  # for each hoopPos, work out where participant should of stood
  placesToStand = 0:20
  
  
  slab = max(which(predict(m, data.frame(Slab=placesToStand ), type='response')>0.5))
  dat1$switchSlab[which(dat1$Participant==pp)] = slab
}


dat2$switchSlab = 0 
for (pp in levels(dat2$Participant))
{
  # fit model from part 1 accuracy data
  ss = s1dat[which(s1dat$Participant==pp),]
  m = glm(data=ss, cbind(Acc, trials-Acc)~Slab+0, offset=ss$off_set, binomial)
  
  # for each hoopPos, work out where participant should of stood
  placesToStand = 0:20
  
  
  slab = max(which(predict(m, data.frame(Slab=placesToStand ), type='response')>0.5))
  dat2$switchSlab[which(dat2$Participant==pp)] = slab
}


#### only keep needed columns ####
dat1 <- dat1[,c(1,10,11,12,13)]

dat2 <- dat2[,c(1,10,11,12,13)]

bothdist2 <- rbind(dat1, dat2)
bothdist2 <- ddply(bothdist2, c("Participant", "throws", "HoopDelta", "switchSlab"), summarise,
                   mdist = mean(abspos))


#### now make the plot with delta metres
bothdist2$throws <- as.factor(bothdist2$throws)

plt5 = ggplot(bothdist2, aes(x=0.46*HoopDelta, y=mdist, colour = throws))
plt5 = plt5 + geom_point(position=position_jitter(width=.4,height=.0))
plt5 = plt5 + geom_vline(aes(xintercept=0.46*switchSlab), colour='black')
plt5 = plt5 + scale_y_continuous(name="Normalised standing position", breaks = c(0,0.25,0.5,0.75,1))
plt5 = plt5 + scale_x_continuous(name="Delta (metres)")
plt5 = plt5 + labs(colour = "No. of throws")
plt5 = plt5 + theme_bw()
plt5 = plt5 + facet_wrap(~Participant, nrow = 3)
