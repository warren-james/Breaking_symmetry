#### Gambling Study - Throwing Task ####
# read in part 1 and get their accuracy scores 

#### Packages ####
library(tidyverse)

#### Constants ####
slab_size <- 0.46

#### Load in data ####
# set path 
results_files <- dir("data/Part1/")

# set up empty data frame 
df_part1 <- data.frame(Participant = character(),
                       Slab = numeric(),
                       InHoop = numeric())


# read in files 
for(f in results_files){
  # read in each file
  d <- read.csv(paste("data/Part1/", f, sep = ""), header = T)
  
  # add in Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[2]
  
  # add in Participant
  d$Participant <- Participant
  
  # add to empty data frame 
  df_part1 <- rbind(df_part1, d)
}

# tidy 
rm(results_files, f, d, Participant)

# rearrange 
df_part1 <- select(df_part1,
                   Participant,
                   everything())

#### sort new columns ####
# get accuracy value
df_part1$Trials <- 12
df_part1$Accuracy <- df_part1$InHoop / df_part1$Trials

# make Participant a Factor
df_part1$Participant <- as.factor(df_part1$Participant)

# save this file
save(df_part1, file = "scratch/data/df_part1")

#### run GLM ####
m <- glm(data = df_part1, Accuracy~Slab:Participant,
         family = binomial)

df_part1$p <- predict(m, type = "response")

#### make plot ####
plt <- ggplot(df_part1, aes(Slab, Accuracy))
plt <- plt + geom_point()
plt <- plt + theme_bw()
plt <- plt + geom_smooth(colour="blue", method=glm,
                         method.args = list(family = "binomial"),
                         aes(y=p), fullrange=T, se=F)
plt <- plt + facet_wrap(~Participant)
plt

# tidy 
rm(m)

#### get slabs_to_test ####
# empty data.frame
slabs_to_test <- data.frame(Participant = character(),
                            Acc_level = numeric(),
                            Test_slab = numeric())

# acc_levels we want
acc_levels <- c(90,75,25,10)
colours <- c("R","Y","B","G")
# separations we want 
separations <- c(1:30)

for(P in levels(df_part1$Participant)) {
  # get subset
  ss <- df_part1[df_part1$Participant == P,]
  
  # run glm for each participant 
  m <- glm(data = ss,
           Accuracy ~ Slab,
           family = binomial)
  
  # get predictions
  p <- predict(m, data.frame(Slab = separations), type = "response")
  p <- as.numeric(p)
  
  for(a in acc_levels){
    slab = which(abs(p-a/100)==min(abs(p-a/100))) 
    
    slabs_to_test <- rbind(slabs_to_test, data.frame(Participant = P,
                                                     Acc_level = a,
                                                     Slab = separations[slab]))
  }
}

# add in colours 
slabs_to_test$Colours <- rep(colours, max(as.numeric(slabs_to_test$Participant)))

# tidy 
rm(m, ss, a, acc_levels, p, P, separations, slab, colours)

# save this 
save(slabs_to_test, file = "scratch/data/slabs_to_test")




