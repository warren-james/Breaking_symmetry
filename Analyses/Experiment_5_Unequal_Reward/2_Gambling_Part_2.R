#### Gambling Study - Throwing Task ####
# sort out part 2
# read it in and make some plots 

#### Notes ####
# participant 9 was tested using the wrong hoops
# They were tested using participant 7's distances so this should 
# reflected in the data...
# This only changes their R value, so it's fine for the most part 

#### Libraries ####
library(tidyverse)

#### Constants ####
slab_size <- 0.46

#### Read in data ####
# dir
results_files <- dir("data/Part2/")

# set up empty frame
df_part2 <- data.frame(Participant = character(),
                       Trial = numeric(),
                       Colour = character(),
                       Direction = character(),
                       Left_Gamble = numeric(),
                       Rght_Gamebl = numeric(),
                       Subject_Position = numeric(),
                       Accuracy = numeric(),
                       Winnings = numeric())

# loop through files 
for(f in results_files){
  d <- read.csv(paste("data/Part2/", f, sep = ""), header = T)
  
  # add in Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[2]
  
  # add in Participant
  d$Participant <- Participant
  
  # add to empty data frame 
  df_part2 <- rbind(df_part2, d)
}

# reorder 
df_part2 <- select(df_part2,
                   Participant,
                   everything())

# tidy 
rm(d, f, Participant, results_files)

#### Sort out data ####
# add in slabs tested 
# load in slabs_to_test
load("scratch/data/slabs_to_test")

# make this wide
tested_hoops <- slabs_to_test %>%
  select(-Acc_level) %>%
  spread(Colours, Slab)

# add this frame to df_part2 
df_part2 <- merge(df_part2,tested_hoops)

# Sort out participant 9's tested to reflect what they were actually tested at 
df_part2$R[df_part2$Participant == "9"] <- tested_hoops$R[tested_hoops$Participant == "7"]

# get HoopDelta
for (row in 1:nrow(df_part2))
{
  df_part2$HoopDelta[row] = df_part2[row,as.character(df_part2$Colour[row])]
}

# tidy
rm(tested_hoops,slabs_to_test,row)

# add in gamble type 
df_part2$Gamble_Type <- "Equal"
df_part2$Gamble_Type[df_part2$Left_Gamble != 25] <- "Unequal"

# get norm_dist 
df_part2$Norm_Dist <- abs(df_part2$Subject_Position/df_part2$HoopDelta)

# make participant a factor 
df_part2$Participant <- as.factor(df_part2$Participant)

# save this file 
save(df_part2, file = "scratch/data/df_part2")
