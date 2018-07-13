#### Level 3 group second semster 2016/17 ####
# Two Throw experiment 
# This is to combine the two datasets and make one graph 

# Written by Warren James

#### load libraries ####
library(tidyverse)
library(psyphy)
library(reshape2)
library(lme4)

#### Any Functions ####

#### Notes #####

#### Any constants ####
Hoop_size <- 0.46

#### Load data ####
load("scratch/df_part2_OT")
load("scratch/df_part2_TT")

#### sort data ####
# Add grouping variable
df_part2_OT$Num_throws <- "One-Throw"
df_part2_TT$Num_throws <- "Two-Throws"

# select columns to keep 
OT <- select(df_part2_OT,
             Participant,
             Num_throws,
             Trial.no.,
             Position,
             HoopDelta,
             abspos,
             switchSlab)

TT <- select(df_part2_TT,
             Participant,
             Num_throws,
             Trial.no.,
             Position,
             HoopDelta,
             abspos,
             switchSlab)

# combine
df <- rbind(OT,TT)

# save data 
df_part2 <- df
save(df_part2, file = "scratch/df_part2")

# tidy 
rm(df_part2_OT, df_part2_TT, OT, TT, df_part2)

#### PLOTS ####
#### PLOTS: Individual points for all positions #### 
plt <- ggplot(df, aes(HoopDelta*Hoop_size,
                      abspos,
                      colour = Num_throws))
plt <- plt + geom_point(position = "jitter")
plt <- plt + facet_wrap(~as.numeric(Participant), nrow = 3)
plt <- plt + theme_bw()
plt <- plt + geom_vline(aes(xintercept = switchSlab*Hoop_size),
                        linetype = "dashed")
plt <- plt + scale_y_continuous(name = "Normalised Participant standing position",
                                limits = c(0,2))
plt <- plt + scale_x_continuous(name = "Delta (Metres)",
                                limits = c(0,10),
                                breaks = c(2,4,6,8,10))
plt <- plt + theme(legend.position = "bottom")
plt$labels$colour <- "Number of Throws"
# show plot 
plt

# save this 
# ggsave("../../Figures/Experiment_2_Two_throw/Part2_BOTH_AvG_allpoints.png",
#        height = 12,
#        width = 18,
#        units = "cm")


#### PLOTS: Average standing position ####
# setup dataframe
pltdat <- as.tibble(df)
pltdat <- pltdat %>%
  group_by(Participant, HoopDelta, Num_throws, switchSlab) %>%
  summarise(mean_pos = mean(abspos))

# make plot 
plt <- ggplot(pltdat, aes(HoopDelta*Hoop_size,
                          mean_pos,
                          colour = Num_throws))
plt <- plt + geom_point(position = position_jitter(width=.1,height=.0))
plt <- plt + facet_wrap(~Participant, nrow = 3)
plt <- plt + theme_bw()
plt <- plt + geom_vline(aes(xintercept = switchSlab * Hoop_size))
plt <- plt + scale_y_continuous(name = "Normalised participant standing position",
                                limits = c(0,1))
plt <- plt + scale_x_continuous(name = "Delta (Metres)",
                                limits = c(0,10),
                                breaks = c(2,4,6,8,10))
plt$labels$colour <- "No. of Throws"
# show plot
plt

# save plot 
# ggsave("../../Figures/Experiment_2_Two_throw/Part2_BOTH_AvG.png",
#        height = 10,
#        width = 20,
#        units = "cm")



#### GLM's on standing pos etc ####
# setup data
glm_dat <- df 

m1 <- glmer(abspos ~ Num_throws + (1|Participant),
            family = gaussian(),
            data = glm_dat)
# don't think this is significant?





