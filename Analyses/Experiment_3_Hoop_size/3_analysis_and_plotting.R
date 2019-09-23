#### level 3 - 2nd Semester 2017/18 ####
# Analysis script 
# For their analysis, they will compare:
# Acc at easiest and hardest difficulty for Optimal and Actual Accuracy
# Probably the same thing for position (can do this, if not needed it's fine)

#### Load in the libraries ####
library(tidyverse)
library(reshape2) # might not need this one?
library(ggthemes)

# set options
options(digits = 4)

#### getting sqaured dist from switch and centre ####
load("scratch/df_part2_norm")

df_dist <- norm_dat %>% 
  mutate(dist_zero = subject_position^2,
         dist_centre = ((subject_position - shift)^2),
         baseline = shift^2) %>%
  # mutate(dist_zero = norm_dist^2,
  #        dist_centre = ((subject_position - shift)/hoop_pos)^2,
  #        baseline = (shift/hoop_pos)^2) %>%
  group_by(participant) %>% 
  summarise(zero = mean(dist_zero),
            acc = mean(dist_centre),
            baseline = mean(baseline)) %>%
  gather(zero:baseline,
         key = "distance_from",
         value = "squared_distance") %>% 
  ggplot(aes(distance_from, squared_distance)) + 
  geom_boxplot()
df_dist

#### load in wide datasets ####
# setup colnames
names <- c("participant", "minus2", "minus1", "m", "plus1", "plus2", "plus3", "type")

# Accuracy
Actual_Acc <- read.table("temp/wide_format/Actual_Acc", header = TRUE, sep = "")

Optimal_Acc <- read.csv("temp/wide_format/Optimal_Acc", header = TRUE, sep = "")

# add in type 
Actual_Acc$type <- "Actual"
Optimal_Acc$type <- "Optimal"

# Position 
Actual_Pos <- read.table("temp/wide_format/Actual_Pos", header = TRUE, sep = "")

Optimal_Pos <- read.table("temp/wide_format/Optimal_Pos", header = TRUE, sep = "")

# add in type 
Actual_Pos$type <- "Actual"
Optimal_Pos$type <- "Optimal"

# sort out column names
colnames(Actual_Acc) <- names
colnames(Optimal_Acc) <- names
colnames(Actual_Pos) <- names
colnames(Optimal_Pos) <- names

# tidy 
rm(names)

# combine the data sets 
Accuracy <- rbind(Actual_Acc, Optimal_Acc)

Position <- rbind(Actual_Pos, Optimal_Pos)

# tidy? 
rm(Actual_Acc, Actual_Pos, Optimal_Acc, Optimal_Pos)

# Should reorder the data so it can be analysed properly, apparently t.test packages
# need this, though I don't see why
Accuracy <- select(Accuracy, 
                   participant, 
                   type,
                   "minus2",
                   "minus1",
                   "m",
                   "plus1",
                   "plus2",
                   "plus3") 

Position <- select(Position, 
                   participant, 
                   type,
                   "minus2",
                   "minus1",
                   "m",
                   "plus1",
                   "plus2",
                   "plus3") 

# make sure type is a factor
Accuracy$type <- as.factor(Accuracy$type)
Position$type <- as.factor(Position$type)

# relabel as wide 
Accuracy_wide <- Accuracy
Position_wide <- Position

# tidy
rm(Accuracy, Position)

#### Load in long datasets ####
# This will probably be used for the plots...
# Accuracy
load("temp/Long_format/Act_acc_long")
load("temp/Long_format/Opt_acc_long")

# set colnames
names <- c("participant",
           "standard_sep",
           "Mean_acc")

# rename columns 
colnames(Act_acc) <- names
colnames(Opt_acc) <- names

# add in type
Act_acc$type <- "Actual"
Opt_acc$type <- "optimal"

# bind these 
Accuracy_long <- rbind(Act_acc, Opt_acc)

# tidy 
rm(Act_acc, Opt_acc)

# Optimal
load("temp/Long_format/Act_Pos_long")
load("temp/Long_format/Opt_Pos_long")

# set colnames
names <- c("participant",
           "standard_sep",
           "Mean_pos")

# rename columns 
colnames(Act_pos) <- names
colnames(Opt_pos) <- names

# add in type
Act_pos$type <- "Actual"
Opt_pos$type <- "optimal"

# bind these 
Position_long <- rbind(Act_pos, Opt_pos)

# tidy 
rm(Act_pos, Opt_pos)

#### Plots #### 
#### NOTE: REDO THESE SO THEY'RE FOR INDIVIDUALS ####
# always good to do plots first 
# This will use all the different standing positions
# Could also make accuracy version just using two separations for testing
# Maybe box plots of accuracy in most extreme separations?
# Or line plots of average accuracy accross all participants?

#### Plots: Accuracy ####
plt_acc <- Accuracy_long %>%
  group_by(type, standard_sep) %>%
  summarise(mean_acc = mean(Mean_acc),
            sd_acc = sd(Mean_acc),
            N = length(Mean_acc),
            se = sd_acc/sqrt(N)) %>%
  ggplot(aes(standard_sep, mean_acc, colour = type)) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean_acc - se,
                    ymax = mean_acc + se)) +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.5,0.75,1)) + 
  scale_x_continuous(limits = c(-2.5,3.5),
                     breaks = seq(-2,3, by = 1)) + 
  theme_bw() + 
  ggthemes::scale_colour_ptol() 
plt_acc$labels$x = "Standardised Separation"
plt_acc$labels$y = "Mean Accuracy"
plt_acc$labels$colour = "Type"
plt_acc 

#plt_acc <- plt_acc+ ggtitle("Average Accuracy for each hoop separation compared to
#            expected accuracy under the optimal strategy")
#ggsave("plots/Session_2_plot.pdf",
#      width = 10,
#       height = 8,
#       units = "in")

#### Plots: Position ####
temp <- group_by(Position_long, type, standard_sep)
plt_dat_pos <- summarise(temp, mean_pos = mean(Mean_pos),
                         sd_pos = sd(Mean_pos), 
                         N = length(Mean_pos),
                         se = sd_pos/sqrt(N))

# tidy
rm(temp)

# make plot 
# plt_pos <- Position_long %>%
#   group_by(type, standard_sep) %>%
#   summarise(mean_pos = mean(Mean_pos),
#             sd_pos = sd(Mean_pos),
#             N = length(Mean_pos),
#             se = sd/sqrt(N)) %>%
#   ggplot(aes(standard_sep-3, mean_pos, colour = type)) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = mean_pos - se,
#                     ymax = mean_pos + se)) + 
#   scale_y_continuous(limits = c(-1,1),
#                      breaks = seq(-1,1,.25)) + 
#   scale_x_continuous(limits = c(-2.5,3.5),
#                      breaks = seq(-2,3,1)) + 
#   ggtitle("Average standing position across hoop separations
#            compared to optimal standing positions") 
# plt_pos$labels$x = "Standardised Separation"
# plt_pos$labels$y = "Mean Standing Position (normalised)"
# plt_pos$labels$colour = "Type"
# plt_pos
plt_pos <- ggplot(plt_dat_pos, aes((standard_sep-3), mean_pos, colour = type))
plt_pos <- plt_pos + geom_point()
plt_pos <- plt_pos + geom_errorbar(aes(ymin = mean_pos - se,
                                       ymax = mean_pos + se))
plt_pos <- plt_pos + scale_y_continuous(limits = c(-1,1),
                                        breaks = c(-1,-.75,-.5,-.25,0,.25,.5,.75,1))
plt_pos <- plt_pos + scale_x_continuous(limits = c(-2.5,3.5),
                                        breaks = seq(-2,3, by = 1))
plt_pos <- plt_pos + labs(x = "Standardised Separation",
                          y = "Mean Standing Position (normalised)",
                          colour = "Type")
plt_pos <- plt_pos + ggtitle("Average standing position across hoop separations
           compared to optimal standing positions")

#### Level 3 analysis ####
# overall accuracy 
acc_ttest <- t.test(Mean_acc ~ type, Accuracy_long, paired = T)

# wide version 
temp <- group_by(Accuracy_long, participant, type)
Accuracy_wide_avg <- summarise(temp, Mean_acc = mean(Mean_acc)) 

# use dcast 
Accuracy_wide_avg <- dcast(Accuracy_wide_avg,
                           participant ~
                           type,
                           variabl.var = "Mean_acc")

# now try ttest again 
acc_ttest_wide <- t.test(Accuracy_wide_avg$Actual,Accuracy_wide_avg$optimal, paired = T)

# Standing pos between closest and furthest 
# prep data 
# just keep the right stuff
# closest and furthest separations
por_ttest_dat <- Position_long[Position_long$standard_sep == 1 |
                                 Position_long$standard_sep == 6,]
# Just Actual performance
por_ttest_dat <- por_ttest_dat[por_ttest_dat$type == "Actual",]

# do test for position 
por_ttest <- t.test(Mean_pos ~ standard_sep, por_ttest_dat, paired = T)

# wide version
temp <- select(por_ttest_dat, 
               participant,
               standard_sep,
               Mean_pos)

temp <- group_by(temp, participant, standard_sep)

# use dcast 
Position_wide_avg <- dcast(temp,
                           participant ~
                             standard_sep,
                           variabl.var = "Mean_pos")

# now try ttest again 
pos_ttest_wide <- t.test(Position_wide_avg[,2],Position_wide_avg[,3], paired = T)

