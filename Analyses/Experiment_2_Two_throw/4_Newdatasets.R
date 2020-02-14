#### Level 3 group second semster 2016/17 ####
# Two Throw experiment 
# This script is to get expected accuracy, optimal accuracy,
# and actual accuracy.
# Shouldn't be too hard...

# Written by Warren James

#### Set Options ####
options(digits = 5)

#### load libraries ####
library(tidyverse)
library(psyphy)
library(reshape2)
library(ggthemes)

#### Any Functions ####
# might be useful for making variable names in a function for making the dataframes?
# make variable from pasted elements
#assign(paste(1,2,sep="_"),dataframe)
# or 
#assign("temp",dataframe)
# both work

# retrieve the column name
#colnames(df_part1)[1]

# Useful to make a function to create the 

#### Notes #####

#### Any constants ####
Hoop_size <- 0.46

#### Load data ####
file_names <- c("scratch/df_part2_OT", "scratch/df_part2_TT","scratch/df_part1")
lapply(file_names,load,.GlobalEnv)

# tidy 
rm(file_names)

#### sort out data ####
# for some reason Accuracy isn't numeric so change that with as.numeric()
df_part2_OT$Accuracy <- as.numeric(df_part2_OT$Accuracy)
df_part2_TT$North.Accuracy <- as.numeric(df_part2_TT$North.Accuracy)
df_part2_TT$South.Accuracy <- as.numeric(df_part2_TT$South.Accuracy)

# combine part2 data? 
# So for the two hoop, you can average their accuracy and it still reflects their score in a way
df_part2_TT$Accuracy <- (df_part2_TT$North.Accuracy + df_part2_TT$South.Accuracy)/2

# select the data we want to keep
df_part2_OT <- select(df_part2_OT, 
                      Participant, 
                      Trial.no.,
                      Position, 
                      Accuracy,
                      HoopDelta,
                      abspos,
                      switchSlab)

# Add in num_throws
df_part2_OT$Num_throws <- "One"


df_part2_TT <- select(df_part2_TT, 
                      Participant, 
                      Trial.no.,
                      Position, 
                      Accuracy,
                      HoopDelta,
                      abspos,
                      switchSlab)

# Add in num_throws
df_part2_TT$Num_throws <- "Two"

# Combine them 
df_part2 <- rbind(df_part2_OT, df_part2_TT)

#### Get accuracy accross separations ####
# create dataframe
acc_sep <-tibble(Participant = character(), 
                 HoopDelta = numeric(),
                 EstimatedAcc = numeric())

# setup slab sequence 
slabs <- c(0:(2*max(df_part2$HoopDelta)))

# loop
for (x in levels(df_part1$participant)){
  # subset data
  ss = df_part1[df_part1$participant==x,]
  # get glm 
  m = glm(data=ss, cbind(inhoop, trials-inhoop)~slab,
          offset=ss$off_set,
          binomial)
  # get predictions
  p = predict(m, data.frame(slab=slabs), type="response")
  p = as.numeric(p)
  # switch point
  for(i in slabs){
    y = p[i + 1]
    acc_sep <- rbind(acc_sep, data.frame(Participant = x,
                                         HoopDelta = i,
                                         EstimatedAcc = y))
  }
}

# tidy 
rm(i, p, slabs, ss, m, x ,y)


#### Need to check they recorded the data properly ####
# Should be 72 trials in each block with 12 for each distance?
# but it doesn't look like they all did that...
# Not sure if that makes it unusable though

#### Get Optimal and Expected accuracy for each person ####
df_new <- df_part2
# add in accuracy for each hoop delta
df_new <- merge(df_new, acc_sep)

# Add in distance from each hoop
df_new$dist_s <- abs(df_new$HoopDelta - df_new$Position)
df_new$dist_n <- abs((-1*df_new$HoopDelta)-df_new$Position)

# add in optimal standing position
df_new$optpos <- df_new$HoopDelta
df_new$optpos[df_new$HoopDelta < df_new$switchSlab] <- 0

# add in optimal acc 
df_new$OptAcc <- df_new$EstimatedAcc
df_new$OptAcc[df_new$optpos == df_new$HoopDelta] <- 0.5

# Centre Strat accuracy
df_new$CentAcc <- df_new$EstimatedAcc

# save this 
save(df_new, file = "scratch/df_wit_acc")

# Now for expected accuracy 
new_acc_sep <- acc_sep
colnames(new_acc_sep) <- c("Participant",
                           "dist_s",
                           "Acc_s")

df_new <- merge(df_new, new_acc_sep)
# do north
colnames(new_acc_sep) <- c("Participant",
                           "dist_n",
                           "Acc_n")
df_new <- merge(df_new, new_acc_sep)

# Get average of this to get expected accuracy 
df_new$ExpAcc <- (df_new$Acc_s + df_new$Acc_n)/2


#### Get Accuracies ####
#### Actual Accuracy ####
# make it a tibble because that's easier to use 
df_part2 <- as.tibble(df_part2)

# summarise 
Act_Acc <- df_part2 %>%
  group_by(Participant, HoopDelta, Num_throws) %>%
  summarise(mean_acc = mean(Accuracy))
Act_Acc$Type <- "Actual"

#### Optimal Accuracy ####
Opt_Acc <- df_new %>%
  group_by(Participant, HoopDelta, Num_throws) %>%
  summarise(mean_acc = mean(OptAcc))
Opt_Acc$Type <- "Optimal"

#### Expected Accuracy ####
Exp_Acc <- df_new %>%
  group_by(Participant, HoopDelta, Num_throws) %>%
  summarise(mean_acc = mean(ExpAcc))
Exp_Acc$Type <- "Expected"

#### Centre Accuracy ####
Cent_Acc <- df_new %>%
  group_by(Participant, HoopDelta, Num_throws) %>%
  summarise(mean_acc = mean(CentAcc))
Cent_Acc$Type <- "Centre"

#### Make some plots of these? ####
# Mainly want to compare optimal to expected and centre I think
# so combine these 
plt_dat <- rbind(Opt_Acc,Exp_Acc,Cent_Acc)

plt_dat$Num_throws <- as.factor(plt_dat$Num_throws)

# make plot 
# Need to label appropriately
plt <- ggplot(plt_dat, aes(HoopDelta*Hoop_size, mean_acc, colour = Type))
plt <- plt + geom_point(position = position_jitter(width=.1,height=.0))
plt <- plt + geom_line()
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(name = "Accuracy",
                                breaks = c(.25,.75))
plt <- plt + scale_x_continuous(name = "Delta (Metres)")
plt <- plt + facet_grid(Participant ~ Num_throws)
plt
# ggsave("../../Figures/Experiment_2_Two_throw/Accuracy.png",
#        height = 21,
#        width = 13,
#        units = "cm")

# tidy 
rm(plt)

# remake this plot but keep only IS1,2, and 3 
plt_dat_reduced <- plt_dat[plt_dat$Participant == "IS1" |
                             plt_dat$Participant == "IS2" |
                             plt_dat$Participant == "IS3",]

# replot with less participants
plt <- ggplot(plt_dat_reduced, aes(HoopDelta*Hoop_size, mean_acc, colour = Type))
plt <- plt + geom_point(position = position_jitter(width=.1,height=.0))
plt <- plt + geom_line()
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(name = "Accuracy",
                                breaks = c(.25,.75))
plt <- plt + scale_x_continuous(name = "Delta (Metres)")
plt <- plt + facet_grid(Participant ~ Num_throws)
plt <- plt + theme(legend.position = "bottom")
plt$labels$colour <- "Line Type"
plt

# save 
# ggsave("../../Figures/Experiment_2_Two_throw/Accuracy(3_participants).png",
#        heigh = 12,
#        width = 16,
#        units = "cm")


#### Make area plots to fit them all on a page? ####
# setup dataframes
plt_dat_Optimal <- plt_dat[plt_dat$Type == "Optimal" & plt_dat$Num_throws != "Two",]
plt_dat_Centre <- plt_dat[plt_dat$Type == "Centre" & plt_dat$Num_throws != "Two",]



# make plot
plt <- ggplot(plt_dat, aes(HoopDelta*Hoop_size, 
                           mean_acc))#,
#fill = Pred_type))
plt <- plt + theme_bw()
plt <- plt + geom_area(data = plt_dat_Optimal, aes(HoopDelta*Hoop_size,
                                                   mean_acc),
                       fill = "blue",
                       alpha = 0.3)
plt <- plt + geom_area(data = plt_dat_Centre, aes(HoopDelta*Hoop_size,
                                                  mean_acc),
                       fill = "red",
                       alpha = 0.3)
plt <- plt + geom_line(data = plt_dat[plt_dat$Type == "Expected",], 
                       aes(linetype = Num_throws),
                       size = 1)
plt <- plt + theme(legend.position = "bottom")
# plt <- plt + geom_point()
# plt <- plt + geom_line(aes(linetype = condition))
plt <- plt + facet_wrap(~as.numeric(Participant), ncol = 6)
plt$labels$x <- "Delta (Metres)"
plt$labels$y <- "Accuracy"
plt$labels$linetype <- "Number of Throws"
plt

# save 
ggsave("../../Figures/Experiment_2_Two_throw/Accuracyshaded_regions.png",
       height = 12,
       width = 18,
       units = "cm")


#### same as above but just lines version ####
plt <- plt_dat %>%
  ungroup() %>%
  mutate(Participant = as.numeric(Participant)) %>%
  filter(Type != "Centre") %>%
  unite(Line_type, c("Num_throws", "Type")) %>%
  filter(Line_type != "One_Optimal") %>%
  ggplot(aes(HoopDelta*Hoop_size, mean_acc, colour = Line_type)) + 
  geom_line() + 
  scale_colour_ptol() + 
  theme_bw() + 
  facet_wrap(~Participant, nrow = 3) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
plt$labels$x <- "Delta (Metres)"
plt$labels$y <- "Accuracy"
plt$labels$Colour <- "Line Type"
plt
# need to sort out level labels
ggsave("../../Figures/Experiment_2_Two_throw/Accuracy_lines.png",
       height = 12, 
       width = 18,
       units = "cm")


