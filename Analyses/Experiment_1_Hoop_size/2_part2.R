#### level 3 - 2nd Semester 2017/18 ####
# Part 2 Standing position script 
# This script looks at where participants stood on each trial and 
# also where they should have stood for the optimal strategy. Also,
# it looks at where they would have stood if they were trying to 
# achieve equal accuracy for both hoop sizes.

# Written by Warren James

#### Load in the libraries ####
library(tidyverse)
library(reshape2)
library(lme4)

#### Constants ####
Hoop_size <- 0.46

#### Created functions #### 
# normalising distance function
norm_dist <- function(x,y){
  2*(x - (y*-1))/(y - (y*-1))-1
}

#### read in the data #### 
dat <- read.csv("data/Part_2/Part2_measures.txt", sep = "\t")

#### Pre-process data #### 
# sort out participant coding 
dat$participant <- paste(dat$experimenter, dat$participant, sep = "_")
dat$participant <- as.factor(dat$participant)

# drop experimenter column
dat <- dat[ , !(names(dat) %in% "experimenter")]

# make all colour values the same 
# sort red
dat$colour[dat$colour == "Red"] <- "R"

# sort yellow
dat$colour[dat$colour == "y"] <- "Y"
dat$colour[dat$colour == " Y"] <- "Y"
dat$colour[dat$colour == "Y "] <- "Y"
dat$colour[dat$colour == "Y  "] <- "Y"
dat$colour[dat$colour == "Y   "] <- "Y"
dat$colour[dat$colour == "Yellow"] <- "Y"
dat$colour[dat$colour == "Yellow "] <- "Y"
dat$colour[dat$colour == "yellow"] <- "Y"

# sort blue 
dat$colour[dat$colour == "Blue"] <- "B"
dat$colour[dat$colour == "Blue "] <- "B"


# unique order now B Y R 

# remove unused levels 
dat$colour <- factor(dat$colour)

# reorder levels
dat$colour <- factor(dat$colour,
                     levels(dat$colour)[c(1,3,2)])


# add in hoop_position 
for (row in 1:nrow(dat))
{
  dat$hoop_pos[row] = dat[row,as.character(dat$colour[row])]
}

# tidy 
rm(row) 

# create columns to fill
dat$small_pos <- 0 
dat$large_pos <- 0 



#### input small hoop pos ####
# rownames used so it doesn't loop over the first 15 rows...

for(i in levels(dat$participant)){
  a <- 10
  for(x in levels(dat$colour)){
    for(z in rownames(dat[dat$colour == x & dat$participant == i,])){
      q <- as.numeric(z)
      if(unique(dat[q,a]) == "large"){
        dat$small_pos[q] <- dat$hoop_pos[q]*-1
      } else { 
        dat$small_pos[q] <-  dat$hoop_pos[q]
      }
    }
    a <- a + 1
  }
}

# tidy
rm(a,i,q,x,z)

# add in large pos 
dat$large_pos <- dat$small_pos*-1

# save
save(dat, file = "scratch/df_part2_raw")

#### load in data from part 1 ####
# part1 acc measures
load("temp/beanbagdat")

# slabs to test 
load("temp/slabs_to_test")

#### normalising distance data ####
# this means we need to make all large hoops negative and standing towards these negative values too
# Also, make small hoops positive, and the standing positions closer to small hoops
# Can do this based on position data; probably separate it out...
norm_dat <- dat

# can probably loop through it?
for(i in levels(norm_dat$participant)){
  for(z in rownames(norm_dat[norm_dat$large_pos > 0 & norm_dat$participant == i,])){
    q <- as.numeric(z)
    norm_dat$small_pos[q] <- norm_dat$small_pos[q] *-1
    norm_dat$subject_position[q] <- norm_dat$subject_position[q]*-1
    norm_dat$large_pos[q] <- norm_dat$large_pos[q]*-1
  }
}

# tidy 
rm(i, q, z)

# apply the normalising distance function
norm_dat$norm_dist <- norm_dist(norm_dat$subject_position, norm_dat$hoop_pos)


#### work out opt switching point #### 
# create empty data.frame to get partcipants accuracy across all hoop separations 
acc_slab <- data.frame(participant = character(),
                       hoop_size = character(),
                       slab = numeric(),
                       acc = numeric())

# create data.frame for just the switch point
switch_slab <- data.frame(participant = character(),
                          hoop_size = character(),
                          slab = numeric())

# set distances to be estimated
dist_seq <- seq(0,38,0.5)

for(x in unique(beanbagdat$participant))
{
  for(i in unique(beanbagdat$hoop_size))
  {
    ss = beanbagdat[beanbagdat$participant==x & beanbagdat$hoop_size==i,]
    m = glm(data=ss, cbind(inhoop, trials-inhoop)~slab+0,
            offset=ss$off_set, binomial)
    for(a in unique(dist_seq))
    {
      p = predict(m, data.frame(slab=a), type="response")
      p = as.numeric(p)
      acc_slab <- rbind(acc_slab, data.frame(participant = x,
                                             hoop_size = i,
                                             slab = a,
                                             acc = round(p[1], digits = 7)))
    }
    slab = max(which(predict(m, data.frame(slab=dist_seq), type='response')>0.5))
    b = dist_seq[slab]
    switch_slab <- rbind(switch_slab, data.frame(participant = x, 
                                                 hoop_size = i, 
                                                 slab = b))
  }
}

# tidy 
rm(ss, m, a, i, p, x, b, slab, dist_seq)

# get switch points for small hoops 
switch_slab_sm <- switch_slab[switch_slab$hoop_size == "small",]

switch_slab_sm <- switch_slab_sm[,c("participant", 
                                    "slab")]

colnames(switch_slab_sm) <- c("participant", "switchSlab_sm")

# now for large 
switch_slab_la <- switch_slab[switch_slab$hoop_size == "large",]

switch_slab_la <- switch_slab_la[,c("participant", 
                                    "slab")]

colnames(switch_slab_la) <- c("participant", "switchSlab_la")

# get switch pos depending on average dist
# this is to reflect the point at which participants can't maintain
# an average acc of 50%
switch_slab_both <- merge(switch_slab_la, switch_slab_sm)

switch_slab_both$switchSlab <- (switch_slab_both$switchSlab_la +
                                  switch_slab_both$switchSlab_sm)/2

switch_slab_both <- switch_slab_both[,c("participant", "switchSlab")]

# create switchSlab column in norm_dat
norm_dat <- merge(switch_slab_both, norm_dat)

# tidy
rm(switch_slab, switch_slab_la, switch_slab_sm, switch_slab_both)


#### get even acc stand dist ####
# get small distances first
temp_small <- melt(slabs_to_test[slabs_to_test$hoop_size == "small",],
                   id = c("participant", "hoop_size"))
temp_small <- temp_small[,c("participant",
                            "value")]
colnames(temp_small) <- c("participant",
                          "small_hoop_dist")

# now get avg dists
temp_avg <- melt(slabs_to_test[slabs_to_test$hoop_size == "avg",],
                 id = c("participant",
                        "hoop_size"))
temp_avg <- temp_avg[,c("participant",
                        "value")]
colnames(temp_avg) <- c("participant",
                        "avg_hoop_dist")

# tidy 
rm(slabs_to_test)

# merge data sets 
temp_avg_small <- cbind(temp_avg, temp_small)
temp_avg_small <- temp_avg_small[,c("participant",
                                    "avg_hoop_dist",
                                    "small_hoop_dist")]
colnames(temp_avg_small) <- c("participant",
                              "hoop_pos",
                              "small_hoop_dist")

# get the amount of shift towards the small hoop 
temp_avg_small$shift <- temp_avg_small$hoop_pos - temp_avg_small$small_hoop_dist

# remove small_hoop_dist
temp_avg_small <- temp_avg_small[,c("participant",
                                    "hoop_pos",
                                    "shift")]

# add this into norm_dat 
norm_dat <- merge(temp_avg_small, norm_dat)

# reorder columns so it's clearer
norm_dat <- norm_dat[,c("participant",
                        "trial",
                        "colour",
                        "direction",
                        "subject_position",
                        "accuracy",
                        "R",
                        "Y",               
                        "B",
                        "B_N_Size",
                        "Y_N_Size",
                        "R_N_Size",
                        "hoop_pos",
                        "small_pos",
                        "large_pos",
                        "norm_dist",
                        "switchSlab",
                        "shift")]
# tidy 
rm(temp_avg, temp_avg_small, temp_small)

# save 
save(norm_dat, file = "scratch/df_part2_norm")

#### get standing position for equal accuracy ####
norm_dat$equacc <- norm_dist(norm_dat$shift, norm_dat$hoop_pos)

#### add in opt standing position ####
norm_dat$optpos <- norm_dat$equacc
norm_dat$optpos[norm_dat$hoop_pos > norm_dat$switchSlab] <- 1

# add in metres column
norm_dat$metres <- norm_dat$hoop_pos*Hoop_size


#### make plot of opt swithing poing by standing position ####
# still need to calculate their opt standing positions
# plt_dat <- norm_dat %>%
#   mutate(participant = as.numeric(participant)) %>%
#   select(participant,
#          trial,
#          hoop_pos,
#          norm_dist,
#          equacc,
#          optpos) %>%
#   gather(norm_dist:optpos,
#          key = "point_type", 
#          value = "norm_dist")
# 
# plt_acc <- plt_dat %>%
#   filter(point_type == "norm_dist") %>%
#   ggplot(aes(hoop_pos*Hoop_size, norm_dist)) + 
#   geom_point(alpha = 0.3) + 
#   theme_bw() work in progress here ....

temp_plt_dat <- norm_dat
temp_plt_dat$participant <- as.numeric(temp_plt_dat$participant)

plt <- ggplot(temp_plt_dat, aes(metres, norm_dist))
plt <- plt + geom_point(alpha = 0.3)
plt <- plt + theme_bw()
plt <- plt + geom_line(aes(metres, equacc,
                           colour = "Equal Accuracy"),
                       size = 1.2,
                       alpha = 0.7)
plt <- plt + geom_line(aes(metres, optpos,
                           colour = "Optimal"),
                       size = 1.2,
                       alpha = 0.7)
plt <- plt + geom_point(aes(metres, optpos,
                            colour = "Optimal"))
plt <- plt + geom_point(aes(metres, equacc,
                            colour = "Equal Accuracy"))
# plt <- plt + geom_vline(xintercept = norm_dat$switchSlab)
plt <- plt + scale_y_continuous(name="Normalised Participant Position", limits=c(-1,1))
plt <- plt + scale_x_continuous(name="Delta (Metres)" ,
                                limits= c(1.5, 10),
                                breaks = c(2,4,6,8,10))
plt <- plt + geom_hline(yintercept = 0)
plt <- plt + facet_wrap(~participant, ncol = 7)
plt <- plt + theme(legend.position = "bottom",
                   strip.text.x = element_blank())
                   #strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))

# Change legend title 
plt$labels$colour <- "Line Type"
plt
#### save plot to figures folder #### 
# ggsave("../../Figures/Experiment_3_Hoop_size/Session_2_plot.png",
#        width = 18,
#        height = 16,
#        units = "cm")

# tidy 
rm(plt, temp_plt_dat)


#### proportion of standing positions ####
norm_dat$prop_sizes <- 0 
norm_dat$prop_sizes[norm_dat$subject_position > 0] <- 1 
norm_dat$prop_sizes[norm_dat$subject_position < 0] <- (-1) 

# get summary of this for each participant and distance 
# create df for this 
prop_sides <- data.frame(participant = character(),
                         hoop_pos = numeric(),
                         prop_sizes = numeric(),
                         prop = numeric(), 
                         count = numeric())

for(i in unique(norm_dat$participant)){
  for(x in unique(norm_dat$hoop_pos[norm_dat$participant == i])){
    for(z in unique(norm_dat$prop_sizes)){
      temp_num <- (nrow(norm_dat[norm_dat$participant == i &
                                   norm_dat$hoop_pos == x,]))
      prop <- (nrow(norm_dat[norm_dat$participant == i &
                                norm_dat$hoop_pos == x &
                                norm_dat$prop_sizes == z,])/temp_num)
      count <- nrow(norm_dat[norm_dat$participant == i &
                                norm_dat$hoop_pos == x &
                                norm_dat$prop_sizes == z,])
      prop_sides <- rbind(prop_sides, data.frame(participant = i,
                                                 hoop_pos = x,
                                                 prop_sizes = z,
                                                 prop = prop,
                                                 count = count))
    }
  }
}

# tidy 
rm(count, i, prop, temp_num, x, z)

prop_sides$prop_sizes <- as.factor(prop_sides$prop_sizes)
prop_sides$stpos_type <- "Centre"
prop_sides$stpos_type[prop_sides$prop_sizes == "-1"] <- "Large Hoop"
prop_sides$stpos_type[prop_sides$prop_sizes == "1"] <- "Small Hoop"


prop_sides$metres <- prop_sides$hoop_pos*Hoop_size

prop_sides$participant <- as.numeric(prop_sides$participant)

#### Make the proportion plot ####
prop_plt <- ggplot(data = prop_sides, 
                   aes(x = metres,
                       y = prop))
prop_plt <- prop_plt + theme_bw()
prop_plt <- prop_plt + geom_area(aes(colour = stpos_type,
                                     fill = stpos_type),
                                 position = "stack")
prop_plt <- prop_plt + scale_y_continuous(name="Normalised Participant Position",
                                          limits=c(0,1))
prop_plt <- prop_plt + scale_x_continuous(name="Delta (Metres)",
                                limits= c(1.5, 10),
                                breaks = c(2,4,6,8,10))
prop_plt <- prop_plt + theme(legend.position = "bottom",
                             strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
# prop_plt <- prop_plt + scale_fill_discrete(breaks = c("Large Hoop",
#                                                       "Centre",
#                                                       "Small Hoop"))
# prop_plt <- prop_plt + scale_color_discrete(breaks = c("Large Hoop",
#                                                        "Centre",
#                                                        "Small Hoop"))
#prop_plt <- prop_plt + geom_vline(data = switch_points,
#                                  aes(xintercept = as.numeric(switch_point)), 
#                                  linetype = "dashed")
prop_plt <- prop_plt + facet_wrap(~participant, ncol = 7)
prop_plt$labels$colour <- "Stood Towards"
prop_plt$labels$fill <- "Stood Towards"
# Draw plot
prop_plt

# Save plot
# ggsave("../../Figures/Experiment_3_Hoop_size/Session_2_proportions.png",
#        width = 18,
#        height = 16,
#        units = "cm")

#### get t-test results to look at proportions ####
# sort out the data frame for this 
bias_sides <- prop_sides[prop_sides$stpos_type != "Centre",]

bias_sides_2 <- bias_sides %>%
  group_by(participant, stpos_type) %>% 
  summarise(total = sum(count)) %>%
  mutate(prop = total/sum(total))

# now do some tests I think...
t.test(prop ~ stpos_type, data = bias_sides_2)
# shows it significant
# but this is a bad way to do it and you know that....

#### GLMER ####
# setup the data
glm_dat <- dat[dat$subject_position != 0,]

# now get if they were closer to the small hoop or not 
glm_dat$small_hoop_left <- 0
glm_dat$small_hoop_left[glm_dat$small_pos < 0] <- 1

glm_dat$subj_left <- 0 
glm_dat$subj_left[glm_dat$subject_position < 0] <- 1

# sort out factors
glm_dat$small_hoop_left <- as.factor(glm_dat$small_hoop_left)


m1 <- glmer(subj_left ~ small_hoop_left + (1|participant),
          family = binomial(),
          data = glm_dat)
summary(m1)
# shows sig effect 

# do we need a varying slope model as well... 
# should probably look at separation as well...



# tidy 
rm(prop_plt)

# # get summaries of counts for each participant to check against level 3's
# temp <- group_by(prop_sides, participant, prop_sizes)
# compare_dat <- summarise(temp, count = sum(count))
# 
# # tidy 
# rm(temp)
# # so it was my mistake in the 'input small hoop pos' part



#### Accuracy: Get accuarcy for each participant in each strategy ####
# separate acc_slab files 
small_acc_slab <- acc_slab[acc_slab$hoop_size == "small",]

large_acc_slab <- acc_slab[acc_slab$hoop_size == "large",]

# remove hoop_size from both as we know what they are, makes merging easier 
small_acc_slab <- select(small_acc_slab,
                         participant, 
                         slab,
                         acc)

large_acc_slab <- select(large_acc_slab,
                         participant, 
                         slab,
                         acc)

# reduce number of columns because we don't need them all 
reduced_norm_dat <- select(norm_dat,
                           participant,
                           subject_position,
                           shift,
                           accuracy,
                           hoop_pos,
                           small_pos,
                           large_pos)

# sort out participant

# will need to normalise distances again, but we don't need that for now 

# tidy
rm(acc_slab)

# want to subtract standing pos from distance to get the distance from each hoop
# then get absolute value obvs

#### Centre (distance) ####
# sort out acc_slab dataframes so they match up 
cssm <- small_acc_slab

# rename acc column 
colnames(cssm) <- c("participant",
                    "hoop_pos",
                    "csm_acc")
 
# large
csla <- large_acc_slab

# rename columns 
colnames(csla) <- c("participant",
                    "hoop_pos",
                    "cla_acc")

# merge these? 
temp <- merge(csla, cssm)

# add into norm_dat
reduced_norm_dat <- merge(reduced_norm_dat, temp)

# tidy 
rm(temp, large_acc_slab, small_acc_slab)

# get average accuracy of this strategy 
reduced_norm_dat$cavg_acc <- (reduced_norm_dat$cla_acc + reduced_norm_dat$csm_acc)/2

#### Expected Accuracy ####
# need to work out distances for each hoop 
reduced_norm_dat$small_dist <- abs(reduced_norm_dat$small_pos - reduced_norm_dat$subject_position)
reduced_norm_dat$large_dist <- abs(reduced_norm_dat$large_pos - reduced_norm_dat$subject_position)

# use csla and cssm data again
exla <- csla
exsm <- cssm

# tidy 
rm(csla,cssm)

# rename columns 
colnames(exla) <- c("participant",
                    "large_dist",
                    "exla_acc")

colnames(exsm) <- c("participant",
                    "small_dist",
                    "exsm_acc")

# merge with main dataset 
reduced_norm_dat <- merge(reduced_norm_dat, exsm)
reduced_norm_dat <- merge(reduced_norm_dat, exla)

# get avg accuracy from this now 
reduced_norm_dat$exavg_acc <- (reduced_norm_dat$exla_acc + reduced_norm_dat$exsm_acc)/2

#### Centre (equal accuracy) ####
# get centre for equal accuracy distances 
reduced_norm_dat$small_dist_eq <- (reduced_norm_dat$small_pos - reduced_norm_dat$shift)
reduced_norm_dat$large_dist_eq <- abs(reduced_norm_dat$large_pos - reduced_norm_dat$shift)

# use exla and exsm data again
eqla <- exla
eqsm <- exsm

# tidy 
rm(exla,exsm)

# rename columns 
colnames(eqla) <- c("participant",
                    "large_dist_eq",
                    "eqla_acc")

colnames(eqsm) <- c("participant",
                    "small_dist_eq",
                    "eqsm_acc")

# merge with main dataset 
reduced_norm_dat <- merge(reduced_norm_dat, eqsm)
reduced_norm_dat <- merge(reduced_norm_dat, eqla)

# get avg accuracy from this now 
reduced_norm_dat$eqavg_acc <- (reduced_norm_dat$eqla_acc + reduced_norm_dat$eqsm_acc)/2


#### Optimal Accuracy ####
# make opt standing pos 
reduced_norm_dat$opt_pos <- reduced_norm_dat$shift
reduced_norm_dat$opt_pos[reduced_norm_dat$eqavg_acc < 0.5] <- reduced_norm_dat$hoop_pos[reduced_norm_dat$eqavg_acc < 0.5]

# get distance from small and large hoops 
reduced_norm_dat$small_dist_opt <- (reduced_norm_dat$hoop_pos - reduced_norm_dat$opt_pos)
reduced_norm_dat$large_dist_opt <- abs(reduced_norm_dat$large_pos - reduced_norm_dat$opt_pos)

# get accuracy for large and small hoop
# can reuse eqla and eqsm for this part 
opla <- eqla
opsm <- eqsm

# tidy 
rm(eqla, eqsm)

# rename columns
colnames(opla) <- c("participant",
                    "large_dist_opt",
                    "opla_acc")

colnames(opsm) <- c("participant",
                    "small_dist_opt",
                    "opsm_acc")

# merge with reduce_norm_dat
reduced_norm_dat <- merge(reduced_norm_dat, opla)
reduced_norm_dat <- merge(reduced_norm_dat, opsm)

# tidy 
rm(opla, opsm)

# get avg acc 
reduced_norm_dat$opt_avg <- (reduced_norm_dat$opla_acc + reduced_norm_dat$opsm_acc)/2

#### new data set with only need columns ####
Acc_dat <- select(reduced_norm_dat,
                  participant,
                  accuracy,
                  hoop_pos,
                  cavg_acc,
                  exavg_acc,
                  eqavg_acc,
                  opt_avg)

# get averages
Acc_dat_wide <- Acc_dat %>% 
  group_by(participant, hoop_pos) %>%
  summarise(Optimal = mean(opt_avg),
            #Actual = mean(accuracy),
            "Centre (Distance)" = mean(cavg_acc),
            Expected = mean(exavg_acc),
            "Centre (Probability)" = mean(eqavg_acc))

# make suitable for plotting (long in this case)
plt_data <- gather(Acc_dat_wide, Acc_type, Accuracy, Optimal:"Centre (Probability)", factor_key = T)

#### Plot of Accuracy types ####
plt <- ggplot(plt_data, aes(hoop_pos*Hoop_size, Accuracy, colour = Acc_type))
plt <- plt + geom_point()
plt <- plt + geom_line()
plt <- plt + facet_wrap(~participant, ncol = 6)
plt <- plt + theme(legend.position = "bottom")
plt$labels$x <- "Delta (metres)"
plt$labels$colour <- "Line Type"
plt
# ggsave("../../Figures/Experiment_3_Hoop_size/Session_2_plot_Accuracy.png",
#        width = 20,
#        height = 13,
#        units = "cm")

# tidy 
rm(plt)

#### new version of above plot ####
plt <- plt_data %>%
  filter(Acc_type == "Optimal" | Acc_type == "Expected") %>%
  ggplot(aes(hoop_pos*Hoop_size, Accuracy, colour = Acc_type)) + 
  geom_line() + 
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_blank()) + 
  ggthemes::scale_colour_ptol() + 
  facet_wrap(~participant, ncol = 7)
plt$labels$x <- "Detla (Metres)"
plt$labels$colour <- "Type"
plt


#### plot Accuracy types with regions instead ####
# setup dataframes
plt_dat_Optimal <- plt_data[plt_data$Acc_type == "Optimal",]
plt_dat_Centre <- plt_data[plt_data$Acc_type == "Centre (Distance)",]



# make plot
# plt <- plt_data %>%
#   ggplot(aes(hoop_pos*Hoop_size, 
#              Accuracy)) + 
#   theme_bw() + 
#   geom_area(data = plt_dat_Optimal,
#             aes(hoop_pos*Hoop_size,
#                 Accuracy),
#             fill = "blue",
#             alpha = 0.3) + 
#   geom_area(data = plt_dat_Centre,
#             aes(hoop_pos*Hoop_size,
#                 Accuracy),
#             fill = "red",
#             alpha = 0.3) + 
  
plt <- ggplot(plt_data, aes(hoop_pos*Hoop_size, 
                            Accuracy))
plt <- plt + theme_bw()
plt <- plt + geom_area(data = plt_dat_Optimal, aes(hoop_pos*Hoop_size,
                                                   Accuracy),
                       fill = "blue",
                       alpha = 0.3)
plt <- plt + geom_area(data = plt_dat_Centre, aes(hoop_pos*Hoop_size,
                                                  Accuracy),
                       fill = "red",
                       alpha = 0.3)
plt <- plt + geom_line(data = plt_data[plt_data$Acc_type == "Expected" |
                                         plt_data$Acc_type == "Centre (Probability)",], 
                       aes(linetype = Acc_type),
                       size = 1)
plt <- plt + theme(legend.position = "bottom",strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
# plt <- plt + geom_point()
# plt <- plt + geom_line(aes(linetype = condition))
plt <- plt + facet_wrap(~as.numeric(participant), ncol = 7)
plt$labels$x <- "Delta (Metres)"
plt$labels$y <- "Accuracy"
plt$labels$linetype <- "Accuracy for:"
plt

# save 
# ggsave("../../Figures/Experiment_3_Hoop_size/Accuracyshaded_regions.png",
#        height = 12,
#        width = 18,
#        units = "cm")



#### Standardised distances ####
for(i in unique(Acc_dat$participant)){
  Acc_dat$standard_sep[Acc_dat$participant == i] <- as.numeric(as.factor(Acc_dat$hoop_pos[Acc_dat$participant == i]))
}

# tidy 
rm(i)

# centre this on slab m 
Acc_dat$standard_sep <- Acc_dat$standard_sep - 3

#### Accuracy: Create new data sets for level 3's to use ####
# These should be an accuracy level for each separation dependent on type
# so, one for: - Actual Accuracy
#              - Expected Actual Accuracy
#              - Centre strat (equidistant)
#              - Centre strat (equal accuracy)
#              - Optimal Strategy
# Need to create a data set with averages for each one then make the files 

#### Accuracy: getting mean scores for each type ####
# Actual Accuracy
temp <- group_by(Acc_dat, participant, standard_sep)
Act_acc <- summarise(temp, mean_Act_acc = mean(accuracy))

# Expected Actual Accuracy
Exp_acc <- summarise(temp, mean_Exp_acc = mean(exavg_acc))

# Centre strat
Cen_acc <- summarise(temp, mean_Cen_acc = mean(cavg_acc))

# Equidistant
Eq_acc <- summarise(temp, mean_Eq_acc = mean(eqavg_acc))

# optimal
Opt_acc <- summarise(temp, mean_Opt_acc = mean(opt_avg))

# tidy 
rm(temp)

#### Save long format: Accuracy ####
save(Act_acc, file = "temp/Long_format/Act_acc_long")
save(Cen_acc, file = "temp/Long_format/Cen_acc_long")
save(Opt_acc, file = "temp/Long_format/Opt_acc_long")
save(Eq_acc, file = "temp/Long_format/Eq_acc_long")
save(Exp_acc, file = "temp/Long_format/Exp_acc_long")


#### Accuracy: Make wide format ####
# Should they all be combined?
# That would make sense, but for now, I can just make separate 
# ones and then combine the ones they will need for their analysis
# Actual
Actual_Wide_Acc <- dcast(Act_acc, 
                         participant ~
                           standard_sep,
                         variabl.var = "mean_Act_acc")
# Expected Actual
Expected_Wide_Acc <- dcast(Exp_acc,
                           participant ~
                             standard_sep,
                           variabl.var = "mean_Exp_acc")

# Centre strat
Centre_Wide_Acc <- dcast(Cen_acc,
                         participant ~
                           standard_sep,
                         variabl.var = "mean_Cen_acc")

# Equidistant
Equi_Wide_Acc <- dcast(Eq_acc,
                       participant ~
                         standard_sep,
                       variabl.var = "mean_Eq_acc")

# optimal
Optimal_Wide_Acc <- dcast(Opt_acc,
                          participant ~
                            standard_sep,
                          variabl.var = "mean_Opt_acc")

#### Accuracy: Save files ####
# Actual
write.table(Actual_Wide_Acc, "temp/Wide_format/Actual_Acc", row.names = FALSE, sep = "\t")

# Expected
write.table(Expected_Wide_Acc, "temp/Wide_format/Expected_Acc", row.names = FALSE, sep = "\t")

# Equidistant
write.table(Equi_Wide_Acc, "temp/Wide_format/Equidistant_Acc", row.names = FALSE, sep = "\t")

# Centre
write.table(Centre_Wide_Acc, "temp/Wide_format/Centre_Acc", row.names = FALSE, sep = "\t")

# Optimal
write.table(Optimal_Wide_Acc, "temp/Wide_format/Optimal_Acc", row.names = FALSE, sep = "\t")

# tidy 
rm(Cen_acc,
   Centre_Wide_Acc,
   Act_acc,
   Actual_Wide_Acc,
   Eq_acc,
   Equi_Wide_Acc,
   Exp_acc,
   Expected_Wide_Acc,
   Opt_acc,
   Optimal_Wide_Acc)



#### Standing posistion: Get standing postion for each participant (normalise) ####
Stn_dat <- select(reduced_norm_dat,
                  participant,
                  hoop_pos,
                  opt_pos,
                  shift,
                  subject_position)

# add in centre 
Stn_dat$centre <- 0

# get standardised distances
for(i in unique(Stn_dat$participant)){
  Stn_dat$standard_sep[Stn_dat$participant == i] <- as.numeric(as.factor(Stn_dat$hoop_pos[Stn_dat$participant == i]))
}

# normalise the distances
# might need to loop through so we get each column changed
index <- c(3,4,5)

for(i in unique(Stn_dat$participant)){
  b <- 1
  for(a in unique(index)){
    Stn_dat[(7 + b)] <- norm_dist(Stn_dat[a], Stn_dat$hoop_pos)
    b <- b + 1  
  }
}

# rename columns 
colnames(Stn_dat) <- c("participant",
                       "hoop_pos",
                       "opt_pos",
                       "shift",
                       "subject_pos",
                       "centre",
                       "standard_sep",
                       "norm_opt",
                       "norm_equ",
                       "norm_act")

#### Standing position: get mean values ####
# not the best measure, but will do for level 3's
temp <- group_by(Stn_dat, participant, standard_sep)

# Actual
Act_pos <- summarise(temp, mean_Act_pos = mean(norm_act))

# Centre strat
Cen_pos <- summarise(temp, mean_Cen_pos = mean(centre))

# Equidistant
Eq_pos <- summarise(temp, mean_Eq_pos = mean(norm_equ))

# optimal
Opt_pos <- summarise(temp, mean_Opt_pos = mean(norm_opt))


#### Save long format: Position ####
save(Act_pos, file = "temp/Long_format/Act_pos_long")
save(Opt_pos, file = "temp/Long_format/Opt_pos_long")
save(Cen_pos, file = "temp/Long_format/Cen_pos_long")
save(Eq_pos, file = "temp/Long_format/Eq_pos_long")

#### Standing position: Make wide format ####
# Actual
Actual_Wide_Pos <- dcast(Act_pos,
                     participant ~
                       standard_sep,
                     variabl.var = "mean_Act_pos")

# Centre strat
Centre_Wide_Pos <- dcast(Cen_pos,
                     participant ~
                       standard_sep,
                     variabl.var = "mean_Cen_pos")

# Equidistant
Equi_Wide_Pos <- dcast(Eq_pos,
                   participant ~
                     standard_sep,
                   variabl.var = "mean_Eq_pos")

# optimal
Optimal_Wide_Pos <- dcast(Opt_pos,
                      participant ~
                        standard_sep,
                      variabl.var = "mean_Opt_pos")

#### Standing position: Save files ####
# Actual
write.table(Actual_Wide_Pos, "temp/Wide_format/Actual_Pos", row.names = FALSE, sep = "\t")

# Equidistant
write.table(Equi_Wide_Pos, "temp/Wide_format/Equidistant_Pos", row.names = FALSE, sep = "\t")

# Centre
write.table(Centre_Wide_Pos, "temp/Wide_format/Centre_Pos", row.names = FALSE, sep = "\t")

# Optimal
write.table(Optimal_Wide_Pos, "temp/Wide_format/Optimal_Pos", row.names = FALSE, sep = "\t")


# Should be done now, may need to combine certain sets together, but it's done for now



