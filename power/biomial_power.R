library(tidyverse)
library(lme4)

source("my_functions.R")

n_conditions <- 2

sigma_person <- 0.2

n_itr <- 10

probs <- c(0.45, 0.55)


# plot_fake_data(n_people, n_conditions, n_trial, probs) 

n_peeps = seq(10, 20, by = 2)
n_trls = seq(25, 200, by = 25)

# Loop over various numers of participants and trials
d <- tibble()
for (np in n_peeps)
{
	print(paste("running simlations for n = ", np, " people", sep = ""))
	for (trl in n_trls)
	{
		d <- bind_rows(d, itr_sim(np, trl, n_itr))
	}
}

ggplot(d, aes(x = n_people, y = power, colour = as.factor(n_trial))) + 
	geom_point() + geom_smooth()

