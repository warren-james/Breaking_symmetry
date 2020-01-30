library(tidyverse)
library(lme4)


n_conditions <- 2

sigma_person <- 0.2

n_itr <- 10

probs <- c(0.45, 0.55)

sim_data <- function(n_people, n_trial, n_conditions)
{
	n_rows <- n_people * n_conditions * n_trial
	

	fake_data <- tibble(
			person = rep(1:n_people, each = n_trial, n_conditions),
			condition = rep(1:n_conditions, each = n_people*n_trial),
			trial  = rep(1:n_trial, each = 1, n_people * n_conditions),
			p_person = probs[condition] + rep(rnorm(n_people, 0, sigma_person), each = n_trial, n_conditions),
			response = if_else(runif(n_rows) < p_person, 1, 0)) %>%
		mutate(
			condition = as.factor(condition),
			person = as.factor(person))

	return(fake_data)
}


plot_fake_data <- function(n_people, n_conditions, n_trial, probs) 
{
	fake_data <- sim_data(n_people, n_trial, n_conditions)

	fake_data %>% 
		group_by(condition, person) %>%
		summarise(proportion = mean(response)) -> fake_data

	ggplot() +
		geom_boxplot(data = fake_data, aes(x = condition, y = proportion)) +
			ggtitle("Some simulated data") +
			geom_point(aes(x = c(1,2), y = probs), size = 3, colour = "red")
}


sim_exp <- function(n_people, n_trial)
{
	fake_data <- sim_data(n_people, n_trial, n_conditions)

	m <- glmer(data = fake_data, response ~ condition + (1|person), family = "binomial")
	p <- coefficients(summary(m))[2,4]

	return(p)
}

itr_sim <- function (n_people, n_trial, n_itr = 100, alpha = 0.05)
{
	p_values <- map_dbl(1:n_itr, ~ sim_exp(n_people=n_people, n_trial=n_trial))
	pwr <- tibble(n_people = n_people, n_trial = n_trial, power = mean(p_values < alpha))

	return(pwr)
}

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

