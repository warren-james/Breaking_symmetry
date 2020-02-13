sim_data <- function(n_people, n_trial, n_conditions, sigma_person, sigma_person_condition)
{
  if(missing(sigma_person)){
    sigma_person <- .5
  }
  if(missing(sigma_person_condition)){
    sigma_person_condition <- .1
  }
  n_rows <- n_people * n_conditions * n_trial
  
  # apply logit function to change [0, 1] probabilities
  # into (-inf, inf) log-odds
  qs <- boot::logit(probs)
  
  
  fake_data <- tibble(
    person = rep(1:n_people, each = n_trial, n_conditions),
    condition = rep(1:n_conditions, each = n_people*n_trial),
    trial  = rep(1:n_trial, each = 1, n_people * n_conditions),
    # generate a random interecept for each person
    q_intercept = rep(rnorm(n_people, 0, sigma_person), each = n_trial, n_conditions),
    # generate a random slope for each person
    q_slope = rep(rnorm(n_people, 0, sigma_person_condition), each = n_trial, n_conditions),
    q_slope2 = if_else(condition == 1, q_slope/2, -q_slope/2),
    # combine to get q for each trial
    q =qs[condition] + q_intercept + q_slope2,
    # convert into p
    p = boot::inv.logit(q),
    # simulate response
    response = rbinom(n_rows, 1, p)) %>%
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
