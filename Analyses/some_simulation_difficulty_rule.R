library(tidyverse)
library(patchwork)





# Function 1
# linear


tibble(x = c(0, 2, 3, 4,5, 7), y = c(1, 0.8, 0.7, 0.69, 0.65, 0)) %>%
	mutate(x2 = x^2, x3 = x^3) -> df

m <- lm(data = df, y ~ x +x2 + x3)
plot(seq(0, 10, 0.1), predict(m, tibble(x = seq(0, 10, 0.1), x2 = x^2, x3 = x^2)))


ggplot(df, aes(x, y))  + geom_point() + stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 3, raw=TRUE),colour="red")

p_given_distance <- function(d) {
 
e <- max(0, 1 - 0.25*d + 0.08*d^2 - 0.01*d^3)
e <- min(1, e)
return(e)

}


# generate simulted psychometric curve (assuming standing in the center)

phi = 0
deltas = seq(0, 8, 0.01)
tibble(
	delta = deltas,
	p = map_dbl(abs(phi*deltas - deltas), p_given_distance)) %>%
	ggplot(aes(x = delta, y = p)) + geom_path(colour = "white", size = 2) + 
	see::theme_blackboard() +
	ggtitle("Psychometric Curve") -> plt1




phi = seq(0, 1, 0.01)
deltas <- seq(1, 6, 0.2)
df = tibble()

for (delta in deltas) {

	p1 <- map_dbl(abs(phi*delta - delta), p_given_distance)
	p2 <- map_dbl(abs(phi*delta + delta), p_given_distance)
	df <- bind_rows(df, tibble(delta = delta, phi = phi, acc= (p1+p2)/2))  
}

df$delta <- as.factor(df$delta)
ggplot(df, aes(x  = phi, y = acc, colour = delta, group = delta)) + 
	geom_path() + 
	see::theme_blackboard() + 
	ggtitle("different standing positions") -> plt2




get_best_phi <- function(d) {
	
	df_d <-filter(df, delta == d)
	idx <- which(df_d$acc == max(df_d$acc))

	return(tibble(delta = d, phi = df_d$phi[idx], acc = max(df_d$acc)))
}

get_best_phi(4)

df_opt <- map_df(deltas, get_best_phi)

ggplot(df_opt, aes(x = delta, y = phi)) + 
	geom_path(size = 2, colour = "skyblue") + 
	see::theme_blackboard() + 
	ggtitle("optimal strategy") -> plt3

plt <- plt1 + plt2 + plt3
ggsave("breaking_assumptions.pdf", width = 10, height = 4)