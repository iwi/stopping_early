library(tibble)
library(ggplot2)
library(purrr)

set.seed(1)
N <- 100
delta <- 0.2
runs <- 10000

simulation <- function(N, mean, sd) {
    x_0 <- rnorm(n = N, mean = mean, sd = sd)
    x_hat <- mean(x_0)
}

simulation_x_0 <- replicate(n = runs / 2, simulation(N, 0, 1))


simulation_x_1 <- replicate(n = runs / 2, simulation(N, delta, 1))



bf <- function(N, delta, sample_mean) {
    exp((N / 2) * delta * (2 * sample_mean - delta))
}

x_0_bayes_factors <- unlist(map(.x = simulation_x_0, {function(x) bf(N, delta, x)}))
x_1_bayes_factors <- unlist(map(.x = simulation_x_1, {function(x) bf(N, delta, x)}))

hist(x_0_bayes_factors)
sim <- tibble(
         bf = unlist(c(x_0_bayes_factors, x_1_bayes_factors)),
         means = c(simulation_x_0, simulation_x_1),
         test = c(rep(0, runs / 2), rep(1, runs / 2)))


ggplot(sim, aes(means)) +
  geom_histogram(data = subset(sim, test == 0), fill = "red", alpha = 0.2) +
  geom_histogram(data = subset(sim, test == 1), fill = "blue", alpha = 0.2)
  
summary(simulation_x_1)

