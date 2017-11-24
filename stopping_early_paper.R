library(tibble)
library(dplyr)
library(ggplot2)
library(purrr)


# Simulate data
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

# Simulate bayes factors
bayes_factors <- function(N, delta, sample_mean) {
    exp((N / 2) * delta * (2 * sample_mean - delta))
}

x_0_bayes_factors <- unlist(map(.x = simulation_x_0, {function(x) bayes_factors(N, delta, x)}))
x_1_bayes_factors <- unlist(map(.x = simulation_x_1, {function(x) bayes_factors(N, delta, x)}))


# Put it all in a tibble and plot
sim <- tibble(
         bf = unlist(c(x_0_bayes_factors, x_1_bayes_factors)),
         means = c(simulation_x_0, simulation_x_1),
         test = c(rep("red", runs / 2), rep("blue", runs / 2)))

bks <- seq(min(sim$bf), max(sim$bf), by = 100)
bks <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 2.1, 10, 100, 1000, 10000) 

p <- ggplot(sim, aes(x = bf,
                fill = test)) +
       geom_histogram(alpha = 0.3,
                      position = "identity") +
       scale_x_log10(breaks = bks,
                     labels = bks) +
       theme_bw()

ggsave('histogrames.png', p, 'png') 

summary(sim$bf)


