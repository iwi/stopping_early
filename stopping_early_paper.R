library(tibble)
library(dplyr)
library(ggplot2)
library(purrr)


# Simulate data
set.seed(1)
max_N <- 1000
delta <- 0.2
runs <- 10000
true_mean <- 0
true_sd <- 1
threshold <- 9

simulation <- function(N, true_mean, true_sd) {
    x_0 <- rnorm(n = N, mean = true_mean, sd = true_sd)
    x_hat <- mean(x_0)
}

simulation_x_0 <- replicate(n = runs / 2, simulation(N, true_mean, true_sd))
simulation_x_1 <- replicate(n = runs / 2, simulation(N, true_mean + delta, true_sd))

# Simulate bayes factors
bayes_factor <- function(N, delta, true_mean) {
    exp((N / 2) * delta * (2 * true_mean - delta))
}

x_0_bayes_factors <- unlist(map(.x = simulation_x_0, {function(x)
                                bayes_factor(max_N, delta, x)}))
x_1_bayes_factors <- unlist(map(.x = simulation_x_1, {function(x)
                                bayes_factor(max_N, delta, x)}))


# Put it all in a tibble and plote
sim <- tibble(
         bf = unlist(c(x_0_bayes_factors, x_1_bayes_factors)),
         means = c(simulation_x_0, simulation_x_1),
         group = c(rep("control", runs / 2), rep("test", runs / 2)))

bks <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 2.1, 10, 100, 1000, 10000) 
p <-
  ggplot(sim, aes(x = bf,
                  fill = group)) +
    geom_histogram(alpha = 0.3,
                   position = "identity") +
    scale_x_log10(breaks = bks,
                  labels = bks) +
    theme_bw()

ggsave('histogrames.png', p, 'png') 



stop_early <- function(max_N, delta, true_mean, true_sd, threshold,
                       hypothesis_mean) {
  x <- rnorm(n = max_N, mean = hypothesis_mean, sd = true_sd)
  for (i in 1:max_N){
    bf <- bayes_factor(i, delta, mean(x[1:i]))
    if (bf > threshold) {
      return(i)
    }
  }
}

simulations <- 
stop_early(1000, 0, 0, 1, 9)

simulations_0 <- unlist(replicate(n = runs / 2, stop_early(max_N, delta, true_mean,
                                                  true_sd, threshold, 0)))

simulations_1 <- unlist(replicate(n = runs / 2, stop_early(max_N, delta, true_mean,
                                                  true_sd, threshold, 0.2)))

hist(simulations_1)
hist(simulations_0)
