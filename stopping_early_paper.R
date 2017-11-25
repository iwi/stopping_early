# Attempt to replicate the analysis in the "Continuous Monitoring of A/B Tests
# without Pain: Optional Stopping in Bayesian Testing" paper
# (https://arxiv.org/pdf/1602.05549.pdf)

# Load requirements
library(tibble)
library(dplyr)
library(ggplot2)
library(purrr)


# Initially we look at the bayes factor behaviour without early stopping.


# Simulate data
set.seed(1)
max_N <- 100
delta <- 0.2
runs <- 10000
true_mean <- 0
true_sd <- 1

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

ggsave('bayes_factor_histogram_no_early_stopping.png', p, 'png') 



###############################################################################
# Simulate runs where we're stopping early
# Stopping based on a bayes factor larger than the threshold
stop_early <- function(max_N, delta, true_mean, true_sd, threshold) {
  # simulate all normal values
  x <- rnorm(n = max_N, mean = true_mean, sd = true_sd)

  # use the mean at each iteration to calculate the bayes factor
  # and compare it with the threshold to stop early if necessary.
  for (i in 1:max_N){
    k <- bayes_factor(i, delta, mean(x[1:i]))
    if (k > threshold) {
      stop_time <- i
      early <- TRUE
      return(c(stop_time, k, early))
    }
  }
  stop_time <- i
  early <- FALSE
  return(c(stop_time, k, early))
}

# Define the bayes factor threshold
threshold <- 9

# Simulate
# Half of the runs with a true mean of 0 (control) 
# and the other half  with 0.2 the test
simulations_0 <- unlist(replicate(n = runs / 2,
                                  stop_early(max_N = max_N,
                                             delta = delta,
                                             true_mean = true_mean,
                                             true_sd = true_sd,
                                             threshold = threshold)))

simulations_1 <- unlist(replicate(n = runs / 2,
                                  stop_early(max_N = max_N,
                                             delta = delta,
                                             true_mean = true_mean + delta,
                                             true_sd = true_sd,
                                             threshold = threshold)))

# Plot two histograms with the samples coming from the test and the control
sim2 <- tibble(
          stop_early_simulations = unlist(c(simulations_0[1,], simulations_1[1,])),
          k_simulations = unlist(c(simulations_0[2,], simulations_1[2,])),
          early = unlist(c(simulations_0[3,], simulations_1[3,])),
          group = c(rep("control", runs / 2), rep("test", runs / 2)))

bks <- c(0, 1, 10, 50, 100, 500, 1000) 
p2 <-
  ggplot(sim2, aes(x = stop_early_simulations,
                  fill = group)) +
    geom_histogram(bins = 100,
                   alpha = 0.3,
                   position = "identity") +
    scale_x_log10(breaks = bks,
                  labels = bks) +
    theme_bw()

p2

bks <- c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)
p3 <-
  ggplot(sim2, aes(x = k_simulations,
                  fill = group)) +
    geom_histogram(bins = 100,
                   alpha = 0.3,
                   position = "identity") +
    scale_x_log10(breaks = bks,
                  labels = bks) +
    theme_bw()

p3

summary(as.factor(sim2$early[1:5000]))
summary(as.factor(sim2$early[5001:10000]))



###############################################################################
# Simulate runs where we're stopping early
# Stopping based on a bayes factor larger than the threshold
stop_early_two_sided <- function(max_N, delta, true_mean, true_sd, threshold) {
  # simulate all normal values
  x <- rnorm(n = max_N, mean = true_mean, sd = true_sd)

  # use the mean at each iteration to calculate the bayes factor
  # and compare it with the threshold to stop early if necessary.
  for (i in 1:max_N){
    k <- bayes_factor(i, delta, mean(x[1:i]))
    if (k > threshold or k < 1 / threshold) {
      stop_time <- i
      return(c(stop_time, k))
    }
  }
  stop_time <- i
  return(c(stop_time, k))
}

# Define the bayes factor threshold
threshold <- 9

# Simulate
# Half of the runs with a true mean of 0 (control) 
# and the other half  with 0.2 the test
simulations_0 <- unlist(replicate(n = runs / 2,
                                  stop_early_two_sided(max_N = max_N,
                                             delta = delta,
                                             true_mean = true_mean,
                                             true_sd = true_sd,
                                             threshold = threshold)))

simulations_1 <- unlist(replicate(n = runs / 2,
                                  stop_early_two_sided(max_N = max_N,
                                             delta = delta,
                                             true_mean = true_mean + delta,
                                             true_sd = true_sd,
                                             threshold = threshold)))

# Plot two histograms with the samples coming from the test and the control
sim2 <- tibble(
          stop_early_simulations = unlist(c(simulations_0[1,], simulations_1[1,])),
          k_simulations = unlist(c(simulations_0[2,], simulations_1[2,])),
          early = unlist(c(simulations_0[3,], simulations_1[3,])),
          group = c(rep("control", runs / 2), rep("test", runs / 2)))

bks <- c(0, 1, 10, 50, 100, 500, 1000) 
p2 <-
  ggplot(sim2, aes(x = stop_early_simulations,
                  fill = group)) +
    geom_histogram(bins = 100,
                   alpha = 0.3,
                   position = "identity") +
    scale_x_log10(breaks = bks,
                  labels = bks) +
    theme_bw()

p2

bks <- c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)
p3 <-
  ggplot(sim2, aes(x = k_simulations,
                  fill = group)) +
    geom_histogram(bins = 150,
                   alpha = 0.3,
                   position = "identity") +
    scale_x_log10(breaks = bks,
                  labels = bks) +
    theme_bw()

p3
