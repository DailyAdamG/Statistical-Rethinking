#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Statistical-Rethinking/Chapter 2")

#R code 2.1

#Compute plausibilities from marble example

ways <- c(0, 3, 8, 9, 0)

ways/sum(ways)

################################################################################

#R code 2.2

#Compute likelihood of the globe tossing data with binomial distribution

dbinom(6, size = 9, prob = .5)

################################################################################

#R code 2.3

#Continuing the globe tossing example with grid approximation

#define grid

p_grid <- seq(from = 0, to = 1, length.out = 20)

#define prior

prior <- rep(1, 20)

#compute likelihood of each value in grid

likelihood <- dbinom(6, size = 9, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

################################################################################

#R code 2.4

#Plotting the posterior distribution with a grid of 20 points

plot(p_grid, posterior, type = "b",
     xlab = "probability of water", ylab = "posterior probability")

mtext("20 points")

################################################################################

#R code 2.5

#Using different priors to replicate figure 2.5 in the book

prior <- ifelse(p_grid < .5, 0, 1)

#compute likelihood of each value in grid

likelihood <- dbinom(6, size = 9, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the new posterior distribution with a grid of 20 points

plot(p_grid, posterior, type = "b",
     xlab = "probability of water", ylab = "posterior probability")

mtext("20 points")

#updated prior

prior <- exp(-5 * abs(p_grid - 0.5))

#compute likelihood of each value in grid

likelihood <- dbinom(6, size = 9, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the new posterior distribution with a grid of 20 points

plot(p_grid, posterior, type = "b",
     xlab = "probability of water", ylab = "posterior probability")

mtext("20 points")

################################################################################

#R code 2.6

#Continuing the globe problem with quadratic approximation

library(rethinking)

globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p), #binomial likelihood
    p ~ dunif(0, 1) #uniform prior
  ),
data = list(W = 6, L = 3))

#display summary of quadratic approximation

precis(globe.qa)

################################################################################

#R code 2.7

#Calculating the posterior analytically to compare how good the approximation was

W <- 6

L <- 3

curve(dbeta(x, W+1, L+1), from = 0, to = 1)

#quadratic approximation

curve(dnorm(x, .67, .16), lty = 2, add = TRUE)

################################################################################

#R code 2.8

#Find the MCMC estimate of the posterior in the globe tossing problem

n_samples <- 1000

p <- rep(NA, n_samples)

p[1] <- .5

W <- 6

L <- 3

for( i in 2:n_samples) {
  p_new <- rnorm(1, p[i-1], 0.1)
  if(p_new < 0) p_new <- abs(p_new)
    if(p_new > 1) p_new <- 2 - p_new
       q0 <- dbinom(W, W+L, p[i-1])
       q1 <- dbinom(W, W+L, p_new)
       p[i] <- ifelse(runif(1) < q1 / q0, p_new, p[i - 1])
}

################################################################################

#R code 2.9

#Comparing MCMC approximation to the analytical posterior

dens(p, xlim = c(0, 1))

curve(dbeta(x, W+1, L+1), lty = 2, add =TRUE)