#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Statistical-Rethinking/Chapter 3")

#R code 3.1

#Finding the probability that someone who tests positive is actually a vampire

Pr_Positive_Vampire <- 0.95

Pr_Positive_Mortal <- 0.01

Pr_Vampire <- 0.001

Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire + 
  Pr_Positive_Mortal * (1 - Pr_Vampire)
(Pr_Vampire_Positive <- Pr_Positive_Vampire * Pr_Vampire / Pr_Positive)

################################################################################

#R code 3.2

#Compute the posterior for the globe tossing model from the previous chapter

p_grid <- seq(from = 0, to = 1, length.out = 1000)

prob_p <- rep(1, 1000)

prob_data <- dbinom(6, 9, prob = p_grid)

posterior <- prob_data * prob_p

posterior <- posterior / sum(posterior)

################################################################################

#R code 3.3

#Creating a sample that has the same proportions as the posterior density

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

################################################################################

#R code 3.4

#Plot of all samples

plot(samples)

################################################################################

#R code 3.5

#Plot showing the density estimate computed from the samples

library(rethinking)
dens(samples)

################################################################################

#R code 3.6

#Find the probability that the proportion of water is below 50%

#add up posterior probability where p < 0.5

sum(posterior[p_grid < 0.5])

################################################################################

#R code 3.7

#Find the frequency of parameter values below 0.5

sum(samples < 0.5) / 1e4

################################################################################

#R code 3.8

#Find how much posterior probability lies between 0.5 and 0.75

sum(samples > 0.5 & samples < 0.75) / 1e4

################################################################################

#R code 3.9

#Finding the boundaries of the lower 80% posterior probability

quantile(samples, 0.8)

################################################################################

#R code 3.10

#Finding the boundaries of the middle 80% posterior probability

quantile(samples, c(0.1, 0.9))

################################################################################

#R code 3.11

#Computing posterior with uniform prior and observing 3 waters on 3 tosses

p_grid <- seq(from = 0, to = 1, length.out = 1000)

prior <- rep(1, 1000)

likelihood <- dbinom(3, size = 3, prob = p_grid)

posterior <- likelihood * prior

posterior <- posterior / sum(posterior)

samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

################################################################################

#R code 3.12

#Compute the 50% percentile compatibility interval

PI(samples, prob = 0.5)

################################################################################

#R code 3.13

#Compute the highest posterior density interval

HPDI(samples, prob = 0.5)

################################################################################

#R code 3.14

#Compute the a maximum a posteriori (MAP) estimate for the 3 waters in 3 tosses example

p_grid[which.max(posterior)]

################################################################################

#R code 3.15

#Approximate the MAP using samples instead

chainmode(samples, adj = 0.01)

################################################################################

#R code 3.16

#Find the posterior mean and median as well

mean(samples)

median(samples)

################################################################################

#R code 3.17

#Calculate expected loss using p = 0.5

sum(posterior * abs(0.5 - p_grid))

################################################################################

#R code 3.18

#Create a function to find the expected loss for every value

loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))

################################################################################

#R code 3.19

#Find the parameter value that minimizes the loss

p_grid[which.min(loss)]

################################################################################

#R code 3.20

#Computing the probability of observing 0, 1, and 2 instances of water when p = 0.7

dbinom(0:2, size = 2, prob = 0.7)

################################################################################

#R code 3.21

#Single dummy observation

rbinom(1, size = 2, prob = 0.7)

################################################################################

#R code 3.22

#Generate 10 dummy observations

rbinom(10, size = 2, prob =0.7)

################################################################################

#R code 3.23

#Generate 100,000 dummy observations

dummy_w <- rbinom(1e5, size = 2, prob = 0.7)

table(dummy_w) / 1e5

################################################################################

#R code 3.24

#Redoing the original globe problem with 100,000 dummy observations and 9 tosses

dummy_w <- rbinom(1e5, size = 9, prob = 0.7)

simplehist(dummy_w, xlab = "dummy water count")

################################################################################

#R code 3.25

#Simulating predicted observations for p = 0.6

w <- rbinom(1e4, size = 9, prob = 0.6)

################################################################################

#R code 3.26

#Propagate parameter uncertainty into these predictions by replacing 0.6 with samples
#from the posterior

w <- rbinom(1e4, size = 9, prob = samples)