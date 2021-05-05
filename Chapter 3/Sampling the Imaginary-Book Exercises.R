#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Statistical-Rethinking/Chapter 3")

#Load necessary library

library(rethinking)

#Code used for Easy questions

p_grid <- seq(from = 0, to = 1, length.out = 1000)

prior <- rep(1, 1000)

likelihood <- dbinom(6, size = 9, prob = p_grid)

posterior <- likelihood * prior

posterior <- posterior / sum(posterior)

set.seed(100)

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

#3E1. How much posterior probability lies below p = 0.2?

sum(samples < 0.2) / 1e4

################################################################################

#3E2. How much posterior probability lies above p = 0.8?

sum(samples > 0.8) / 1e4

################################################################################

#3E3. How much posterior probability lies between p = 0.2 and p = 0.8?

sum(samples > 0.2 & samples < 0.8) / 1e4

################################################################################

#3E4. 20% of the posterior probability lies below which value of p?

quantile(samples, 0.2)

################################################################################

#3E5. 20% of the posterior probability lies above which value of p?

quantile(samples, 0.8)

################################################################################

#3E6. Which values of p contain the narrowest interval equal to 66% of the posterior
#probability?

HPDI(samples, prob = 0.66)

################################################################################

#3E7. Which values of p contain 66% of the posterior probability, assuming equal
#posterior probability both below and above the interval?

PI(samples, prob = 0.66)

################################################################################

#3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
#Construct the posterior distribution using grid approximation. Use the same flat
#prior as before.

p_grid <- seq(from = 0, to = 1, length.out = 1000)

prior <- rep(1, 1000)

likelihood <- dbinom(8, size = 15, prob = p_grid)

posterior <- likelihood * prior

posterior <- posterior / sum(posterior)

plot(posterior ~ p_grid, type = "l")

################################################################################

#3M2. Draw 10,000 samples from the grid approximation from above. Then use the
#samples to calculate the 90% HPDI for p.

samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

HPDI(samples, prob = 0.9)

################################################################################

#3M3. Construct a posterior predictive check for this model and data. This means
#simulate the distribution of samples, averaging over the posterior uncertainty
#in p. What is the probability of observing 8 water in 15 tosses?

w <- rbinom(1e4, size = 15, prob = samples)

sum(w == 8) / 1e4

simplehist(w)

################################################################################

#3M4. Using the posterior distribution constructed from the new (8/15) data, now
#calculate the probability of observing 6 water in 9 tosses.

w <- rbinom(1e4, size = 9, prob = samples)

sum(w == 6) / 1e4

################################################################################

#3M5. Start over at 3M1, but now use a prior that is zero below p = 0.5 and a
#constant above p = 0.5. This corresponds to prior information that a majority of
#the Earth's surface is water. Repeat each problem above and compare the inferences.
#What difference does the better prior make? If it helps, compare inferences (using
#both priors) to the true value of p = 0.7.

p_grid <- seq(from = 0, to = 1, length.out = 1000)

prior <- ifelse(p_grid < .5, 0, 1)

likelihood <- dbinom(8, size = 15, prob = p_grid)

posterior <- likelihood * prior

posterior <- posterior / sum(posterior)

plot(posterior ~ p_grid, type = "l")

#The prior makes it so anything below 0.5 is impossible. This makes it so the model
#does not completely trust the data and the histogram is similar to the original
#until reaching 0.5 on the x-axis of the histogram.

################################################################################

#Code used for Hard questions

data(homeworkch3)

#3H1. Using grid approximation, compute the posterior distribution for the probability
#of a birth being a boy. Assume a uniform prior probability. Which parameter value
#maximizes the posterior probability?

p_grid <- seq(from = 0, to = 1, length.out = 1000)

prior <- rep(1, 1000)

boys <- sum(birth1) + sum(birth2)

likelihood <- dbinom(boys, size = 200, prob = p_grid)

posterior <- likelihood * prior

posterior <- posterior / sum(posterior)

plot(posterior ~ p_grid, type = "l")

p_grid[which.max(posterior)]

################################################################################

#3H2. Using the sample function, draw 10,000 random parameter values from the posterior
#distribution you calculated above. Use these samples to estimate the 50%, 89%,
#and 97% highest posterior density intervals.

samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

HPDI(samples, prob = .5)

HPDI(samples, prob = .89)

HPDI(samples, prob = .97)

################################################################################

#3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up
#with 10,000 numbers, each one a count of boys out of 200 births. Compare the 
#distribution of predicted numbers of boys to the actual count in the data (111 boys
#out of 200 births). There are many good ways to visualize the simulations, but
#the dens command (part of the rethinking package) is probably the easiest way in
#this case. Does it look like the model fits the data well? That is, does the
#distribution of predictions include the actual observation as a central, likely
#outcome?

boy_sim <- rbinom( 1e4, size = 200, prob = samples)

dens(boy_sim, adj = 0.1)
abline(v = 111, col = "red")

#The prediction looks pretty accurate.The histogram is centered around the actual
#observed births of 111.

################################################################################

#3H4. Now compare 10,000 counts of boys from 100 simulated first borns only to the
#number of boys in the first births, birth1. How does the model look in this light?

first_boy_sim <- rbinom(1e4, size = 100, prob = samples)

dens(first_boy_sim, adj = 0.1)
abline(v = sum(birth1), col = "red")

#This estimate is not as good as the previous one. Most estimates are to the right
#of the observed births of 51 boy first borns.

################################################################################

#3H5. The model assumes that sex of first and second births are independent. To
#check this assumption, focus now on second births that followed female first borns.
#Compare 10,000 simulated counts of boys to only those second births that followed
#girls. To do this correctly, you need to count the number of first borns who were
#girls and simulate that many births, 10,000 times. Compare the counts of boys in
#your simulations to the actual observed count of boys following girls. How does
#the model look in this light? Any guess what is going on in these data?

girl_then_boy <- birth2[birth1 == 0]

girl_then_boy_sim <- rbinom(1e4, size = length(girl_then_boy), prob = samples)

dens(girl_then_boy_sim, adj = 0.1)
abline(v = sum(girl_then_boy), col = "red")

#This is a terrible prediction and it suggests that the births may not be independent