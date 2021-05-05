#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Statistical-Rethinking/Chapter 4")

#Loading necessary libraries

library(rethinking)

library(splines)

#R code 4.1

#Simulate coin flip and person positioning on soccer field problem

pos <- replicate(1000, sum(runif(16, -1, 1)))

################################################################################

#R code 4.2

#Using multiplicative power instead of additive for normal distribution example

prod(1 + runif(12, 0, 0.1))

################################################################################

#R code 4.3

#Checking distribution from previous example

growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))

dens(growth, norm.comp = TRUE)

################################################################################

#R code 4.4

#Showing how small muliplicative problems approximate additive problem distribution

big <- replicate(10000, prod(1 + runif(12, 0, 0.5)))

small <- replicate(10000, prod(1 + runif(12, 0, 0.1)))

dens(big, norm.comp = TRUE)

dens(small, norm.comp = TRUE)

################################################################################

#R code 4.5

#Showing how multiplied values may not be Gaussian, but they are on the log scale

log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))

dens(log.big, norm.comp = TRUE)

################################################################################

#R code 4.6

#Translating from model definition to Bayes theorem

w <- 6

n <- 9

p_grid <- seq(from = 0, to = 1, length.out = 100)

posterior <- dbinom(w, n, p_grid) * dunif(p_grid, 0, 1)

posterior <- posterior / sum(posterior)

################################################################################

#R code 4.7

#Loading data for future problems

library(rethinking)

data(Howell1)

d <- Howell1

################################################################################

#R code 4.8

#Check the structure of the data frame

str(d)

################################################################################

#R code 4.9

#Used to summarize each column in the data frame

precis(d, hist = FALSE)

################################################################################

#R code 4.10

#Access the height column

d$height

################################################################################

#R code 4.11

#Filter the data to only include adult data

d2 <- d[d$age >= 18, ]

################################################################################

#R code 4.12

#Plot the prior for mean height example

curve(dnorm(x, 178, 20), from =100, to = 250)

################################################################################

#R code 4.13

#Plot the prior for sigma height

curve(dunif(x, 0, 50), from = -10, to = 60)

################################################################################

#R code 4.14

#Simulating heights by sampling from the prior

sample_mu <- rnorm(1e4, 178, 20)

sample_sigma <- runif(1e4, 0, 50)

prior_h <- rnorm(1e4, sample_mu, sample_sigma)

dens(prior_h)

################################################################################

#R code 4.15

#See how changing the prior sigma changes the distribution

sample_mu <- rnorm(1e4, 178, 100)

prior_h <- rnorm(1e4, sample_mu, sample_sigma)

dens(prior_h)

################################################################################

#R code 4.16

#Mapping the posterior distribution through brute force

mu.list <- seq(from = 150, to = 160, length.out = 100)

sigma.list <- seq(from = 7, to = 9, length.out = 100)

post <- expand.grid(mu = mu.list, sigma = sigma.list)

post$LL <- sapply(1:nrow(post), function(i) sum(
  dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)))

post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) +
  dunif(post$sigma, 0, 50, TRUE)

post$prob <- exp(post$prod - max(post$prod))

################################################################################

#R code 4.17

#View the posterior distribution

contour_xyz(post$mu, post$sigma, post$prob)

################################################################################

#R code 4.18

#View the heat map

image_xyz(post$mu, post$sigma, post$prob)

################################################################################

#R code 4.19

#Study the posterior by sampling parameter values from it

sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)

sample.mu <- post$mu[sample.rows]

sample.sigma <- post$sigma[sample.rows]

################################################################################

#R code 4.20

#Plot the samples

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))

################################################################################

#R code 4.21

#Characterize the shapes of the marginal posterior densities of mu and sigma

dens(sample.mu)

dens(sample.sigma)

################################################################################

#R code 4.22

#Summarize the width of these densities with compatibility intervals

PI(sample.mu)

PI(sample.sigma)

################################################################################

#R code 4.23

#Showing how sigma can have a long right hand tail by sampling 20 random heights

d3 <- sample(d2$height, size = 20)

################################################################################

#R code 4.24

#Mapping the new distribution through brute force

mu.list <- seq(from = 150, to = 170, length.out = 200)

sigma.list <- seq(from = 4, to = 20, length.out = 200)

post2 <- expand.grid(mu = mu.list, sigma = sigma.list)

post2$LL <- sapply(1:nrow(post2), function(i) sum(
  dnorm(d3, mean = post2$mu[i], sd = post2$sigma[i], log = TRUE)))

post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) +
  dunif(post2$sigma, 0, 50, TRUE)

post2$prob <- exp(post2$prod - max(post2$prod))

sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = TRUE, prob = post2$prob)

sample2.mu <- post2$mu[sample2.rows]

sample2.sigma <- post2$sigma[sample2.rows]

plot(sample2.mu, sample2.sigma, cex = 0.5, col = col.alpha(rangi2, 0.1), 
     xlab = "mu", ylab = "sigma", pch = 16)

################################################################################

#R code 4.25

#View the marginal posterior density for sigma

dens(sample2.sigma, norm.comp = TRUE)

################################################################################

#R code 4.26

#Loading the data and selecting only adults

data("Howell1")

d <- Howell1

d2 <- d[d$age >= 18,]

################################################################################

#R code 4.27

#Placing the model formulas into alist

flist <- alist(height ~ dnorm(mu, sigma),
               mu ~ dnorm(178, 20),
               sigma ~ dunif(0, 50))

################################################################################

#R code 4.28

#Fit the model to the data in the data frame d2

m4.1 <- quap(flist, data = d2)

################################################################################

#R code 4.29

#Examine the posterior distribution

precis(m4.1)

################################################################################

#R code 4.30

#Tell quap function where to start climbing the hill to find the MAP

start <- list(
  mu = mean(d2$height),
  sigma = sd(d2$height)
)

m4.1 <- quap(flist, data = d2, start = start)

################################################################################

#R code 4.31

#Using a more informative prior than previously

m4.2 <- quap(
  alist(height ~ dnorm(mu, sigma),
             mu ~ dnorm(178, 0.1),
             sigma ~ dunif(0, 50)), data = d2)

precis(m4.2)

################################################################################

#R code 4.32

#Observing a matrix of variances and covariances for model m4.1

vcov(m4.1)

################################################################################

#R code 4.33

#Decomposing a variance-covariance matrix into its two elements

diag(vcov(m4.1))

cov2cor(vcov(m4.1))

################################################################################

#R code 4.34

#Extract samples from multi-dimensional posterior

post <- extract.samples(m4.1, n = 1e4)

head(post)

################################################################################

#R code 4.35

#Check the summary

precis(post, hist = FALSE)

################################################################################

#R code 4.36

#Showing what is going on when using extract.samples function

library(MASS)

post <- mvrnorm(n = 1e4, mu = coef(m4.1), Sigma = vcov(m4.1))

################################################################################

#R code 4.37

#Plot adult height and weight against each other

data("Howell1")

d <- Howell1

d2 <- d[d$age >= 18,]

plot(d2$height ~ d2$weight)

################################################################################

#R code 4.38

#Simulating heights from the model using only priors

set.seed(2971)

N <- 100 # 100 lines

a <- rnorm(N, 178, 20)

b <- rnorm(N, 0, 10)

################################################################################

#R code 4.39

#Plotting the lines just created

plot(NULL, xlim = range(d2$weight), ylim = c(-100, 400),
     xlab = "weight", ylab = "height")

abline(h = 0, lty = 2)

abline(h = 272, lty = 1, lwd = 0.5)

mtext("b ~ dnorm(0, 10)")

xbar <- mean(d2$weight)

for(i in 1:N) curve(a[i] + b[i] * (x - xbar),
                    from = min(d2$weight), to = max(d2$weight), add = TRUE,
                    col = col.alpha("black", 0.2))

################################################################################

#R code 4.40

#Restrict the equation to only positive values using log-normal function

b <- rlnorm(1e4, 0, 1)

dens(b, xlim = c(0, 5), adj = 0.1)

################################################################################

#R code 4.41

#Run prior predictive simulation again, but with log-normal prior instead

set.seed(2971)

N <- 100 # 100 lines

a <- rnorm(N, 178, 20)

b <- rlnorm(N, 0, 1)

################################################################################

#R code 4.42

#Build the posterior approximation using a linear model

data("Howell1")

d <- Howell1

d2 <- d[d$age >= 18, ]

#define the average weight, x-bar

xbar <- mean(d2$weight)

#fit model

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d2
)

################################################################################

#R code 4.43

#Create the same model but using log of b

m4.3b <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + exp(log_b) * (weight - xbar),
    a ~ dnorm(178, 20),
    log_b ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d2
)

################################################################################

#R code 4.44

#Inspect the marginal posterior distributions of the parameters

precis(m4.3)

################################################################################

#R code 4.45

#Inspect the variance-covariance matrix

round(vcov(m4.3), 3)

################################################################################

#R code 4.46

#Plot the raw data and compute the posterior mean values for a and b and draw line

plot(height ~ weight, data = d2, col = rangi2)

post <- extract.samples(m4.3)

a_map <- mean(post$a)

b_map <- mean(post$b)

curve(a_map + b_map * (x - xbar), add = TRUE)

################################################################################

#R code 4.47

#Examining the samples used

post <- extract.samples(m4.3)

post[1:5,]

################################################################################

#R code 4.48

#Re-estimate the model using only the first 10 rows

N <- 10

dN <- d2[1:N,]

mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
    ), 
  data = dN
)

################################################################################

#R code 4.49

#Plot 20 of these lines to see what the uncertainty looks like

#extract 20 samples from the posterior

post <- extract.samples(mN, n = 20)

#display raw data and sample size

plot(dN$weight, dN$height,
     xlim = range(d2$weight), ylim = range(d2$height),
     col = rangi2, xlab = "weight", ylab = "height")
mtext(concat("N = ", N))

#plot the lines, with transparency

for(i in 1:20)
  curve(post$a[i] + post$b[i] * (x - mean(dN$weight)),
        col = col.alpha("black", 0.3), add = TRUE)

################################################################################

#R code 4.50

#Taking samples of person that weighs 50 kg

post <- extract.samples(m4.3)

mu_at_50 <- post$a + post$b * (50 - xbar)

################################################################################

#R code 4.51

#Plot the density at mu = 50

dens(mu_at_50, col=rangi2, lwd = 2, xlab = "mu|weight = 50")

################################################################################

#R code 4.52

#Find the compatibility interval of mu at 50 kg

PI(mu_at_50, prob = 0.89)

################################################################################

#R code 4.53

#Use the link function to compute mu for each case in the data and sample from the 
#posterior distribution

mu <- link(m4.3)

str(mu)

################################################################################

# R code 4.54

#Find the distribution of mu for each unique weight value on the x-axis

#define sequence of weights to compute predictions for these values will be on the
#horizontal axis

weight.seq <- seq(from = 25, to = 70, by = 1)

#use link to compute mu for each sample from posterior and for each weight in weight.seq

mu <- link(m4.3, data = data.frame(weight = weight.seq))

str(mu)

################################################################################

#R code 4.55

#Visualize the new reduced data set by plotting the distribution of mu values at
#each height

#use type = "n" to hide raw data

plot(height ~ weight, d2, type = "n")

#loop over samples and plot each mu value

for(i in 1:100)
  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))

################################################################################

#R code 4.56

#Summarize the distribution for each weight value

#summarize the distribution of mu

mu.mean <- apply(mu, 2, mean)

mu.PI <- apply(mu, 2, PI, prob = 0.89)

################################################################################

#R code 4.57

#Plot the summaries on top of the data

#plot raw data fading out points to make line and interval more visible

plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))

#plot the MAP line, aka the mean mu for each weight

lines(weight.seq, mu.mean)

#plot a shaded region for 89% PI

shade(mu.PI, weight.seq)

################################################################################

#R code 4.58

#Showing how to perform same function as the link function, but manually

post <- extract.samples(m4.3)

mu.link <- function(weight) post$a + post$b * (weight - xbar)

weight.seq <- seq(from = 25, to = 70, by = 1)

mu <- sapply(weight.seq, mu.link)

mu.mean <- apply(mu, 2, mean)

mu.CI <- apply(mu, 2, PI, prob = 0.89)

################################################################################

#R code 4.59

#Use sim function to create a collection of simulated heights that embody the
#uncertainty in the posterior as well as the uncertainty in the Gaussian distribution
#of heights.

sim.height <- sim(m4.3, data = list(weight = weight.seq))

str(sim.height)

################################################################################

#R code 4.60

#Summarize the simulated heights

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

################################################################################

#R code 4.61

#Plot everything built

#plot raw data

plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))

#draw MAP line

lines(weight.seq, mu.mean)

#draw CI region for line

shade(mu.CI, weight.seq)

#draw PI region for simulated heights

shade(height.PI, weight.seq)

################################################################################

#R code 4.62

#Increase the number of samples for simulation to create more rigid outline of PI

sim.height <- sim(m4.3, data = list(weight = weight.seq), n = 1e4)

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

################################################################################

#R code 4.63

#Showing how to perform same function as the sim function, but manually

post <- extract.samples(m4.3)

weight.seq <- 25:70

sim.height <- sapply(weight.seq, function(weight)
  rnorm(
    n = nrow(post),
    mean = post$a + post$b * (weight - xbar),
    sd = post$sigma))

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

################################################################################

#R code 4.64

#Load all the data, not just adults

data(Howell1)

d <- Howell1

################################################################################

#R code 4.65

#Approximate the posterior using polynomial

d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight)

d$weight_s2 <- d$weight_s ^ 2

m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d
)

################################################################################

#R code 4.66

#Look at summary using precis

precis(m4.5)

################################################################################

#R code 4.67

#Plot the model to understand the model fits, create model first

weight.seq <- seq(from = -2.2, to = 2, length.out = 30)

pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq ^ 2)

mu <- link(m4.5, data = pred_dat)

mu.mean <- apply(mu, 2, mean)

mu.PI <- apply(mu, 2, PI, prob = 0.89)

sim.height <- sim(m4.5, data = pred_dat)

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

################################################################################

#R code 4.68

#Plot the model

plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5))

lines(weight.seq, mu.mean)

shade(mu.PI, weight.seq)

shade(height.PI, weight.seq)

################################################################################

#R code 4.69

#Create a new model with more polynomials

d$weight_s3 <- d$weight_s ^ 3

m4.6 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2 + b3 * weight_s3,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0,10),
    sigma ~ dunif(0, 50)
  ), data = d
)

################################################################################

#R code 4.70

#Plotting the values on the original scale instead of the z-score

plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5), xaxt = "n")

################################################################################

#R code 4.71

#Construct the x-axis using the axis function

at <- c(-2, -1, 0, 1, 2)

labels <- at * sd(d$weight) + mean(d$weight)

axis(side = 1, at = at, labels = round(labels, 1))

################################################################################

#R code 4.72

#Loading cherry blossom data

data("cherry_blossoms")

d <- cherry_blossoms

precis(d, hist = FALSE)

################################################################################

#R code 4.73

#Choosing knots for spline

d2 <- d[complete.cases(d$doy), ] #complete cases on doy

num_knots <- 15

knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))

################################################################################

#R code 4.74

#Code to construct the necessary basis for a cubic spline

B <- bs(d2$year,
        knots = knot_list[-c(1, num_knots)],
        degree = 3, intercept = TRUE)

################################################################################

#R code 4.75

#Display the basis functions

plot(NULL, xlim = range(d2$year), ylim = c(0, 1), xlab = "year", ylab = "basis")

for (i in 1:ncol(B)) lines(d2$year, B[,i])

################################################################################

#R code 4.76

#Build the model in quap

m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data = list(D = d2$doy, B = B),
  start = list(w = rep(0, ncol(B))))

################################################################################

#R code 4.77

#Plot the posterior predictions

post <- extract.samples(m4.7)

w <- apply(post$w, 2, mean)

plot(NULL, xlim = range(d2$year), ylim = c(-6, 6),
     xlab = "year", ylab = "basis * weight")

for(i in 1:ncol(B)) lines(d2$year, w[i] * B[,i])

################################################################################

#R code 4.78

#Plotting the 97% PI for mu at each year

mu <- link(m4.7)

mu_PI <- apply(mu, 2, PI, 0.97)

plot(d2$year, d2$doy, col = col.alpha(rangi2, 0.3), pch = 16)

shade(mu_PI, d2$year, col = col.alpha("black",  0.5))

################################################################################

#R code 4.79

#Creating the same model with less elegant matrix algebra code

m4.7alt <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + sapply(1:827, function(i) sum(B[i,] * w)),
    a ~ dnorm(100, 1),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = list(D = d2$doy, B=B),
  start = list(w = rep(0, ncol(B)))
)