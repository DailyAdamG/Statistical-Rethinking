#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Statistical-Rethinking/Chapter 5")

#Loading necessary library

library(rethinking)

#R code 5.1

#Load data and standardize the variables of interest

#load data and copy

data("WaffleDivorce")

d <- WaffleDivorce

#standardize variables

d$D <- standardize(d$Divorce)

d$M <- standardize(d$Marriage)

d$A <- standardize(d$MedianAgeMarriage)

################################################################################

#R code 5.2

#Standard deviation of age at marriage

sd(d$MedianAgeMarriage)

################################################################################

#R code 5.3

#Compute the approximate posterior

m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)), data = d)

################################################################################

#R code 5.4

#Simulate from the priors

set.seed(10)

prior <- extract.prior(m5.1)

mu <- link(m5.1, post = prior, data = list(A = c(-2, 2)))

plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))

for(i in 1:50) lines(c(-2, 2), mu[i,], col = col.alpha("black", 0.4))

################################################################################

#R code 5.5

#Calculate posterior predictions

#compute percentile interval of mean

A_seq <- seq(from = -3, to = 3.2, length.out = 30)

mu <- link(m5.1, data = list(A = A_seq))

mu.mean <- apply(mu, 2, mean)

mu.PI <- apply(mu, 2, PI)

#plot it all

plot(D ~ A, data = d, col = rangi2)

lines(A_seq, mu.mean, lwd = 2)

shade(mu.PI, A_seq)

################################################################################

#R code 5.6

#Fit a similar regression for marriage rate

m5.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)), data = d)

################################################################################

#R code 5.7

#Draw the DAG from the book

library(dagitty)

dag5.1 <- dagitty("dag{A -> D; A -> M; M -> D}")

coordinates(dag5.1) <- list(x = c(A = 0, D = 1, M = 2), y = c(A = 0, D = 1, M = 0))

drawdag(dag5.1)

################################################################################

#R code 5.8

#Create the second DAG from the book and display the implied conditional dependencies

DMA_dag2 <- dagitty("dag{D <- A -> M}")

impliedConditionalIndependencies(DMA_dag2)

################################################################################

#R code 5.9

#Check the first DAG's conditional independencies

DMA_dag1 <- dagitty("dag{D <- A -> M -> D}")

impliedConditionalIndependencies(DMA_dag1)

################################################################################

#R code 5.10

#Approximating the posterior distribution

m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    a <- dnorm(0, 0.2),
    bM <- dnorm(0, 0.5),
    bA <- dnorm(0, 0.5),
    sigma <- dexp(1)), data = d)

precis(m5.3)

################################################################################

#R code 5.11

#Visualize the posterior distributions for all 3 models, focusing only on slope
#parameters bM and bA

plot(coeftab(m5.1, m5.2, m5.3), par = c("bA", "bM"))

################################################################################

#R code 5.12

#Simulate the kind of causal relations shown in the previous DAG

N <- 50 #number of simulated States

age <- rnorm(N) # sim A

mar <- rnorm(N, -age) #sim A -> M

div <- rnorm(N, age) #sim A -> D

################################################################################

#R code 5.13

#Approximate the posterior for marriage rate

m5.4 <-
  quap(
    alist(
      M ~ dnorm(mu, sigma),
      mu <- a + bAM * A,
      a ~ dnorm(0, 0.2),
      bAM ~ dnorm(0, 0.5),
      sigma ~ dexp(1)), data = d)

################################################################################

#R code 5.14

#Compute the residuals by subtracting the observed marriage rate from the predicted
#rate

mu <- link(m5.4)

mu_mean <- apply(mu, 2, mean)

mu_resid <- d$M - mu_mean

################################################################################

#R code 5.15

#Simulating predictions averaging over the posterior

#call link without specifying new data so it uses the original data

mu <- link(m5.3)

#summarize samples across cases

mu_mean <- apply(mu, 2, mean)

mu_PI <- apply(mu, 2, PI)

#simulate observations
#again no new data, so uses original data

D_sim <- sim(m5.3, n = 1e4)

D_PI <- apply(D_sim, 2, PI)

################################################################################

#R code 5.16

#Plot the predictions against the observed data

plot(mu_mean ~ d$D, col = rangi2, ylim = range(mu_PI),
     xlab = "Observed divorce", ylab = "Predicted divorce")
abline(a = 0, b = 1, lty = 2)
for(i in 1:nrow(d)) lines(rep(d$D[i], 2), mu_PI[,i], col = rangi2)

################################################################################

#R code 5.17

#Use identify to label some points

identify(x = d$D, y = mu_mean, labels = d$Loc)

################################################################################

#R code 5.18

#Showing how a truly causal predictor influences both the outcome and a spurious
#predictor

N <- 100 #number of cases

x_real <- rnorm(N) #x_real as Gaussian with mean  and 0 and stddev 1

x_spur <- rnorm(N, x_real) #x_spur as Gaussian with mean = x_real

y <- rnorm(N, x_real) #y as Gaussian with mean = x_real

d <- data.frame(y, x_real, x_spur) #bind all together in data frame

################################################################################

#R code 5.19

#Posterior distribution of counterfactual outcomes for divorce model

data("WaffleDivorce")

d <- list()

d$A <- standardize(WaffleDivorce$MedianAgeMarriage)
d$D <- standardize(WaffleDivorce$Divorce)
d$M <- standardize(WaffleDivorce$Marriage)

m5.3_A <- quap(
  alist(
    ## A -> D <- M
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    a <- dnorm(0, 0.2),
    bM <- dnorm(0, 0.5),
    bA <- dnorm(0, 0.5),
    sigma <- dexp(1),
    ## A -> M
    M ~ dnorm(mu_M, sigma_M),
    mu_M <- aM + bAM * A,
    aM ~ dnorm(0, 0.2),
    bAM ~ dnorm(0, 0.5),
    sigma_M <- dexp(1)), data = d)

################################################################################

#R code 5.20

#Test how manipulating A affects M, create 30 intervals to test

A_seq <- seq(from = -2, to = 2, length.out = 30)

################################################################################

#R code 5.21

#Simulate observations from model m5.3_A

#prep data
sim_dat <- data.frame(A = A_seq)

#simulate M and then D, using A_seq
s <- sim(m5.3_A, data = sim_dat, vars=c("M", "D"))

################################################################################

#R code 5.22

#Plot the predictions

plot(sim_dat$A, colMeans(s$D), ylim = c(-2, 2), type = "l",
     xlab = "manipulated A", ylab = "counterfactual D")

shade(apply(s$D, 2, PI), sim_dat$A)

mtext("Total counterfactual effect of A on D")

################################################################################

#R code 5.23

#Expected causal effect of increasing median age to 30

#new data frame, standardized to mean 26.1 and std dev 1.24

sim2_dat <- data.frame(A = (c(20, 30) - 26.1) / 1.24)

s2 <- sim(m5.3_A, data = sim2_dat, vars = c("M", "D"))

mean(s2$D[, 2] - s2$D[, 1])

################################################################################

#R code 5.24

#See what changing M does when A = 0

sim_dat <- data.frame(M = seq(from = -2, to = 2, length.out = 30), A = 0)

s <- sim(m5.3_A, data = sim_dat, vars = "D")

plot(sim_dat$M, colMeans(s), ylim = c(-2, 2), type = "l",
     xlab = "manipulated M", ylab = "counterfactual D")

shade(apply(s, 2, PI), sim_dat$M)

mtext("Total counterfactual effect of M on D")

################################################################################

#R code 5.25

#Doing the sim function from hand, first choose a range of values

A_seq <- seq(from = -2, to = 2, length.out = 30)

################################################################################

#R code 5.26

#Extract posterior samples for simulation of M

post <- extract.samples(m5.3_A)

M_sim <- with(post, sapply(1:30,
                           function(i) rnorm(1e3, aM + bAM*A_seq[i], sigma_M)))

################################################################################

#R code 5.27

#Simulate the values for D as well

D_sim <- with(post, sapply(1:30,
                           function(i) rnorm(1e3, a + bA*A_seq[i] + bM * M_sim[, i], sigma)))

################################################################################

#R code 5.28

#Load data into R for milk problem

data(milk)

d <- milk

str(d)

################################################################################

#R code 5.29

#Standardize the data

d$K <- standardize(d$kcal.per.g)

d$N <- standardize(d$neocortex.perc)

d$M <- standardize(log(d$mass))

################################################################################

#R code 5.30

m5.5_draft <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0,1),
    bN ~ dnorm(0,1),
    sigma ~ dexp(1)), data = d)

################################################################################

#R code 5.31

#See why the error message popped up

d$neocortex.perc

################################################################################

#R code 5.32

#Make a new data frame with only complete cases

dcc <- d[complete.cases(d$K, d$N, d$M), ]

################################################################################

#R code 5.33

#Redo model code but with complete cases instead

m5.5_draft <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0,1),
    bN ~ dnorm(0,1),
    sigma ~ dexp(1)), data = dcc)

################################################################################

#R code 5.34

#Simulate and plot 50 prior regression lines

prior <- extract.prior(m5.5_draft)

xseq <- c(-2, 2)

mu <- link(m5.5_draft, post = prior, data = list(N = xseq))

plot(NULL, xlim = xseq, ylim = xseq)

for(i in 1:50) lines(xseq, mu[i,], col = col.alpha("black", 0.3))

################################################################################

#R code 5.35

#Previous example had bad priors. Updating the priors to create a better model

m5.5 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)), data = dcc)

################################################################################

#R code 5.36

#View the posterior summary

precis(m5.5)

################################################################################

#R code 5.37

#Plot predicted mean and 89% compatibility interval for the mean

xseq <- seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)

mu <- link(m5.5, data = list(N = xseq))

mu_mean <- apply(mu, 2, mean)

mu_PI <- apply(mu, 2, PI)

plot(K ~ N, data = dcc)

lines(xseq, mu_mean, lwd = 2)

shade(mu_PI, xseq)

################################################################################

#R code 5.38

#Construct a similar model but looking at body mass instead of neocortext

m5.6 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)), data = dcc)

precis(m5.6)

################################################################################

#R code 5.39

#Add both predictor variables to the model

m5.7 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + +bN * N + bM * M,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)), data = dcc)

precis(m5.7)

################################################################################

#R code 5.40

#Comparing the posterior to the previous 2 models

plot(coeftab(m5.5, m5.6, m5.7), pars = c("bM", "bN"))

################################################################################

#R code 5.41

#Create counterfactual plot when making N = 0

xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)

mu <- link(m5.7, data = data.frame(M = xseq, N = 0))

mu_mean <- apply(mu, 2, mean)

mu_PI <- apply(mu, 2, PI)

plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K))

lines(xseq, mu_mean, lwd = 2)

shade(mu_PI, xseq)

################################################################################

#R code 5.42

#Simulate data consistent with first DAG shown

#M -> K <- N

#M -> N

n <- 100

M <- rnorm(n)

N <- rnorm(n, M)

K <- rnorm(n, N - M)

d_sim <- data.frame(K = K, N = N, M = M)

################################################################################

#R code 5.43

