#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Statistical-Rethinking/Chapter 4")

#Load necessary library

library(rethinking)

#4E1. In the model definition below, which line is the likelihood?

#y_i ~ Normal(mu, sigma)
#mu ~ Normal(0, 10)
#sigma ~ Exponential(1)

#yi is the likelihood function

################################################################################

#4E2. In the model definition just above, how many parameters are in the posterior
#distribution?

#There are 2 parameters in the posterior distribution. The mu and the sigma.

################################################################################

#4E3. Using the model definition above, write down the appropriate form of Bayes'
#theorem that includes the proper likelihood and priors.

#Pr(mu, sigma | y) = Pr(y | mu, sigma) * Pr(mu) * Pr(sigma) / 
#(INT INT Pr(y | mu, sigma) * Pr(mu) * Pr(sigma) d-mu d-sigma)

################################################################################

#4E4. In the model definition below, which line is the linear model?

#y_i ~ Normal(mu, sigma)
#mu_i = alpha + beta * x_i
#alpha ~ Normal(0, 10)
#beta ~ Normal(0, 1)
#sigma ~ Exponential(2)

#mu_i = alpha + beta * x_i is the linear model.

################################################################################

#4E5. In the model definition just above, how many parameters are in the posterior
#distribution?

#There are 3 parameters in the posterior distribution. The alpha, the beta, and the sigma.

################################################################################

#4M1. For the model definition below, simulate observed y values from the prior
#(not the posterior).

mu_prior <- rnorm(1e4, 0, 10)

sigma_prior <- rexp(1e4, 1)

h_sim <- rnorm(1e4, mu_prior, sigma_prior)

dens(h_sim)

################################################################################

#4M2. Translate the model just above into a quap formula.

f <-  alist(
    y ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 10),
    sigma ~ dexp(1))

################################################################################

#4M3. Translate the quap model formula below into a mathematical model definition.

#y ~ dnorm(mu, sigma),
#mu <- a + b * x,
#a ~ dnorm(0, 10),
#b ~ dunif(0, 1),
#sigma ~ dexp(1)

#y_i ~ Normal(mu, sigma)

#mu = a + b * x_i

#a ~ Normal(0, 10)

#b ~ Uniform(0, 1)

#sigma ~ Exponential(1)

################################################################################

#4M4. A sample of students is measured for height each year for 3 years. After the
#third year, you want to fit a linear regression predicting height using year as
# a predictor. Write down the mathematical model definition for this regression,
#using any variable names and priors you choose. Be prepared to defend your choice
#of priors.

#height_i ~ Normal(mu, sigma)

#mu = a + b * year_i

#a ~ Normal(0, 100)

#b ~ Uniform(0, 10)

#sigma ~ Uniform(0, 50)

################################################################################

#4M5. Now suppose I remind you that every student got taller each year. Does this
#information lead you to change your choice of priors? How?

#This is tricky, because priors are supposed to be agnostic of the data being modeled.
#I suppose I could change the b distribution from earlier to go from 1 to 10 instead 
#of 0 to 10, since I know the kids grow every year. However, that could be interpreted
#as creating a prior after examining the data.

################################################################################

#4M6. Now suppose I tell you that the variance among heights for students of the
#same age is never more than 64cm. How does this lead you to revise your priors?

#This is the same issue as the previous problem. Assuming I can change my priors
#without compromising the model, I'd change the sigma distribution to Uniform(0, 64)
#instead.

################################################################################

#4H1. The weights listed below were recorded in the !Kung census, but heights were
#not recorded for these individuals. Provide predicted heights and 89% intervals
#for each of these individuals. That is, fill in the table below, using model-based
#predictions.

#Weights: 46.95, 43.72, 64.78, 32.59, 54.63

#Grab the data

data("Howell1")

d <- Howell1

d2 <- d[d$age >= 18, ]

#Create a model for the data

m <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm(178, 20),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)), data = d2
)

#Extracting samples from model

post <- extract.samples(m)

str(post)

#Plugging weights from the model samples into the formula

y1 <- rnorm(1e4, post$a + post$b * 46.95, post$sigma)
y2 <- rnorm(1e4, post$a + post$b * 43.72, post$sigma)
y3 <- rnorm(1e4, post$a + post$b * 64.78, post$sigma)
y4 <- rnorm(1e4, post$a + post$b * 32.59, post$sigma)
y5 <- rnorm(1e4, post$a + post$b * 54.63, post$sigma)

#Averaging over the posterior to compute predictions for each height

mean(y1)
mean(y2)
mean(y3)
mean(y4)
mean(y5)

#Finding the 89% PI for each data point

PI(y1, prob = 0.89)
PI(y2, prob = 0.89)
PI(y3, prob = 0.89)
PI(y4, prob = 0.89)
PI(y5, prob = 0.89)

################################################################################

#4H2. Select out all the rows in the Howell data with ages below 18 years of age.
#If you do it right, you should end up with a new data frame with 192 rows in it.

#Grab the data

data("Howell1")

d <- Howell1

d3 <- d[d$age < 18, ]

#a) Fit a linear regression to these data, using quap. Present and interpret the
#estimates. For every 10 units of increase in weight, how much taller does the
#model predict a child gets?

m <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm(178, 20),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)), data = d3
)

precis(m, hist = FALSE)

#The model suggests that every 1 kg increases the height by 2.68 cm. This means
#that for an increase in 10 kg we expect the child to grow 26.8 cm.

#b) Plot the raw data, with height on the vertical axis and weight on the horizontal
#axis. Superimpose the MAP regression line and 89% interval for the mean. Also
#superimpose the 89% interval for predicted heights.

weight.seq <- seq(from = 1, to = 45, length.out = 50)

mu <- link(m, data = data.frame(weight = weight.seq))

sim.height <- sim(m, data = list(weight = weight.seq))

mu.mean <- apply(mu, 2, mean)

mu.PI <- apply(mu, 2, PI, prob = 0.89)

sim.height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight, data = d3, col = col.alpha(rangi2, 0.5))

lines(weight.seq, mu.mean)

shade(sim.height.PI, weight.seq)

shade(mu.PI, weight.seq)

#c) What aspects of the model fit concern you? Describe the kinds of assumptions
#you would change, if any, to improve the model. You don't have to write any new
#code. Just explain what the model appears to be doing a bad job of, and what you
#hypothesize would be a better model.

#The big problem is that the data isn't linear. The model over-estimates a child's
#height based on his weight at the extremes of the model. A polynomial would likely
#help the model's accuracy.

################################################################################

#4H3. Suppose a colleague of yours, who works on allometry, glances at the practice
#problems just above. Your colleague exclaims, "That's silly. Everyone knows that
#it's only the logarithm of body weight that scales with height!" Let's take your
#colleague's advice and see what happens.

#a) Model the relationship between height (cm) and the natural logarithm of weight
#(log-kg). Use the entire Howell1 data frame, all 544 rows, adults and non-adults.
#Can you interpret the resulting estimates?

m2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * log(weight),
    a ~ dnorm(138, 50),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)), data = d
)

precis(m2, hist = FALSE)

#The results mean that for every 1 unit increase in log-kg, the height prediction
#increases by 46.98 cm.

#b) Begin with this plot: plot(height ~ weight, data = Howell1). Then use samples
#from the quadratic approximate posterior of the model in (a) to superimpose on
#the plot: (1) the predicted mean height as a function of weight, (2) the 97%
#interval for the mean, and (3) the 97% interval for the predicted heights.

post <- extract.samples(m2)

log_weight.seq <- seq(from = 1, to = 65, length.out = 200)

mu <- link(m2, data = data.frame(weight = log_weight.seq))

mu.mean <- apply(mu, 2, mean)

mu.PI <- apply(mu, 2, PI, prob = 0.97)

sim.height <- sim(m2, data = list(weight = log_weight.seq))

sim.height.PI <- apply(sim.height, 2, PI, prob = 0.97)

plot(height ~ weight, data = d, col = col.alpha(rangi2, 0.5))

lines(log_weight.seq, mu.mean)

shade(mu.PI, log_weight.seq)

shade(sim.height.PI, log_weight.seq)