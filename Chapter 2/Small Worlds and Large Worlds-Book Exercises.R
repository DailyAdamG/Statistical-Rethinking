#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Statistical-Rethinking/Chapter 2")

#2E1. Which of the expressions below correspond to the statement: the probability
#of rain on Monday?

#2 and 4 are correct. Pr(rain|Monday) and Pr(rain,Monday) / Pr(Monday)

################################################################################

#2E2. Which of the following statements correspond to the expression: Pr(Monday|rain)?

#3 is correct. The probability that it is Monday, given that it is raining.

################################################################################

#2E3. Which of the expressions below correspond to the statement: the probability
#that it is Monday, given that it is raining?

#1 and 4 are correct. Pr(Monday|rain) and Pr(rain|Monday) Pr(Monday) / Pr(rain)

################################################################################

#2M1. Recall the globe tossing model from the chapter. Compute and plot the grid
#approximate posterior distribution for each of the following sets of observations.
#In each case, assume a uniform prior for p.

# (1) W,W,W
# (2) W,W,W,L
# (3) L,W,W,L,W,W,W

#Part One

#define grid

p_grid <- seq(from = 0, to = 1, length.out = 20)

#define prior

prior <- rep(1, 20)

#compute likelihood of each value in grid for 3 water in 3 tosses

likelihood <- dbinom(3, size = 3, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the posterior distribution with a grid of 20 points

plot(posterior ~ p_grid, type = "l")

#Part Two

#compute likelihood of each value in grid for 3 waters in 4 tosses

likelihood <- dbinom(3, size = 4, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the posterior distribution with a grid of 20 points

plot(posterior ~ p_grid, type = "l")

#Part Three

#compute likelihood of each value in grid for 5 waters in 7 tosses

likelihood <- dbinom(5, size = 7, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the posterior distribution with a grid of 20 points

plot(posterior ~ p_grid, type = "l")

################################################################################

#2M2. Now assume a prior for p that is equal to zero when p < 0.5 and is a positive
#constant when p >= 0.5. Again compute and plot the grid approximate posterior distribution
#for each of the sets of observations in the problem just above.

#Part One

#define grid

p_grid <- seq(from = 0, to = 1, length.out = 20)

#define prior

prior <- ifelse(p_grid < .5, 0, 1)

#compute likelihood of each value in grid for 3 water in 3 tosses

likelihood <- dbinom(3, size = 3, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the posterior distribution with a grid of 20 points

plot(posterior ~ p_grid, type = "l")

#Part Two

#compute likelihood of each value in grid for 3 waters in 4 tosses

likelihood <- dbinom(3, size = 4, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the posterior distribution with a grid of 20 points

plot(posterior ~ p_grid, type = "l")

#Part Three

#compute likelihood of each value in grid for 5 waters in 7 tosses

likelihood <- dbinom(5, size = 7, prob = p_grid)

#compute product of likelihood and prior

unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

#Plotting the posterior distribution with a grid of 20 points

plot(posterior ~ p_grid, type = "l")

################################################################################

#2M3. Suppose there are two globes, one for Earth and one for Mars. The Earth globe
#is 70% covered in water. The Mars globe is 100% land. Further suppose that one of
#these globes-you don't know which-was tossed in the air and produced a "land" observation.
#Assume that each globe was equally likely to be tossed. Show that the posterior
#probability that the globe was Earth, conditional on seeing "land" (Pr(Earth|land)),
#is 0.23.

#Pr(Land|Earth) = .3, Pr(Land|Mars) = 1, Pr(Earth) and Pr(Mars) = .5

#Applying Bayes theorem gives us this calculation

round(.3 * .5 / (.5 * .3 + .5 * 1),2)

################################################################################

#2M4. Suppose you have a deck with only three cards. Each card has two sides, and
#each side is either black or white. One card has two black sides.The second card
#has one black and one white side. The third card has two white sides. Now suppose
#all three cards are placed in a bag and shuffled. Someone reaches into the bag and
#pulls out a card and places it flat on a table. A black side is shown facing up,
#but you don't know the color of the side facing down. Show that the probability
#that the other side is also black is 2/3. Use the counting method (Section 2 of the chapter)
#to approach this problem. This means counting up the ways that each card could produce
#the observed data (a black side facing up on the table).

#The first step is to count all the ways to produce a black side facing up. There
#are 3 ways. The double-sided black card has 2 ways to do it, the black and white
#card has 1 way and the double-sided white card has 0. So there are 3 total possibilities
#and 2 of them come from the double-sided black card, so there is a 2/3 probability
#that the other side of the card is black.

################################################################################

#2M5. Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose
#a card is drawn from the bag and a black side appears face up. Again calculate the
#probability that the other side is black.

#With the extra double-sided black card there are now 5 ways to produce a black side
#facing up and 4 of them can occur from the 2 double-sided black cards. That means
#there is a 4/5 chance that the side facing down is black.

################################################################################

#2M6. Imagine that black ink is heavy, and so cards with black sides are heavier
#than cards with white sides. As a result, it's less likely that a card with black
#sides is pulled from the bag. So again assume there are three cards: B/B, B/W,
#and W/W. After experimenting a number of times, you conclude that for every way
#to pull the B/B from the bag, there are 2 ways to pull the B/W card and 3 ways to
#pull the W/W card. Again suppose that a card is pulled and a black side appears
#face up. Show that the probability the other side is black is now 0.5. Use the
#counting method as before.

#There are still 3 ways to produce a black card, but now there is an unequal chance
#of choosing the 3 different cards. So we need to multiply the ways by the chances
#of each card being pulled and divide by the total. The B/W card is twice as likely
#to be picked as the B/B card. So the B/B ways stay at 2, but now the B/W card is
#at 2 as well. The W/W card is still impossible to obtain from our data, so it doesn't
#affect our odds. Out of the 4 possibilities, 2 of them come from the B/B card, so
#the probability is 0.5.

################################################################################

#2M7. Assume again the original card problem, with a single card showing black side
#face up. Before looking at the other side, we draw another card from the bag and
#lay it face up on the table.The face that is shown on the new card is white. Show
#that the probability that the first card, the one showing a black side, has black
#on its other side is now 0.75. Use the counting method, if you can. Hint: Treat
#this like the sequence of globe tosses, counting all the ways to see each observation,
#for each possible first card.

#There are 2 ways to pull a B/B card in the first trial and 3 ways to pull a white
#card in the second trial if the B/B card is first. That gives 6 possible ways to
#create the sequence in the problem. There is one way to pull a B/W card in the first
#trial and 2 ways to pull a white card in the second trial. That gives 2 possible
#ways to create the sequence. It is impossible to pull a W/W card to begin with,
#so there are no possible combinations from that branch of the tree. That leaves
#us with 8 total ways and 6 of them come from the B/B card. 6/8 = .75.

################################################################################

#2H1. Suppose there are two species of panda bear. Both are equally common in the
#wild and live in the same places. They look exactly alike and eat the same food,
#and there is yet no genetic assay capable of telling them apart. They differ however
#in their family sizes. Species A gives birth to twins 10% of the time, otherwise
#birthing a single infant. Species B births twins 20% of the time, otherwise birthing
#singleton infants. Assume these numbers are known with certainty, from many years
#of field research. Now suppose you are managing a captive panda breeding program.
#You have a new female panda of unknown species, and she has just given birth to
#twins. What is the probability that her next birth will also be twins?

#This question is trying to solve Pr(twins2|twins1) which is equivalent to
#Pr(twins1, twins2) / Pr(twins). Pr(twins) = 0.5 * 0.2 + 0.5 * 0.1 and that is equal
#to 0.15. Next, I need to calculate the probability of Species A having back to back
#twins and calculate the probability of Species B having back to back twins and 
#then multiply each one by 0.5, since there is an equal likelihood of each occurring 
#and then add them together to get the numerator. 0.1 * 0.1 * 0.5 = 0.005 and
#0.2 * 0.2 * 0.5 = 0.02. Adding them together gives 0.025 and 0.025 / .15 = .167.

################################################################################

#2H2. Recall all the facts from the problem above. Now compute the probability that
#the panda we have is from species A, assuming we have observed only the first birth
#and that it was twins.

#This question is asking for Pr(A|twins) and using Bayes Theorem gives the following
#equation: Pr(twins|A) * Pr(A) / Pr(twins). We know that Pr(twins) = 0.15 from the
#previous problem. We know that Pr(A) is 0.5, because we have an equal chance of
#picking species A or species B and Pr(twins|A) is given as 0.1. Plugging these
#values into the equation gives us 0.1 * 0.5 / 0.15 and that equals 0.33.

################################################################################

#2H3. Continuing on from the previous problem, suppose the same panda mother has
#a second birth and that it is not twins, but a singleton infant. Compute the posterior
#probability that this panda is species A.

#This problem is asking for Pr(A|singleton) given that the first birth were twins.
#Using Bayes Theorem gives the following equation: Pr(singleton|A) * Pr(A) / Pr(singleton).
#Using the previous problem, we know that the updated Pr(A) is 0.33. We also know
#that Pr(singleton|A) is 0.9, because it is given to us. Pr(singleton) can be found
#by summing the probability of birthing a singleton given species A and the probability
#of birthing a singleton given species B. We know there is a 0.33 chance of being
#species A and a 0.67 chance of being species B. We also know that species A produces
#a singleton birth 90% of the time and that species B produces a singleton birth
#80% of the time. Plugging all of these values into the equation gives the following:
#0.9 * 0.33 / ((0.33 * 0.9) + (0.67 * 0.8)) and that gives us 0.36 probability that
#the panda is species A.

################################################################################

#2H4. A common boast of Bayesian statisticians is that Bayesian inference makes it
#easy to use all of the data, even if the data are of different types. So suppose
#now that a veterinarian comes along who has a new genetic test that she claims can
#identify the species of our mother panda. But the test, like all tests, is imperfect.
#This is the information you have about the test: The probability it correctly identifies
#a species A panda is 0.8. The probability it correctly identifies a species B panda
#is 0.65. The vet administers the test to your panda and tells you that the test
#is positive for species A. First ignore your previous information from the births
#and compute the posterior probability that your panda is species A. Then redo your
#calculation, now using the birth data as well.

#This problem is looking for Pr(A|testA), using Bayes Theorem, gives us the following
#equation: Pr(testA|A) * Pr(A) / Pr(testA). We know there is a 50% chance of the
#panda being species A if we ignore the birth data, so Pr(A) = 0.5. We are given
#that the test correctly identifies species A 80% of the time, so Pr(testA|A) is
#0.8. That just leaves Pr(testA). We know that choosing species B or species A is
#equal and we know that species A is correct 80% of the time and that species B
#is correct 65% of the time. This means that species B is incorrect 35% of the time
#and identifies species B pandas as species A. This is enough information to find
#Pr(testA). Plugging everthing into Bayes Theorem gives the following:
# 0.8 * 0.5 / ((0.5 * 0.8) + (0.5 * 0.35)) and this equals 0.7.

#Using the birth data, we can just update the Pr(A) in the previous equation with
#0.36 instead of 0.5 to get the correct answer.
#0.8 * 0.36 / ((0.36 * 0.8) + 0.64 * 0.35)) gives us a probability of 0.56.