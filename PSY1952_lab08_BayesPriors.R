# Psy1952 -- Spring 2023
# Course Instructor:  Patrick Mair   <mair@fas.harvard.edu>
# Teaching Fellow: Johnny Castillo <jcastillo@g.harvard.edu>

## Lab 8 - Bayesian Priors ##

#Load Packages
if (!require("pacman")) {install.packages("pacman"); require("pacman")}; p_load(devtools)
p_load(devtools)
install_github("nicebread/BFDA", subdir="package")
p_load(tidyverse, ggplot2, rstudioapi, graphics, BFDA, rstanarm, brms, car, sjPlot, emmeans,
       lmerTest, bayestestR)

#Direct path to the folder holding script
current_path <- getActiveDocumentContext()$path    # gets path for R script
setwd(dirname(current_path)) # sets working directory to folder this R script is
getwd() 


#### Substantively Motivated (Informative) Priors ####
# In the National Basketball Association (NBA Tm) each team plays 82 games per season. 
# Suppose our team is so-so and equally likely to win or lose each of its games.
# Let's think about the team's win-loss record as the season progresses. 
# What proportion of the time will our team have more wins than losses? 

# For our team wins and losses are determined by tossing a fair coin (ie. binomial pmf).
# We want to estimate the proportion, but we want our prior to be as uninformative as
# possible to *bias* the results as little as possible.
# Therefore you may start out by hypothesizing a uniform prior:
set.seed(42) # Set seed for reproducibility
rand.unif <- runif(1000, min = 0, max = 1)
a <- 0; b <- 1
hist(rand.unif, freq = FALSE, xlab = 'x', ylim = c(0, 1.4), xlim = c(-.2, 1.2), density = 20, main = "Uniform distribution for the interval [0,1]")
curve(dunif(x, min = a, max = b), from = -.2, to = 1.2, n = 82, col = "darkblue", lwd = 3, add = TRUE, yaxt = "n", ylab = 'probability')


# Now we're done right? 


# Well, that's not quite right... we can't be equally likely to have 0 wins and 100% losses & 0 losses & 100% wins
# That's impossible. That's also why the uniform prior is often called an improper prior; it places prior beliefs in
# places where its just not possible and therefore adds noise to our estimation process


# Onward and upward, we still want to have an uninformative prior... maybe we'll use our subjective knowledge!
# You might think a team would most likely be ahead about half the time throughout a season.

# As such you could model your prior beliefs as a normal distribution 
# N(0.5, .2)
set.seed(123)
x1 <- sort(rnorm(82, mean = 0.5, sd = .2))
hist(x1, freq = FALSE, main = "Normal Distribution (0.5, 0.2)", breaks = 50) 
f_x1 <- dnorm(x1, mean = 0.5, sd = .2)      # theoretical density values
lines(x1, f_x1, col = "blue", lwd = 3)    # theoretical distribution (density)

# Nice job! Now lets simulate some data and evaluate whether our prior matches our hypothesis:
# Set seed for reproducibility
set.seed(1952)

# Define variables
m <- 20000
n <- 82
prop.ahead <- numeric(m)

# Calculate proportion of games where our team leads
for (i in 1:m) {
  x <- sample(c(-1,1), n, repl=T)
  cum <- cumsum(x)
  ahead <- (c(0, cum) + c(cum,0))[1:n]
  prop.ahead[i] <- mean(ahead >= 0)
}

# Create histogram
hist(prop.ahead, breaks = seq(0, 1, by = .1), prob = TRUE, col = "skyblue2", 
     xlab = "Proportion", main = "Proportion of 82-game season where our team leads")
curve(dbeta(x, .5, .5), add = TRUE, col = "blue", lwd = 3) # Add beta distribution curve

#The "bathtub shaped" histogram above shows an approximate distribution of the proportion of time 
#   during a season that such a team is ahead.
# It appears that half the time is the least likely proportion for time being ahead.
# Q: What's happening here?


# O   
#/|\
#/ \   Wait for it...

# Explanation: even though over time the average will eventually settle back to 0.5 (equal # of wins & losses), 
#   the amount of time our team is actually ahead or behind will follow a U-shaped distribution because 
#   once you are better than 50/50, it's harder to fall under 50/50 (because of random chance).
                                                                                                                                                                                                              
# Wow. So the normal distribution is NOT the answer to everything. 

# Is there anything that could have saved us from this folly?
# Besides experience (posterior --> prior)? Theory.
# Above when we set up this problem we mentioned it was a proportion (0-1)
# The curve is the PDF of Beta(.5,.5).

# In conclusion, there is no such thing as a prior with "truly no information"
# But we can use the anticipated distribution of the findings to enforce ignorance

# Parting thoughts: alternatively, you could have modeled this with a Gamma if you asked a slightly
#   different question: How long between wins? 
# Note: the way you would interpret the magnitude of the resulting parameter would be flipped

# Part of the beauty of Bayes inference is that we could look at both of these if we wanted to, and be confident
#   we're evaluating the evidence in the most empirical way possible (no multiple comparisons here! Just evidence)

# ** Back2Slides **



#### Prior predictive checks ####
# No background story here, because I want you to focus on the process


# Set simulation parameters
set.seed(42); n <- 100
x <- runif(n, 0, 10) #Simulate responses from uniform prior distribution
slope <- 2
intercept <- 3
noise <- rnorm(n, 0, 2)
# Model: y ~ m * x + b + E (error)
y <- slope * x + intercept + noise # Simulate predictions


# Define Priors: b ~ N(0, 3), m ~ N(0, 1)
prior_intercept_mean <- 0
prior_intercept_sd <- 3
prior_slope_mean <- 0
prior_slope_sd <- 1

# Simulate data from prior
n_simulations <- 1000
prior_intercepts <- rnorm(n_simulations, prior_intercept_mean, prior_intercept_sd)
prior_slopes <- rnorm(n_simulations, prior_slope_mean, prior_slope_sd)

# Generate prior predictive data
prior_predictive_data <- data.frame()
for (i in 1:n_simulations) {
  sim_intercept <- prior_intercepts[i]
  sim_slope <- prior_slopes[i]
  sim_y <- sim_intercept + sim_slope * x
  sim_data <- data.frame(x = x, y = sim_y, simulation = rep(i, length(x)))
  prior_predictive_data <- rbind(prior_predictive_data, sim_data)
}
# Add our actual data to the dataframe
prior_predictive_data$y_pred <- slope * x + intercept + noise

#Visualize Prior Predictive data
ggplot(prior_predictive_data, aes(x = x, y = y, group = simulation)) +
  geom_line(alpha = 0.1, color = "blue") +
  geom_smooth(aes(y = y_pred), method = "lm", se = FALSE, color = "red", size = 2) +
  labs(title = "Prior Predictive Checks - Linear Regression Model",
       x = "X",
       y = "Y") +
  theme_bw()

#Q: What do we think?


# Our simulated data does overlap with the prior predictive distribution to a degree, but besides the intercept
#   the slope is not located in the center of mass. 
# The explanation for this is that our simulated data slope was 3, whereas our prior predictive distribution was 
#   sampled from a normal distribution (0,1), making a slope value of 3 particularly unlikely (3 SD)

# ** Back2Slides **


#### Prior Sensitivity Analysis - Mixed ####
## Example: Korean Speech
## Dataset from : Winter, B., & Grawunder, S. (2012). The phonetic profile of Korean formality.
## Journal of Phonetics, 40, 808-815. Variables in model:
## We are interested whether the voice pitch differs across attitude and sex.

## data preparation 
politeness <- read.csv("https://mair.sites.fas.harvard.edu/datasets/politeness_data.csv") %>% as_tibble()
politeness <- politeness %>% 
  dplyr::select(-frequency) %>% 
  mutate_all(as.factor) %>% 
  add_column(frequency = politeness$frequency) %>% 
  rename(sex = gender, id = subject)
dim(politeness)
print(politeness, n = Inf)

## response: frequency voice pitch measured in Hz
## sex: 3 males, 3 females, between-subjects condition
## attitude: polite vs. informal, within-subjects condition
## id: subject ID (random effect)
## scenario: 7 speech scenarios (random effect)

## quick EDA 
with(politeness, table(id, scenario))       ## full crossed design
with(politeness, table(attitude, scenario))
with(politeness, table(attitude, sex))

ggplot(politeness, aes(x = frequency)) + geom_histogram(bins = 10, fill = "white", col = "black")      ## response

## interaction plot (sex, attitude)
politeness %>% group_by(sex, attitude) %>%
  summarise(freq_mean = mean(frequency, na.rm = TRUE)) %>%
  ggplot(aes(x = sex, y = freq_mean, color = attitude)) +
  geom_line(aes(group = attitude)) +
  geom_point()


## Frequentist model fit 
## Let's start with fitting the full model with lme4. This is not part of a 
## prior sensitivity analysis, but it is helpful to give us some benchmark estimates
## -) full fixed-effects interaction;
## -) two random intercepts for id and scenario
## -) random slope for attitude at participant level

fit_speech_lmer <- lmer(frequency ~ sex*attitude + (1 + attitude|id) + (1|scenario), data = politeness)
plot(fit_speech_lmer)          ## quick residual check --> OK
summary(fit_speech_lmer)       ## random slope sd very small (singular warning)

##  Bayesian model fits: brms 
get_prior(frequency ~ sex*attitude + (1 + attitude|id) + (1|scenario), data = politeness)  ## default brms priors
## Note that it uses a student_t with df = 3, location = 203.9, and scale = 82.7 intercept prior. 
## We could use this one, or overwrite it with our Gaussian prior (which we will do).
## Also, it uses a student_t for sigma (actually, is uses a half-t). Again, we could use this one, or overwrite it with 
## our exp(0.015) which we will do. We keep the remaining priors (random effects) as they are.

## ------ model 0: use the uninformative rstanarm defaults in brms
priors0 <- c(prior(normal(194, 164), class = Intercept),
             prior(normal(0, 325.75), class = b, coef = "sexM"),
             prior(normal(0, 325.75), class = b, coef = "attitudeinf"),
             prior(normal(0, 374.62), class = b, coef = "sexM:attitudeinf"),
             prior(exponential(0.015), class = sigma))
priors0     
## Q: Whats different here than what we specified in class?
## We should plot these priors here, but we'll do that further below in conjunction with the posterior

if (!file.exists("fit_speech_brm0.rda")) {
  fit_speech_brm0 <- brm(frequency ~ sex*attitude + (1 + attitude|id) + (1|scenario), data = politeness, 
                         prior = priors0, seed = 123)   
  save(fit_speech_brm0, file = "./fit_speech_brm0.rda") 
} else {
  load("fit_speech_brm0.rda")  
}
prior_summary(fit_speech_brm0)         ## check whether it actually took the proper priors --> OK


## ------ model 1: we decrease the sd for the parameters we're interested in: N(0, 100) slope priors
priors1 <- c(prior(normal(194, 164), class = Intercept),
             prior(normal(0, 100), class = b, coef = "sexM"),
             prior(normal(0, 100), class = b, coef = "attitudeinf"),
             prior(normal(0, 100), class = b, coef = "sexM:attitudeinf"),
             prior(exponential(0.015), class = sigma))
priors1     ## our new priors

if (!file.exists("fit_speech_brm1.rda")) {
  fit_speech_brm1 <- brm(frequency ~ sex*attitude + (1 + attitude|id) + (1|scenario), data = politeness, 
                         prior = priors1, seed = 123)   
  save(fit_speech_brm1, file = "./fit_speech_brm1.rda") 
} else {
  load("fit_speech_brm1.rda")  
}
prior_summary(fit_speech_brm1)         ## check whether it actually took the proper priors --> OK
## Looks good; for the priors we haven't specified it uses the defaults

## ------ model 2: now we use N(0, 20) slope priors
priors2 <- c(prior(normal(194, 164), class = Intercept),
             prior(normal(0, 20), class = b, coef = "sexM"),
             prior(normal(0, 20), class = b, coef = "attitudeinf"),
             prior(normal(0, 20), class = b, coef = "sexM:attitudeinf"),
             prior(exponential(0.015), class = sigma))
priors2

if (!file.exists("fit_speech_brm2.rda")) {
  fit_speech_brm2 <- brm(frequency ~ sex*attitude + (1 + attitude|id) + (1|scenario), data = politeness, 
                         prior = priors2, seed = 123)   
  save(fit_speech_brm2, file = "fit_speech_brm2.rda") 
} else {
  load("fit_speech_brm2.rda")  
}

prior_summary(fit_speech_brm2)         ## check whether it actually took the proper priors  --> OK

## ------- compare results 
## Let's also cross-check the results of the brms fits
parameters::model_parameters(fit_speech_brm0, effects = "fixed")          
parameters::model_parameters(fit_speech_brm1, effects = "fixed")          
parameters::model_parameters(fit_speech_brm2, effects = "fixed")          ## uh, quite some shrinkage
## We see that the N(0, 20) prior did quite some shrinkage, especially for sex and interaction
## (i.e., the parameters are pulled towards 0)
## What should we do? We can certainly go with the N(0, 100) model. On the other hand, this shrinkage is not necessarily a 
## bad thing, given the small n. We are just more "defensive"/"conservative" about the effects. 

## ------ compare BFs
## Bayes factors (with point H0)
set.seed(111)
bf_uninf <- bayesfactor_parameters(fit_speech_brm0, null = 0)
bf_weak <- bayesfactor_parameters(fit_speech_brm1, null = 0)
bf_inform <- bayesfactor_parameters(fit_speech_brm2, null = 0)

bf_uninf
bf_weak
bf_inform    
## quite some differences in the BFs

plot(bf_uninf) + xlim(-200, 200)      ## plot priors and posteriors (from the BF object)
plot(bf_weak) + xlim(-200, 200)
plot(bf_inform) + xlim(-200, 200)

## Now what? I'd probably go with the N(0, 100). 
## Otherwise there might be too much shrinkage for sex.
## This is because we are weighing both knowledge and new information.
## We're good to selectively choose the weak prior because our posterior will represent
##  some of the first knowledge communicated about whatever phenomena were investigating --
##  if it wasn't, then we should probably be using substantive priors.
## As our research matures and we build on our own priors, then we have to be more conservative
##  about our priors -- effectively forcing our accumulated results to defend themselves.

## Remember -- Bayesian Statistics is a learning paradigm. You can choose to remain ignorant, 
#   or you can start building knowledge for the future, today.


#### Advanced: Bayes Factors ~ BFDA ####
# Bayes Factor Design Analysis (BFDA)
# Essentially, one bayesian equivalent to the power analysis. Given a set of parameters and instructions, you can simulate
#   the probability of observing the results you obtained. You can also do other things such as calculate the number of people
#   and with the rigor of Bayes behind you, you can also do really interest things like sequential hypothesis testing -- but
#   I'm getting ahead of myself.

# For my simulation I will be using the specifying an abtest which allows for Bayes Factor Design Analyses comparing Log Odds 
#   effects sizes (which is necessary given the logit link present in all GLMs). 
# For our efect size, we will select a small ES according to log odds 0.27 (equivalent to cohens d of .15 {small=.2})
# We'll keep the default priors for now N(0,1).

# The minimum allowed number of samples (simulated participants) in each simulation was 20, and the maximum was 80.
# it is recommended to do 10,000 studies to promote stability, but be aware this can take VERY long
# Given the uncertainty associated with the true effects, the alternative hypothesis was evaluated via 2 sided tests.
# Finally, seeds were used in order to promote reproducibility.

#Define and run simulations for BFDA // Load data
# Null Hypothesis (H0)
if (file.exists("./NUll_bfda.rda")) {
  load(file = "./NUll_bfda.rda")
} else {
  # Install and load the BFDA package if not already installed
  if (!requireNamespace("BFDA", quietly = TRUE)) {
    install.packages("BFDA")
  }
  library(BFDA)
  sim.H0 <- BFDA.sim(expected.ES = 0, type = "abtest", options.sample = list(effecttype = "logOR"),
                     prior = list("normal", list(prior.mean = 0, prior.variance = 0.1)),
                     n.min = 20, n.max = 250, B = 2000, alternative = "two.sided",
                     verbose = TRUE, cores = 4, seed = 111234)
}
# Alternative Hypothesis (H1)
if (file.exists("./Medium_bfda.rda")) {
  load(file = "./Medium_bfda.rda")
} else {
  # Install and load the BFDA package if not already installed
  if (!requireNamespace("BFDA", quietly = TRUE)) {
    install.packages("BFDA")
  }
  library(BFDA)
  
  Medium.H1 <- BFDA.sim(expected.ES = rnorm(100, 0.907, 0.1), type = "abtest",
                        options.sample = list(effecttype = "logOR"),
                        prior = list("normal", list(prior.mean = 0.0, prior.variance = 0.5)),
                        n.min = 20, n.max = 80, B = 10000, alternative = "two.sided",
                        verbose = TRUE, cores = 4, seed = 1952)
}

# Once we simulate we can do a bunch of cool things!
# Note: `Boundary` in the below sections refers to our desired Bayes factor levels

# Refresher on how to interpret Bayes Factors:
# BF < 1: The evidence supports H0 over H1. A smaller value indicates stronger evidence in favor of H0.
# 0.33 < BF < 1: Weak or anecdotal evidence for H0.
# 0.1 < BF <= 0.33: Moderate evidence for H0.
# BF <= 0.1: Strong evidence for H0.
# BF = 1: The data is inconclusive, there is no evidence in favor of either hypothesis.
# BF > 1: The evidence supports H1 over H0. A larger value indicates stronger evidence in favor of H1.
# 1 < BF <= 3: Weak or anecdotal evidence for H1.
# 3 < BF <= 10: Moderate evidence for H1.
# BF > 10: Strong evidence for H1.

#Examine at what participant number the false discovery rate would be adequate
SSD(Medium.H1, alpha=.05, boundary=c(1.1)) #5% false discovery rate; achieved at 20
# What this plot is showing is that as more samples are accumulated, there appears to be more power overall

#Evaluating the probability of obtaining a certain effect size given max participants
BFDA.analyze(Medium.H1, design="fixed", n=80, boundary = 1) #Most support the null 
BFDA.analyze(Medium.H1, design="fixed", n=80, boundary = 3) #Most inconclusive; some H1
BFDA.analyze(Medium.H1, design="fixed", n=80, boundary = 6) #Most inconclusive; some H1
# What we can immediately observe from these analyses is that at a fixed sample size of 80
#   there is evidence for the alternative hypothesis, but this evidence decreases with a 
#   more conservative (ie. larger) boundary/BF.

#Another thing we can do is use sequential hypothesis testing to determine what our sample size should be
# The main idea is that we should examine how our Bayes Factor changes as we accumulate evidence
# This takes advantage of the fact that all the information is already self-contained within Bayes theorem
#   and therefore, we can run as many comparisons as we wish without biasing the results
# Practically, this would confer the benefit that you can stop your study at whatever point the data supports 
#   an evidence threshold which is chosen by the modeler.
BFDA.analyze(Medium.H1, design="sequential", n.min=20, n.max=80, boundary=1.6)
# You can essentially consider this as how much uncertainty you want in your estimates relative to sample size

#Visualize the distribution of bayes factors for both our hypotheses at n=80
evDens(BFDA.H1=Medium.H1, BFDA.H0=sim.H0, n=80, boundary=c(1/3,3), xlim=c(1/10, 20))
# Enables us to see there is strong evidence to support the alternative hypothesis, 
#   with a large skew towards larger effect sizes

#Visualize the accumulation of BF support for either alternative hypothesis
plot(Medium.H1, n.min=20, n.max=80, boundary= 1/3)
# Interpreted the same as any sequential sampling model (drift diffusion modeling, etc.)
# Basically shows how evidence is accumulated over time:
#   This enables us to combine all the knowledge we have seen above in a graphical format