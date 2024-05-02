# Psy1952 -- Spring 2023
# Course Instructor:  Patrick Mair <mair@fas.harvard.edu>
# Teaching Fellow: Johnny Castillo <jcastillo@g.harvard.edu>

## Lab 9 - Bayesian Analysis and Interpretation ##

## TODAY'S PLAN ##
# 1) Quick Review Game (~10min)
# 2) Problem Set 4: Applied Bayesian Analysis (~55min)
# 3) Game Discussion (~10min)

#### Quick Review Game ####
# ~ 10 minutes

# Materials:
# I will place a stack of cards in front of you
# There will be 16 colors with black font and 8 with colored font in each deck
# Your job is to match descriptions (in black font) to the concept it describes (in colored font)
# For each concept card, there is an associated correct answer and lure descriptor

# Methods:
# 1) Split into at most 5 groups
# 2) Match the correct descriptor to the concept it describes as quickly (and correctly) as possible
# 3) When you are done, give me your sets of paired cards with your group name on top
# Note: Also  please give me your leftover cards in a separate pile


#### Problem Set 4: Applied Bayesian Analysis ####
# ~55 minutes

#Load Packages
if (!require("pacman")) {install.packages("pacman"); require("pacman")}; p_load(devtools)
p_load(tidyverse, ggplot2, rstudioapi, graphics, rstanarm, brms, car, sjPlot, emmeans,
       lmerTest, bayestestR, car, mgcv, BayesFactor, broom.mixed, betareg, vioplot, glmmTMB, 
       bayesplot, parameters)


#Direct path to the folder holding script
current_path <- getActiveDocumentContext()$path    # gets path for R script
setwd(dirname(current_path)) # sets working directory to folder this R script is
getwd() 

#Now you will complete a complete Bayesian Analysis including:
#   (I) Exploratory Data Analysis
#   (II) Model Specification
#     - (a) Likelihood distribution selection
#   (III) Model Fit(s?)
#     - (a) Prior specification
#   (IV) Model Diagnostics (Check assumptions)
#     - (a) MCMC diagnostics (trace plots, ESS)
#     - (b) Distribution family diagnostics (PPC)
#     - (c) Prior sensitivity analysis
#     - (d) Goodness of Fit (LOO & WAIC)
#   (V) Results & Interpretation
#     - (a) Effects plots
#     - (b) Parameter interpretation (Median/CrI,ROPE)
#     - (c) Reporting Resulting for publication

# (optional) helper function: Beta_squeeze
#Will constrain values to fall between 0,1 -- but not exactly at 0 or 1.
#Rationale: a Beta distribution can only be estimated for values in the range between 0,1
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}

# For this problem set you will be working with data from Brendan Woo, a G5 working with 
#   Liz Spelke. 
# FYI: Brendan defends next month so if you see him around wish him luck!

# Background: 
# In Phase 1 of the experiment, they repeatedly showed 3-month-old infants that an actor 
#       preferred one object (e.g., a teddy bear) over another (e.g., a ball).
# In Phase 2, the bear moved somewhere new, and the ball moved to where the bear originally was.
# The question of interest was "do 14- to 15-month-old toddlers expect an actor to act 
#     in a way consistent  or inconsistent from their perspective? "

# Load Data
LTsn <- read.csv("https://mair.sites.fas.harvard.edu/datasets/LTsn.csv")
str(LTsn) #Examine Data
# 'Subject.ID' Unique subject ID
# 'Sex'
# The dependent variable (Looking Time, or 'LT') -- an index of toddlers' expectations, 
#     with longer looking times suggesting toddlers were surprised during a trial. 
# The variable ('Type'):
#     (Same-to-Actor; same object in its new location)
#     (Different-to-Actor; different object, old location)
# 'TrialPair: when in Phase 2 a trial occurs, with higher numbers indicating 
#     the trial occurred later.
#Note: They didn't want to push toddlers' patience, so looking time was capped at 30 seconds.


## I. Exploratory Data Analysis ##
#Note: You may need to clean the data to conduct certain visualizations/analyses

#Histogram
hist(LTsn$LT, main = "Looking time (expectations) in toddlers", xlab="time (seconds)")

# Complete additional visualizations you deem relevant for deciding on next steps 
#   for your analysis.

## II. Select Likelihood Model ##
# AKA: Determining the Data Generating Process (DGP)

#Based on the histogram of the DV and any of your chosen visualzations:
# (a) Pick a distribution family for the model 

# (a2) Justify your decision:

# (a3) (only if applicable) 
# Complete any additional data preparation necessary for your chosen distribution family:
#Transform data

## III. Model Fitting ##
# We want to take into account the random effects (2 of them):
# Hint: Each toddler has seen trial pairs 1, 2, and 3.
# 1) Slope: which toddler (Subject.ID) provided data; and
# Based on the hint we allow the effect of type to vary by Subject.ID
# 2) Intercept: at what point in an experiment (TrialPair) did a trial take place.

# (a) Fit Bayesian mixed-effects model with chosen distribution
# *For simplicity let's focus on slope priors and keep sigma and intercept priors at default. 
#Note: You should have 10000 iterations, but 1000 is a good place to start

# (a2) Examine priors

# (b) Define weak and informative priors
# Note: For simplicity you can set these as normal (Gaussian) priors centered at 0, 
#           varying only the standard deviations.

# Here's a formula to help:
# 'rstanarm' uses the following formula to compute it's default (weakly-informative) 
#       priors: N(0, sd(y)*2.5 / sd(x))
#  Use this formula for computing weak priors and then modify them by a factor of 
#       10 to make stronger priors
# Hint: Consider whether you want to make your prior SDs larger or smaller to make them
#       more informative!

# Weakly-informative priors

# Informative priors -- I'm going with 1/10 the SD from the weak priors
#  since a smaller SD means a narrower distribution, which corresponds
#  to a more informative prior.

# (c) Now, fit Bayesian mixed-effects models with default, weak, and informative priors
# Here are some guidelines:
# You should have two models and ensure to include your priors with the `prior = ` argument.
# You should fit your models using brms, and please use seed = 123 in each brm()
#      call (or alternatively set.seed(123) before each model fit)
#
#     IMPORTANT: Please save your computed models to .RDS files, and load them
#      along with your code using a conditional sequence!
# This is what I mean and briefly explained in class last week:
#
#            if (!file.exists('my_model.RDS')) {
#              model <- brm(y ~ x1 + x2, data = my_data, family = likelihood_family,
#                           prior = my_priors, seed = 123, iter = 2000)   
#              saveRDS(model, file = 'my_model.RDS') 
#            } else {
#              model <- readRDS('my_model.RDS')
#            }
#
#      And please submit your .RDS files along with your .R code on Gradescope
#
#     *** IF YOU DON'T DO THIS, I WILL NOT RUN YOUR CODE ***
#          There are too many of you for me to train all
#                  the models from scratch.

# Define weakly-informative prior for b_Type

# Define informative prior for b_Type


## IV. Model Diagnostics ##
# 1) MCMC diagnostics: Evaluate the quality of the MCMC sampling in each of your
#      models by reviewing the trace plots and the effective sample size (ESS) for
#      each parameter. 
# Hint: Use posterior_samples() to obtain the estimated values from the MCMC
#   chain and then use the mcmc_trace function

# (a1) Model with weakly-informative priors

# (a2) Model with informative priors

# (a3) Answer the following questions:
# Do the trace plots look "healthy"? Why or why not? 
# If your sampling procedure produced poor results, what would you change to improve
#   the model?


# 2) Distribution family diagnostics: Generate PPCs for both models to determine
#      whether in fact it proved reasonable to use a 0-1 inflated binomial likelihood.

# (a1) Model with weakly-informative priors

# (a2) Model with informative priors

# (a3 )Answer: Are we justified in choosing this likelihood?


# 3) Prior Sensitivity Analysis:  
#     NOTE: as with the model objects, please save the results of your bayesfactor_parameters()
#      call to an external .RDS file so that I don't have to wait for every submission to sample
#      from the priors! You can do this as follows:
#
#           if (!file.exists('PS4_bfp_weak.RDS')) {
#             bfp_weak <- bayesfactor_parameters(mod_weak, null = 0)
#             saveRDS(bfp_weak, 'PS4_bfp_weak.RDS')
#           } else {
#             bfp_weak <- readRDS('PS4_bfp_weak.RDS')
#           }
#           plot(bfp_weak)
#
#     Followed by the same thing for the informative priors. (This is an example using my own
#      code from the solutions file, so substitute your own object names as needed.)
#
#     Once again, please don't forget to include these files with your submission!

# (a) Check whether the priors we specified were incorporated into the model fit by running prior_summary() on each model.
# Model with weakly-informative priors

# Model with informative priors

# (b) Use bayesfactor_parameters() to generate plots of the posteriors and priors
#      for each model, compare to a (point, not range) null hypothesis of 0. 
# Do not interpret the Bayes' Factors, just inspect the plots. 

# Posteriors vs. priors: weak

# Posteriors vs. priors: informative


# (c) Prior Sensitivity Analysis
#   Here you will compare the 'TypeSameMtoMActor; parameter across both models
#     and discuss whether there are any apparent effects of shrinkage

# (c1) Save the summary of each brms model to a variable


# (c2) Extract the estimated values for the TypeSameMtoMActor parameter for each summary

# (c3) Print the estimated values


# (c4) Report whether there are any apparent effects of shrinkage


# (c5) A posterior estimate balances new information (likelihood) against previous knowledge (prior),
#   with this in mind, which (if any) component in your models is being weighed more heavily in our 
#   estimation of the posterior?


# (d) Goodness of Fit model comparisons
#   Here you will compare how well both our models fit using two model comparison measures:
#     leave-one-out-cross-validation (loo) and watanabe-akaike information criterion (WAIC)

#   In order to do so, we must consider two different models, with the same prior specifications.

# (d0) fit a new distribution (with a same prior as before) where you use a distinct but 
#  related distribution to model your phenomena.
# Note: Remember to save your model as specified previously

# Hint: Here are some distributions with theoretically similar distributions:
# Gaussian - Student's T & Cauchy
# Beta - zero/zero-one inflated Beta
# Poisson - Negative Binomial
# Gamma - Erlang

#Transform data to fit new model (optional)

#Fit new model


# (d1) Compare the fit of both our models using loo
# If you are not familiar with this package remember you can examine the package information by
#   doing ?loo

# Background information regarding how to interpret a loo comparison output:
# The 'elpd_diff' column indicates the difference in expected log pointwise predictive density 
#   between each model and the best model
# The 'se_diff' column provides the standard error of the difference. 

# (d2) Report which model you would prefer based on this metric
# Guidelines:
# A difference of less than 2 can be considered as "weak" evidence in favor of one model over another.
# A difference between 2 and 6 can be considered as "moderate" evidence.
# A difference greater than 6 can be considered as "strong" evidence.
# Note: These are just rough guidelines; interpretation of elpd differences should always be informed 
#     by the specific analysis context, the nature of the data, and your research question.

# (d3) Compare the fit of both our models using waic
# Note: This metric is interpretted in a similar fashion to AIC and BIC

# (d4) Based on these outputs, select a model to proceed with AND justify your decision


# (d5) Save your selected model to a variable named "Best_fit"


## V: Results and Interpretation ##
# Note: For this section we will focus our analysis on the fixed effect of b_TypeSameMtoMActor/Type

# (a) Create an effects plot of the chosen model

# (b) Interpretation of the fixed parameter
# (b1) Median

# (b2) Plot 95% & 50% CrI's only for the parameter 'TypeSameMtoMActor'
#95% CrI

#50% CrI

# (b3) Plot Region of Practical Equivalence (ROPE)
# Hint: You can estimate the rope range using the rope() function.
#       After calculating the rope range, save that to a variable and plot the variable


# (b4) Interpret the posterior probability of direction (pd)
# Guidelines: Due to a 1:1 correspondence of the pd with p-value, 1-pd is equivalent to p-value

# (b5) Interpret and report your results in publication format.
#Example of a brief summary I previously wrote for publication that you can reference:
# Results from this analysis indicate the effect of time has a 100% probability [pd] 
#   of being positive providing evidence that the intensity of negative feelings experienced 
#   at T2 (2.81 ± 0.98) was lower than feelings at T1 (3.45 ± 0.82; M = -0.77, HDI95% [-0.83, -0.70]
# In other words, people experienced less intense negative feelings one year after the 9/11 attack. 


#### Game Discussion ####
# ~10 minutes

