# PSY1952_lab02_GAM.R
# Juan Castillo - February 10 / 13, 2023
# Email: jcastillo@g.harvard.edu


##------------------------------------------------##
## PSY1952: Multivariate Analysis in Psychology   ##
##                                                ##
##          Lecture OT: Bayesian GA(M)Ms          ##
##------------------------------------------------##


## -------------------- Quick review: Working directories ------------------------------

# R is always pointed at a directory on your computer. 
# You can find out which directory by running the getwd (get working directory) function; 

getwd()

# If this directory is already where your data is stored, you're all set! 
# If not, we'll have to change the directory*

# first, we'll want to use a function from the rstudioapi package to get the file path.
if (!require(rstudioapi)) {install.packages("rstudioapi"); require(rstudioapi)} ##for the getActiveDocumentContext() function used below

current_path <- getActiveDocumentContext()$path    ## gets path for this R script
setwd(dirname(current_path)) ## sets working directory to folder this R script is in ("Unit05Model1.rda" & "Unit05Model2.rda"should also be there)

getwd() # and let's check where our current path is



## -------------------- Preamble: Package Management in R -----------------------------

## Let's load some packages with the help of PacMan
if (!require(pacman)) {install.packages("pacman"); require(pacman)} #Not the videogame character    ==    if(!require(X)) {install.packages(X); require(X)}
p_load(tidyverse, mgcv, HRW, mgcViz, gratia, itsadug, sjPlot, car, visreg, MPsychoR, vioplot, 
       emmeans, brms, vtree)



## -------------------------- Bayesian GA(M)M ----------------------------------
## ---------------------- Previosuly in 1952: GAM for count data ----------------------
## Taken from Wright, D. B., & London, K. (2009). Modern Regression Techniques Using R. Sage. 
## In the present study, 64 couples completed questionnaires about childbirth, the couple's 
## relationship, parent–baby bond quality 9 weeks post-childbirth, and ptsd symptoms

load(url("https://mair.sites.fas.harvard.edu/datasets/couples_ptsd.rda"))
couples_ptsd
## - bond ... parent–baby bond
## - positive_emo ... positive emotions in birth
## - negative_emo ... negative emotions in birth
## - control ... control in birth (related to feelings of personal security)
## - support ... support in birth
## - affection ... relationship affection
## - consensus ... relationship consensus

## - ptsd_symptoms ... number of PTSD symptoms (--> count response)


## ------- Example I: Negative-binomial regression, PTSD data -----------
hist(couples_ptsd$ptsd_symptoms, breaks = 40, xlab = "# of PTSD symptoms", main ="Distribution of PTSD symptoms") 
#Here we see an example of an overdispersed count variable 

#Original non-Bayesian model
fit_gam_nb2 <- mgcv::gam(ptsd_symptoms ~ s(bond) + positive_emo + negative_emo + control + support + consensus + affection, 
                         data = couples_ptsd, family = "nb", method = "ML")
summary(fit_gam_nb2)

## We re-fit the PTSD model from above in a Bayesian fashion (no random effects involved)
if (!file.exists("Unit05Model1.rda")) {
  bg_nb <- brm(ptsd_symptoms ~ s(bond) + positive_emo + negative_emo + control + support + consensus + affection, 
               data = couples_ptsd, family = negbinomial(), seed = 123,
               control = list(adapt_delta = 0.99))
  save(bg_nb, file = "Unit05Model1.rda")
} else {
  load("Unit05Model1.rda")
}

set.seed(123)
pp_check(bg_nb, type = "hist") + xlim(-1, 50)           ## PPC  --> looks good
print(summary(bg_nb), digits = 4)                       ## wrap print around summary to see more digits

## -) Smooth terms part: posterior estimated variance of the smooth term. 
## The larger these value the more wiggly the smooth. 

## If a credible interval does NOT include 0 there is evidence that a smooth is
##    required over and above a linear parametric effect. 

## -) Population-level effects part: Linear effects. Note that there is also sbond_1
##    which can be ignored.

## -) Family specific parameters: simply the shape estimate of the NB distribution

## Compared to the Frequentist fit from above:
fit_gam_nb2 %>% summary()            ## --> they match closely

## Effects plots for some selected variables:
plot(conditional_smooths(bg_nb))                              ## partial residual plot (smooth effect)
plot(conditional_effects(bg_nb, effects = "negative_emo"))    ## linear effect* for negative emotions
plot(conditional_effects(bg_nb, effects = "positive_emo"))    ## linear effect for positive emotions
## Note: they can be curved because of the log-link.


## ------ Example II: 0-1 inflated beta ---------
load(url("https://mair.sites.fas.harvard.edu/datasets/Mirman.Rdata"))
## Mirman, D. (2014). Growth Curve Analysis and Visualization Using R. CRC Press. 

## Word learning example revisited: 
## Subject ... A unique identifier for each participant (10 observations per participant).
## TP ... A categorical between-participants factor (transitional probability) with two levels (low and high)
## Block: A numeric variable indicating training block, ranging from 1 to 10 ("time")
## Accuracy: Proportion correct for a given participant in a given training  block, ranging from 0 to 1 (DV)

head(WordLearnEx, 20)
vtree(~ TP + Block, data = WordLearnEx)
## There are 280 observations in each TP condition, each of the 28 participants have 10 measurements (Block)
## In 1950 we've fitted a Gaussian and a beta mixed-effects polynomial model (Unit 16 & 20) 

## --- quick EDA ---
ggplot(WordLearnEx, aes(x = Accuracy)) + geom_histogram()
table(WordLearnEx$Accuracy)          ## accuracy table
## Note: exact 0 and 1 proportions --> instead of squeezing we go 0-1 inflated beta

## plot overall learning trajectories
ggplot(WordLearnEx, aes(Block, Accuracy, group = Subject)) +
  geom_line(aes(colour = Subject), size = 0.5) +
  scale_x_continuous(breaks = 1:10) +
  guides(colour = "none")

## let's apply a smoother
ggplot(WordLearnEx, aes(Block, Accuracy, group = Subject)) +
  geom_smooth(aes(colour = Subject), se = FALSE, size = 0.5) +
  scale_x_continuous(breaks = 1:10) +
  guides(colour = "none")
## fairly heterogeneous trajectories


## ------- model fit -----
## 0-1 inflated beta, factor-smooth interaction, random intercept and slope
if (!file.exists("Unit05Model2.rda")) {
  gamm_acc <- brm(Accuracy ~ TP + s(Block, by = TP) + (1 + Block|Subject), data = WordLearnEx, 
                  family = zero_one_inflated_beta(), 
                  seed = 123, control = list(adapt_delta = 0.99))
  save(gamm_acc, file = "Unit05Model2.rda")
} else {
  load("Unit05Model2.rda")
}

set.seed(123)
pp_check(gamm_acc, type = "hist") + stat_bin(bins = 10)   ## posterior predictive check (histogram version)  --> great
pp_check(gamm_acc)                  ## posterior predictive check (smooth version) 

print(summary(gamm_acc), digits = 3)        ## lots of stuff here
## -) Smooth Terms: variance of the smooths, if CrI includes 0 there is evidence that effect is linear.
## -) Group-Level Effects: simply the sd's of random intercept and slope.
## -) Population-Level Effects: intercept and main effect of TP (ignore the rest).
## -) Family Specific Parameters: parameters of the 0-1 inflated beta.

## --- effects plot
plot(conditional_smooths(gamm_acc))                                ## using brms function
## We see that in the Low condition the effect is pretty much linear. 
## TP High, almost meaningful, but not quite. Maybe at 89%*


##-------------- AND NOW THE LAB! ----------------##



##------------------------------------------------##
## Lab 02: Generalized Additive Models            ##
##                                                ##
## To GAM, or not to GAM, that is the question    ##
##------------------------------------------------##

# Today we're going to review the GAM workflow and elaborate
#  on when smooth terms are useful (and when they are not).

#### Data ####
#  This data comes from Dr. Tessa Charlesworth's work on modeling historical changes in
#  implicit and explicit attitudes. Huge thanks to her for sharing her data and code for us
#  to work with GAMMS today! 

#  If the data we work with today interests you, then you can read the publication here:
browseURL("https://tessaescharlesworth.files.wordpress.com/2022/07/charlesworth_change-4_published.pdf")
# The GAMMs from todays lab pertain to supplemental analyses in the paper.

# Load data
load(url("https://mair.sites.fas.harvard.edu/datasets/TSvectors_NEWDATA.RData"))
load(url("https://mair.sites.fas.harvard.edu/datasets/TSvectors_OLDDATA.RData"))


#### Packages ####

p_load(ggplot2, cowplot, mgcv, mgcViz, gratia, sjPlot)

## Analyzing sexuality bias over time ----
# Tessa's data are timeseries data, so she begins by organizing them into timeseries objects
sex.WtsALL <- ts(c(sex.Wts, sex.WtsNEW), start = c(2007, 1), frequency = 12)     ## implicit sexuality bias
sex.WEtsALL <- ts(c(sex.WEts, sex.WEtsNEW), start = c(2007, 1), frequency = 12)  ## explicit sexuality bias

# Next she parses these into data frames

# Implicit sexuality bias
sexuality_datfram <- as.data.frame(sex.WtsALL)
sexuality_datfram$year <- rep(c(2007:2020), each = 12)
sexuality_datfram$month <- rep(c(1:12), times = 14)
sexuality_datfram$conttime <- 1:dim(sexuality_datfram)[1]
colnames(sexuality_datfram)[1] <- "dscore"

head(sexuality_datfram)
dim(sexuality_datfram) # 14 years * 12 months ~ 168

# Explicit sexuality bias
sexualityE_datfram <- as.data.frame(sex.WEtsALL)
sexualityE_datfram$year <- rep(c(2007:2020), each = 12)
sexualityE_datfram$month <- rep(c(1:12), times = 14)
sexualityE_datfram$conttime <- 1:dim(sexualityE_datfram)[1]
colnames(sexualityE_datfram)[1] <- "dscore"

head(sexualityE_datfram)
dim(sexualityE_datfram)

# Let's do some descriptive visualizations
p_SIy <- ggplot(sexuality_datfram, aes(x = year, y = dscore)) + geom_point() + geom_smooth() + ggtitle('Implicit sexuality bias by year')
p_SIm <- ggplot(sexuality_datfram, aes(x = month, y = dscore)) + geom_point() + geom_smooth() + ggtitle('Implicit sexuality bias by month')
p_SEy <- ggplot(sexualityE_datfram, aes(x = year, y = dscore)) + geom_point() + geom_smooth() + ggtitle('Explicit sexuality bias by year')
p_SEm <- ggplot(sexualityE_datfram, aes(x = month, y = dscore)) + geom_point() + geom_smooth() + ggtitle('Explicit sexuality bias by month')

plot_grid(p_SIy, p_SIm, p_SEy, p_SEm)

# Q: Based on these plots, how useful do you expect GAMs to be for modeling these associations?


# For the bottom left plot, the association between explicit sexuality bias and year (time)
#  isn't especially "wiggly". Therefore, a smooth term may be unnecessarily complex to model the relation.
#  (relative to a parametric linear term).

# However, this is only apparent from the visualization!!
#  The significance test evaluating smooth terms will NOT give us this insight.


# Continuing with Tessa's analysis...
# We next fit GAMs with smooth terms for year and month.
# Let's do implicit first, then explicit.

# Implicit sexuality bias GAM
gam_sexuality_I <- bam(dscore ~ s(year, k = 14) + s(month, k = 12), data = sexuality_datfram, family = gaussian)

## note: bam() is just gam() for very large datasets
##  see ?bam() for info

# Let's do our GAM check
gam.check(gam_sexuality_I)

# k-index for year is definitely iffy (based on <1 standard)
# p-value for year is also highly significant; again, bad.
# edf ~ k' is fine though (edf clearly smaller)

# Even though we saw some wiggling in the scatterplots earlier, these statistics should
#  make us hesitate; we may not really want years to be smoothed, or perhaps the choice
#  of k (14) may be sub-optimal.

# A note: Even if we wanted to, we could not increase the amount of knots to be greater 
# then the available degrees of freedom for a given predictor; This is likely why Tessa
# chose the maximum amount of k's for each predictor.

# Ok, next let's evaluate thr residuals plots
gratia::appraise(gam_sexuality_I)  ## looks good

# And now we can plot the GAM fit
op <- par(mfrow = c(1, 2))
plot(gam_sexuality_I, shade = TRUE, main = 'implicit sexuality bias GAM')
par(op)

# Using sjPlot instead
plot_model(gam_sexuality_I, type = 'pred', terms = c('year'))
plot_model(gam_sexuality_I, type = 'pred', terms = c('year', 'month'))
# We can already see a little bit of why the smooth term for year may be unnecessary.
#  There is some variation in the slope over time, but it's not super "wiggly".

plot_model(gam_sexuality_I, type = 'pred', terms = c('month'))
plot_model(gam_sexuality_I, type = 'pred', terms = c('month', 'year'))
# But what if we wanted exact months or years?



# Next: Tessa's code for extracting and plotting the first derivative for the year term
#Q: Why do we want to examine the rate of change in the year term?

# Retrieve first derivatives of GAM
fd_sexuality <- derivatives(gam_sexuality_I, n = 13)
fd_sexuality_year <- fd_sexuality$derivative

#We can use this information to determine when the greatest decrease in the data was
which(fd_sexuality_year == min(as.vector(fd_sexuality_year))) #year 10 = 2017
min(as.vector(fd_sexuality_year)) # the derivative decreased by ~ -0.04

plot(as.vector(fd_sexuality_year)[1:13], type = "l",
     ylab = "First derivative", xlab = "", main = "SEXUALITY",
     ylim = c(-0.05, 0.05), axes = FALSE,
     lwd = 3, col = "coral"); box(); axis(2, at = seq(-0.05, 0.05, by = 0.025), 
     labels = seq(-0.05, 0.05, by = 0.025)); axis(1, at = 1:13, labels = 2008:2020); abline(h = 0, lty = 2); lines(x = c(1:13), y = c(as.vector(fd_sexuality_year)[1:13] - sd(as.vector(fd_sexuality_year)[1:13])/sqrt(14)), lty = 2, col = "cadetblue", lwd = 2); lines(x = c(1:13), y = c(as.vector(fd_sexuality_year)[1:13] + sd(as.vector(fd_sexuality_year)[1:13])/sqrt(14)), lty = 2, col = "cadetblue", lwd = 2)

# Without going deep here, what you can see is that the smooth term is monotonically decreasing.
#  There also isn't a ton of variation in the derivative (i.e., rate of change). So, perhaps
#  we should not prefer a nonparametric smooth term over a parametric linear term.

# Here is some extra code to compare these results to the first derivative for the month term:
plot(fd_sexuality_year[14:26], type = "l",
     ylab = "First derivative", xlab = "Month", main = "SEXUALITY",
     ylim = c(-0.05, 0.05), axes = T,
     lwd = 3, col = "coral"); abline(h = 0, lty = 2); lines(x = c(1:13), y = c(as.vector(fd_sexuality_year)[14:26] - sd(as.vector(fd_sexuality_year)[14:26])/sqrt(14)), lty = 2, col = "cadetblue", lwd = 2); lines(x = c(1:13), y = c(as.vector(fd_sexuality_year)[14:26] + sd(as.vector(fd_sexuality_year)[14:26])/sqrt(14)), lty = 2, col = "cadetblue", lwd = 2)

# Here we observe that the month term fluctuates between positive and negative slopes.
# THIS is an indication that the nonparametric spline is likely providing additional insight. 
# (As compared to a parametric linear term)


# Ok, finally, let's inspect the GAM model parameters
summary(gam_sexuality_I)

# Q: What kind of inference(s) can we make based on the significance of the non-parametric smooth terms?





# The test being evaluated is whether a smooth term's spline significantly differs from a horizontal line.
#  This is a *weak* test -- it's trivial for a spline to differ from a flat line.
#  As we'll see even more clearly below, significance doesn't truly indicate whether the spline meaningfully 
#  differs from a linear effect. 


#-- BAYESIAN ASIDE/RANT I -- Hypothesis Tests: "I know you are (significant), but what am I?" ~ H1
# *Keep this rant in mind for when we discuss Meta-analyses*

# Now for comparison, let's do the Explicit sexuality bias GAM
gam_sexuality_E <- bam(dscore ~ s(year, k = 14) + s(month, k = 12), data = sexualityE_datfram, family = gaussian)

# GAM check
gam.check(gam_sexuality_E)

# Similar to the implicit model in terms of the evaluation of the smooth term for year -- not a great spline.

# Residuals plots
appraise(gam_sexuality_E) ## these look good

# Plot GAM fit
op <- par(mfrow = c(1, 2))
plot(gam_sexuality_E, shade = TRUE, main = 'explicit sexuality bias GAM')
par(op)

# with sjPlot
plot_model(gam_sexuality_E, type = 'pred', terms = c('year'))
plot_model(gam_sexuality_E, type = 'pred', terms = c('year', 'month'))
plot_model(gam_sexuality_E, type = 'pred', terms = c('month'))
plot_model(gam_sexuality_E, type = 'pred', terms = c('month', 'year'))


# Here it's even more apparent that the "year" effect is basically a straight line.
#  BUT it's not a *flat* line.
# Q: Do you expect the smooth term associated with month to be significant?

summary(gam_sexuality_E)

# ...all the smooth terms are (unsurprisingly) significantly different from horizontal lines. 



## A little bit of both: Modeling year as parametric, month as smooth ----
# Let's go back to the implicit sexuality bias example, and fit a new GAM where year is modeled as parametric
gam_sexuality_I_year.param <- bam(dscore ~ year + s(month, k = 12), data = sexuality_datfram, family = gaussian)

# If we do our workflow from here, we should see some differences
gam.check(gam_sexuality_I_year.param)  ## the fit looks less ideal now, but we'll keep this specification for now
appraise(gam_sexuality_I_year.param)

# Here are our effects plots
plot_model(gam_sexuality_I_year.param, type = 'pred', terms = c('year'))
#plot_model(gam_sexuality_I, type = 'pred', terms = c('year'))
plot_model(gam_sexuality_I_year.param, type = 'pred', terms = c('year', 'month'))
#plot_model(gam_sexuality_I, type = 'pred', terms = c('year', 'month'))
plot_model(gam_sexuality_I_year.param, type = 'pred', terms = c('month'))
plot_model(gam_sexuality_I_year.param, type = 'pred', terms = c('month', 'year'))

# Model summary
summary(gam_sexuality_I_year.param)

# Now we can actually interpret the year effect like a normal parametric effect! (ie. via the beta parameter)
#  Examining the estimated value it becomes obvious that d-score (implicit bias) decreased with time.

# And finally, if we compare this to the first model, we can see a major decrease in AIC
# Final Q: What is the significance of this?
AIC(gam_sexuality_I_year.param, gam_sexuality_I)

#Another option: Bayesian Information Criterion
BIC(gam_sexuality_I_year.param, gam_sexuality_I)
