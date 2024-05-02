# PSY1952_lab04_LGM_V2.R
# Juan Castillo - February 24/27, 2023
# jcastillo@g.harvard.edu

##------------------------------------------------##
## PSY1952: Multivariate Analysis in Psychology   ##
## Lab 04:  Latent Growth Models                  ##
##------------------------------------------------##

# In this lab, we're going to utilize simulated longitudinal data from the 
#   "longitudinalData" package to practice constructing Latent Growth Models.

# We will develop "theories" with variables in the data, and will subsequently
#   illustrate these narratives as path diagrams. Finally, we will use
#   lavaan syntax to define the latent growth structures in R.

#### Packages ####
require(pacman)
p_load(rstudioapi, longitudinalData, tidyverse, effectsize, viridis, imager, semPlot, lavaan, stringr)

# First I point my working directory to wherever this RScript (and the data) resides
current_path <- getActiveDocumentContext()$path    ## gets path for this R script
setwd(dirname(current_path)) ## sets working directory to folder this R script is in
getwd() # and let's check where our current path is


#### Data ####
# Here are our simulated data:
data(artificialJointLongData)

# First, we rename these variables so the data reflect a fictional
#  anxiety study involving self-report (Subjective) and physiological (objective)
#  measures of anxiety for 150 participants across 4 time points.
anxiety <- with(artificialJointLongData,
                data.frame(id = id, anx_t0 = x0, anx_t1 = x1, anx_t2 = x2, anx_t3 = x3,
                           cort_t0 = w0, cort_t1 = w1, cort_t2 = w2, cort_t3 = w3,
                           gsr_t0 = v0, gsr_t1 = v1, gsr_t2 = v2, gsr_t3 = v3))
# Anx ~ Self Report & Cort/GSR ~ Physiological
# GSR ~ a measure of skin conductance related to sweat & often used to measure physiological arousal

# For simplicity with lavaan, we're going to standardize all the values
anxiety <- anxiety %>% standardize()

# To make things theoretically more interesting, let's divide
#  the participants into two groups: a CBT group and a control group.
anxiety$grp <- factor(c(rep('CBT', 75), rep('control', 75)))

# Let's assume that these are existing groups (i.e., people who have been in
#  CBT and people who have not) rather than an experimental manipulation.

# Okay, what do the data look like?
head(anxiety)

#Change variable types to simplify transformations
anxiety$anx_t0 <- as.numeric(anxiety$anx_t0); anxiety$anx_t1 <- as.numeric(anxiety$anx_t1); anxiety$anx_t2 <- as.numeric(anxiety$anx_t2); anxiety$anx_t3 <- as.numeric(anxiety$anx_t3)
anxiety$cort_t0 <- as.numeric(anxiety$cort_t0); anxiety$cort_t1 <- as.numeric(anxiety$cort_t1); anxiety$cort_t2 <- as.numeric(anxiety$cort_t2); anxiety$cort_t3 <- as.numeric(anxiety$cort_t3)
anxiety$gsr_t0 <- as.numeric(anxiety$gsr_t0); anxiety$gsr_t1 <- as.numeric(anxiety$gsr_t1); anxiety$gsr_t2 <- as.numeric(anxiety$gsr_t2); anxiety$gsr_t3 <- as.numeric(anxiety$gsr_t3)

# Wide format is good for lavaan, but let's also make a long format
#  version for descriptive visualizations and summaries.
anxiety_long <-  
  pivot_longer(anxiety, cols = c(anx_t0, anx_t1, anx_t2, anx_t3, 
                                 cort_t0, cort_t1, cort_t2, cort_t3,
                                 gsr_t0, gsr_t1, gsr_t2, gsr_t3), names_to = c('var', 'time'), names_sep = '_') %>%
  mutate(time = as.numeric(gsub('[t]', '', time)))

# Okay, we're ready to get started!


#### EDA ####
# Because these are simulated data, we can't expect them to align perfectly  with our
#  professional knowledge. However, a narrative is a critical component of LG and other 
#  path models as our theories often guide our model specifications (as we'll see today).

#  I tried to prepare the dataset so that it has at least a few properties that
#  seem sensible - let's take a look!

# Quick look at group means for each measurement (anxiety, cortisol, and skin conductance)
anxiety_long %>% group_by(grp, var) %>% dplyr::summarize(mean = mean(value), sd = sd(value))

# So we already have a hint from the exploratory analysis that the CBT group has
#  lower anxiety measures (psychological and physiological) than the control group.

# A plot can give us a little more insight, since we'll be interested in trajectories over time
ggplot(anxiety_long, aes(x = value, group = time, color = time)) +
   geom_density() + facet_wrap(~grp + var) + scale_color_viridis(option = 'magma', end = .8) +
   ggtitle('Densities of anxiety values over time')
# These densities confirm our descriptive summaries as more extreme anxiety-related values
#   were typical for the control group - particularly for the objective measures.

# We can also get a sense of the way these variables change over time by looking at a few example
#  subjects' individual trajectories of each measurement, taking subjects from each group:
ggplot(anxiety_long %>% filter(id %in% c('s1', 's2', 's76', 's77')),
       aes(x = time, y = value, color = var)) +
   geom_point() + geom_line(aes(group = var)) + facet_wrap(~grp + id, nrow = 1)
#From this visualization it seems like CBT seems to maintain anxiety levels, whereas
#   they generally increase over time in the control group.


#### Theoretical framework ####
# Okay, so let's assume for the moment that we want to analyze self-reported
#  anxiety levels (i.e., the "anx" variable) as our latent construct of interest.

# What exactly does it mean for this to be our "latent" construct?

# The idea is that we have a series of measured (or observed) variables,
#  and we have some theory about the constructs or processes that "cause"
#  them. In this case, our measured variable of interest is the anxiety
#  self-report measure, and we imagine it to be caused by some latent
#  anxiety construct.
plot(load.image('LGM1.jpeg'), axes = FALSE)



# This relationship can be described by two parameters of the latent construct:
#  its intercept and its "shape" (aka slopes over time). 

# Because the anxiety variable was measured in discrete timepoints, 
#  we can describe the parametric relationship between the latent intercepts and 
#  slopes (shape) across each timepoint.
# To keep things simple, we'll assume a linear growth trajectory for the shape of anxiety, 
#   and a constant effect for the intercept.
# Let's examine what this would look like in a diagram:
plot(load.image('LGM2.jpeg'), axes = FALSE)

# This is how we would indicate this in lavaan syntax:
LMG_mod_1 <- '
  # Linear growth curve specification for latent anxiety and measured anxiety
  anx_i =~ 1*anx_t0 + 1*anx_t1 + 1*anx_t2 + 1*anx_t3 #Constant effect
  anx_s =~ 0*anx_t0 + 1*anx_t1 + 2*anx_t2 + 3*anx_t3 #Linear
'

# Okay, let's fit this latent growth model!
LGM_fit_1 <- lavaan::growth(LMG_mod_1, data = anxiety, estimator = "ML")

# Here's our fitted path diagram -- look familiar?
semPaths(LGM_fit_1, what = "est", edge.label.cex = 1.5, label.cex = 2,
         esize = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 1, mar = c(3, 5, 3.5, 5), colFactor = .3)

# Fantastic, as we can see the structure matches exactly what we hypothesized!



# At this point we would likely want to examine goodness of fit measures, but let's
#   pretend were conducting a confirmatory data analysis and have a very strong
#   a-priori theory. This theory states other (exogenous) variables might influence 
#   the latent construct -- like our grouping variable.

# Specifically, we expect that there are group differences in latent
#  anxiety across the groups affecting both the intercept and the shape, so we will model
#  this additional relationship with a time-invariant covariate.
plot(load.image('LGM3.jpeg'), axes = FALSE)

# Adding this additional covariate in lavaan just involves building on what we have already modeled
LMG_mod_2 <- '
  # Linear growth curve specification for latent anxiety and measured anxiety
  anx_i =~ 1*anx_t0 + 1*anx_t1 + 1*anx_t2 + 1*anx_t3
  anx_s =~ 0*anx_t0 + 1*anx_t1 + anx_t2 + anx_t3
  
  # Exogenous time-invariant covariate specification for group
  # Note the lack of terms indicating time
  anx_i ~ grp
  anx_s ~ grp
'
# Note: the difference between =~ and ~

# Okay, let's fit it:
LGM_fit_2 <- lavaan::growth(LMG_mod_2, data = anxiety, estimator = "ML")

# Here's our fitted path diagram:
semPaths(LGM_fit_2, what = "est", edge.label.cex = 1.5, label.cex = 2,
         esize = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 1, mar = c(3, 5, 3.5, 5), colFactor = .3)

# Again, here's an example of how drawing path structures is incredibly helpful in modeling LPMs.

summary(LGM_fit_2)


# Now let's complicate this a little further by considering our
#  other measured variables -- cortisol and GSR. 
# These variables were observed at the same timepoints as 'anx'

# Depending on our theory, we might model the relationships between these variables
#  differently. For this analysis, we're going to assume that physiological
#  measurements also influence anxiety *measurements*, but that they are
#  *not* "caused" by the latent anxiety construct we're predominantly
#  interested in.

# This gives us the following diagram for the overall relationship structure:
plot(load.image('LGM4.jpeg'), axes = FALSE)

# We might believe this to be the case because, for example, we theorize
#  that cortisol and GSR measurements are "caused" by a different latent
#  process, such as a latent "arousal" variable, which is distinct from
#  our latent anxiety variable.

# Great, we established the structure of the relationships, but 
#   before we implement this in lavaan we should decide on the 
#   *nature* of those relationships as well.

# This might sound scary and *ambiguous*, but in reality we already
#    did this when we chose a linear growth curve for the slope effect of 
#    the latent anxiety variable on anxiety measurements.
#  Another example is our choice of a time-invariant effect for our group
#    covariate (CBT vs. control) - this selection communicates we theorize that
#    covariates only affect anxiety measurements by acting on latent anxiety, and 
#    that the effect is constant over time.

# Back to Cortisol and GSR measurements:
#  Q: What kinds of relationships might those measurements have to the anxiety
#  measurements over time? 



# Normally we would rely on prior knowledge to inform our modeling decisions.
#   But, I'm going to make things up :)

# CORTISOL:
# Suppose that Cortisol has a "lingering" influence on anxiety measurements --
#   i.e., Cortisol levels from the initial timepoint continue to influence 
#   anxiety measurements at subsequent timepoints.
# This would make sense if Cortisol levels quickly spike and take time to 
#   return to baseline -- resulting in a spillover effect on downstream
#   processes (like anxiety measurements).

# GSR:
#  Suppose that in contrast GSR has a "rapid" influence on anxiety measurements
#  This could occur if changes in skin conductance are "transient" and quickly 
#     respond to changes in arousal.

# Before we go ahead and implement this model specification in lavaan, let's 
#   first consider how we previously accounted for latent effects on measured anxiety.

# Linear growth curve for the effects of latent anxiety on measured anxiety:
plot(load.image('LGM2.jpeg'), axes = FALSE)
#  This plot expresses that we previously decided to *fix* the parameters 
#     governing this relationship to be linear.

# In contrast, for Cortisol and GSR, we're going to *estimate* the parameters
#  describing their association with measured anxiety. 

# But, how do we incorporate our substantive knowledge with parameter estimation? 

# For Cortisol (spillover effect), we might estimate the continued impact
#   of earlier timepoints by incorporating time-lagged effects. 
# Specifically, we will assume that anxiety measurements at timepoint T are
#   influenced by *all* prior Cortisol measurements (all the way back to T-3).
plot(load.image('LGM5.jpeg'), axes = FALSE)


#### ** INTERACTIVE Pt. 1 **: FILL IN THE BLANKS ####
LGM_mod_itx1 <- '
# Time-varying effects to be estimated for Cortisol:

# At time t0, anxiety is predicted by the concurrent t0 Cortisol
anx_t0 ~ cort_t0

# At t1, we have the t1 effects, as well as the "lingering" t0 ("Lag-1") effects
anx_t1 ~ cort_t0 + cort_t1

# At t2, we have the t2 (concurrent), t1 (Lag-1) & t0 (Lag-2) effects
anx_t2 ~ cort_t0 + cort_t1 + cort_t2

# Finally, at t3 we implement all four possible effects for Cortisol (t0, t1, t2, and t3).
anx_t3 ~ cort_t0 + cort_t1 + cort_t2 + cort_t3
'

# Now, let's fit it and exmaine our fitted path diagram:
LGM_fit_intx1 <- lavaan::growth(LGM_mod_itx1, data = anxiety, estimator = "ML")
semPaths(LGM_fit_intx1, what = "est", edge.label.cex = 1.5, label.cex = 2,
         esize = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 1, mar = c(3, 5, 3.5, 5), colFactor = .3)
# Q: Can it really be THAT easy? Yes ;-)


## ** INTERACTIVE Pt. 1 ** ~ Answer Key ##
LMG_mod_3 <- '
# Time-varying effects to be estimated for Cortisol:

# At time t0, anxiety is predicted by the concurrent t0 Cortisol
anx_t0 ~ cort_t0

# At t1, we have the t1 effects, as well as the "lingering" t0 ("Lag-1") effects
anx_t1 ~ cort_t0 + cort_t1

# At t2, we have the t2 (concurrent), t1 (Lag-1) & t0 (Lag-2) effects
anx_t2 ~ cort_t0 + cort_t1 + cort_t2

# Finally, at t3 we implement all four possible effects for Cortisol (t0, t1, t2, and t3).
anx_t3 ~ cort_t0 + cort_t1 + cort_t2 + cort_t3
'

# Now, let's fit it and exmaine our fitted path diagram:
LGM_fit_3 <- lavaan::growth(LMG_mod_3, data = anxiety, estimator = "ML")
semPaths(LGM_fit_3, what = "est", edge.label.cex = 1.5, label.cex = 2,
         esize = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 1, mar = c(3, 5, 3.5, 5), colFactor = .3)



# Splendid, now let's implement the more "transient" relationship of GSR
#  by only implementing "recent" time-lagged effects. 
# Specifically, we will assume that anxiety measurements at time T are only influenced by the
#  GSR levels at time T and T-1
plot(load.image('LGM6.jpeg'), axes = FALSE)


#### ** INTERACTIVE Pt. 2 **: DELETE THE EXTRAS ####
LGM_mod_itx2 <- '
# Time-varying effects to be estimated for GSR:

# At time t0: anxiety is predicted by concurrent t0 GSR
anx_t0 ~ gsr_t0

# At t1:
anx_t1 ~ gsr_t0 + gsr_t1

# At t2:
anx_t2 ~ gsr_t1 + gsr_t2

# Finally, at t3:
anx_t3 ~ gsr_t2 + gsr_t3
'

# Now, let's fit it and exmaine our fitted path diagram:
LGM_fit_intx2 <- lavaan::growth(LGM_mod_itx2, data = anxiety, estimator = "ML")
semPaths(LGM_fit_intx2, what = "est", edge.label.cex = 1.5, label.cex = 2,
         esize = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 1, mar = c(3, 5, 3.5, 5), colFactor = .3)




## ** INTERACTIVE Pt. 2 ** ~ Answer Key ##
LMG_mod_4 <- '
# Time-varying effects to be estimated for GSR:

# At time t0: anxiety is predicted by the concurrent t0 GSR
anx_t0 ~ gsr_t0

# At t1: we have the t1 effects, as well as the "lingering" t0 ("Lag-1") effect
anx_t1 ~ gsr_t0 + gsr_t1

# At t2: we have the t2 effect as well as the "lingering" t1 (Lag-1) effect
anx_t2 ~ gsr_t1 + gsr_t2

# Finally, at t3: we have the t3 effect as well as the "lingering" t2 (Lag-1) effect
anx_t3 ~ gsr_t2 + gsr_t3
'

# Now, let's fit it and exmaine our fitted path diagram:
LGM_fit_4 <- lavaan::growth(LMG_mod_4, data = anxiety, estimator = "ML")
semPaths(LGM_fit_4, what = "est", edge.label.cex = 1.5, label.cex = 2,
         esize = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 1, mar = c(3, 5, 3.5, 5), colFactor = .3)



# Awesome! We have now implemented all the pieces of our model! 
# Quick Recap: 
# 1) We have an exogenous time-invariant covariate (Group) influencing 
#  the intercept and shape parameters of our latent anxiety varible;
# 2) We also have a linear growth curve for the latent anxiety process's 
#   effect on measured anxiety over time
# 3/4) We also estimate concurrent and time-lagged effects for two measured time-varying 
#   covariates -- Cortisol and GSR -- with differing relationships to measured anxiety.
plot(load.image('LGM7.jpeg'), axes = FALSE)

# WOOH -- now this is an *EXTREMELY* complex and "theoretically" sound model
# What I hope you realize from this exercise is that even though our final model
#   is incredibly complex, implementing the model piece-wise was actually pretty simple

# Don't worry -- I'm not going to ask you to implement this; but really, all you need to do
#   to implement this model is to combine the pieces we've already implemented


#### Final Model ####
LMG_finmod <- '
  # Linear growth curve specification for latent anxiety and measured anxiety
  anx_i =~ 1*anx_t0 + 1*anx_t1 + 1*anx_t2 + 1*anx_t3
  anx_s =~ 0*anx_t0 + 1*anx_t1 + 2*anx_t2 + 3*anx_t3
  
  # Exogenous time-invariant covariate specification for group
  anx_i ~ grp
  anx_s ~ grp
  
  # Time-varying effects to be estimated for cortisol and GSR:
  
  # At time t0, anxiety is predicted by the concurrent t0 cortisol and GSR
  anx_t0 ~ cort_t0 + gsr_t0
  
  # At t1, we have the t1 effects for each, as well as the "lingering" t0
  #  ("Lag-1") effects for each
  anx_t1 ~ cort_t0 + cort_t1 + gsr_t0 + gsr_t1
  
  # At t2, the t0 ("Lag-2") effect for GSR has "worn off", but not for cortisol.
  #  Then, we also have the t2 (concurrent) and t1 (Lag-1) effects for each
  anx_t2 ~ cort_t0 + cort_t1 + cort_t2 + gsr_t1 + gsr_t2
  
  # Finally, at t3, we have only t3 and t2 effects for GSR, but we have
  #  all four possible effects for cortisol (t0, t1, t2, and t3).
  anx_t3 ~ cort_t0 + cort_t1 + cort_t2 + cort_t3 + gsr_t2 + gsr_t3
' 


#### Model fit and visualization ####
# Okay, we're finally ready to fit our latent growth model!
LGM_finfit <- lavaan::growth(LMG_finmod, data = anxiety, estimator = "ML")

# Here's our fitted path diagram -- look familiar?
semPaths(LGM_finfit, what = "est", edge.label.cex = 1.5, label.cex = 2,
         esize = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 1, mar = c(3, 5, 3.5, 5), colFactor = .3)

# NOW (under a confirmatory analysis) -- we examine the goodness-of-fit measures
fm <- fitmeasures(LGM_finfit)
fm[c('cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')]  
# What are we looking for here?
# Comparative Fit Index (CFI) > 0.95 
# Root Mean Square Error of Approximation (rmsea) < 0.07
# Standardized Root Mean Square Residual (srmr) < 0.08

# Overall then the model is...eh?


# Finally, we can review the summary output for the fitted estimates
LGM_summary <- summary(LGM_finfit, fit.measures = TRUE, standardized = TRUE)

# I won't go through and interpret each individual estimate here
#  because these are simulated data (no gaurentee the output will make sense).

# However, I WILL show you how to isolate a few key parameters from the
#  summary object, so you can interpret the estimates for your data

# NOTE: The summary object has a "pe" dataframe, for "parameter estimates"
head(LGM_summary$pe)
#LGM_summary$pe

# Below I pull out row numbers associated with key effects.

# Here are the estimated effects of group on latent anxiety:
LGM_summary$pe[9:10, ] %>% mutate(across(!1:3, round, 2))

# Here are the time t0 effects of cortisol and GSR on measured anxiety
LGM_summary$pe[11:12, ] %>% mutate(across(!1:3, round, 2))

# Time-varying t1 effects
LGM_summary$pe[13:16, ] %>% mutate(across(!1:3, round, 2))

# Time-varying t2 effects
LGM_summary$pe[17:21, ] %>% mutate(across(!1:3, round, 2))

# Time-varying t3 effects
LGM_summary$pe[22:27, ] %>% mutate(across(!1:3, round, 2))
