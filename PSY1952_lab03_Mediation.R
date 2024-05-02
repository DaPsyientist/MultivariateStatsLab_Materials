# PSY1952_lab03_Mediation.R
# Juan Castillo - February 17/20, 2023
# jcastillo@g.harvard.edu

##------------------------------------------------##
## PSY1952: Multivariate Analysis in Psychology   ##
## Lab 03:  Mediation                             ##
##------------------------------------------------##

# Today we're going to discuss mediation analysis with an emphasis on conceptual/theoretical 
#   perspectives over implementation (covered thoroughly in lecture).

# We will explore important decisions around when and why a mediation model
#  is appropriate compared to a standard regression setup.
# **SPOILER: Hypothesis or theory-driven mediation is always warranted **

# But first, a (flawed) simile from Chat-GPT:
# "Mediation is like the statistical equivalent of couples therapy, 
#   helping two variables work out their differences and find a common ground!"

## Packages ----
require(pacman)
p_load(rstudioapi, tidyverse, sjPlot, gtools, effectsize, semPlot, lavaan)

## Data ----
# The data we use today includes information about (per-capita) happiness ratings and 
#   alcohol consumption around the world in 2016 -- see link below:
browseURL("https://www.kaggle.com/marcospessotto/happiness-and-alcohol-consumption")

# Continuing with the method from last week I will now point my working directory to 
#   wherever this RScript (and the data) resides
current_path <- getActiveDocumentContext()$path    ## gets path for this R script
setwd(dirname(current_path)) ## sets working directory to folder this R script is in ("Unit05Model1.rda" & "Unit05Model2.rda"should also be there)

getwd() # and let's check where our current path is

# Load Data (If you haven't yet, download the file to wherever this RScript lives)
happiness_alc <- read.csv('L3_HappiAlc.csv', stringsAsFactors = TRUE)

# Clean data ~ I will remove the higher-GDP countries to make our analysis cleaner,
happiness_alc <- happiness_alc %>% filter(GDP_PerCapita < 250)

# Overview data
head(happiness_alc)
dim(happiness_alc)
str(happiness_alc)

# Again for simplicity, we're just going to work with three variables here:
happiness_alc$GDP_PerCapita   ## Per capita GDP
happiness_alc$HappinessScore  ## Happiness score
happiness_alc$Beer_PerCapita  ## and Beer consumption per capita (in liters)

# We're going to explore several theories about how these three variables might be related,
#  and discuss what each model setup would imply about our theory.


#### Theory 1: Happiness and GDP predict beer consumption ####
# This is the simplest theory we're going to test.

# Let's start with descriptive visualizations:
ggplot(happiness_alc, aes(x = HappinessScore, y = Beer_PerCapita)) + geom_point() + ggtitle('Beer Consumption by Happiness Score')
ggplot(happiness_alc, aes(x = GDP_PerCapita, y = Beer_PerCapita)) + geom_point() + ggtitle('Beer Consumption by GDP')

# Q: What is the implied model structure here?
# Hint: What variable do both of these visualizations have in common?
# -------------------------------------------------------- #



# Our first model describes the classic multiple regression setup. 
# We assume happiness and GDP (independently) predict beer consumption, and therefore 
#   model this association with additive predictors in a regression.

# For completeness, let's develop the model in a step-wise fashion.

# beer ~ happiness****
summary(lm(Beer_PerCapita ~ HappinessScore, data = happiness_alc))

# beer ~ GDP****
summary(lm(Beer_PerCapita ~ GDP_PerCapita, data = happiness_alc))

# Both individual regressions indicate significant positive associations
#   suggesting happiness and GDP may explain shared variance in beer consumption. 

# What we will examine now is whether the individual effects persist after conditioning 
#   the predictors on one another.

# Now for the multiple regression model:
Th1_lm <- lm(Beer_PerCapita ~ HappinessScore + GDP_PerCapita, data = happiness_alc)
summary(Th1_lm)
standardize_parameters(Th1_lm) #Handy function to calculate standardized Beta coefficients

# Effects plots
plot_model(Th1_lm, type = 'pred')
plot_model(Th1_lm, type = 'pred', terms = c('HappinessScore', 'GDP_PerCapita'))
plot_model(Th1_lm, type = 'pred', terms = c('GDP_PerCapita', 'HappinessScore'))

# Fantastic -- we can conclude that beer consumption is significantly predicted by
#  happiness levels after controlling for GDP, but beer consumption is NOT
#  significantly predicted by GDP after controlling for happiness levels.

# So intuitively we see from this example that there is a finite amount of variance
#   in the data which can be explained by our predictors, and therefore including
#   different predictors can affect what terms are significant (and therefore our conclusions)

# Q: Summarize the importance of mediation given the insight above:
# -------------------------------------------------------- #


# Mediation is critical because it helps us understand the underlying processes linking
#   variables together (inference)


#### Theory 2: Happiness predicts beer consumption differently by GDP ####
# Now we're going to be more specific about our theory.
#  Namely, we'll specify we believe the effect of happiness on beer consumption varies by GDP.

# Let's visualize the data to evaluate this theory:
ggplot(happiness_alc, aes(x = HappinessScore, y = Beer_PerCapita, color = GDP_PerCapita))+ geom_point()  +scale_color_gradient(low="black", high="red") 

# Still difficult to see -- let's try faceting by GDP quantiles
ggplot(happiness_alc %>% mutate(GDP_PerCapita_qtl = quantcut(happiness_alc$GDP_PerCapita)),  ## this mutate() call is creating a quantile variable for GDP
       aes(x = HappinessScore, y = Beer_PerCapita)) + 
   geom_point() + geom_smooth(method = 'lm') +  facet_wrap(~GDP_PerCapita_qtl)

# Now we can see the association between happiness and beer consumption varies at different GDP levels.
# Thus, the effect of happiness on beer consumption (our DV) is MODERATED by a countries GDP level.

# **Q: What change does this imply for our model structure?
# -------------------------------------------------------- #



# Therefore our second model is a multiple regression setup with an interaction term between
#   our predictors: GDP level and happiness.

# So we add an interaction!
Th2_lm <- lm(Beer_PerCapita ~ HappinessScore*GDP_PerCapita, data = happiness_alc)
summary(Th2_lm)
standardize_parameters(Th2_lm)
plot_model(Th2_lm, type = 'pred', terms = c('HappinessScore', 'GDP_PerCapita')) #Great way to visualize interactions

# The interaction term is actually not significant here. So even though
#  visually we identified different slopes in our descriptive visualization plot, 
#  the confidence intervals around each effect level are too broad (and thus overlapping) 
#  to consider these differences significant -- we return to theory I...or do we?

# Discuss: Goals of Modeling; Parsimony (Exploratory) v. Confirmatory



# Now, how does this interaction model relate to a moderation model?
#  Statistically - the same.
#  Theoretically - slightly different (due to language).
#  ~Welcome to the Danger Zone~

# The reason is the following:
#  Our research question in Theory 2 was effectively:
#  "Does *happiness level* predict beer consumption differently by *GDP*?"
#  And our results indicate the answer is "no".

# In an interaction model, the model simultaneously tests the complementary hypothesis:
#  "does *GDP* predict beer consumption differently by *happiness level*?"
plot_model(Th2_lm, type = 'pred', terms = c('GDP_PerCapita', 'HappinessScore'))

# In moderation, we usually phrase this in only *one* way depending on our 
#  underlying theory. If we're most interested in the association between happiness 
#  and beer consumption we would say we're investigating whether GDP moderates the 
#  influence of happiness on beer consumption (here we would declare it does not).

# A proper path diagram illustrates this directionality -- as seen in the path diagram below:
semPlot::semPaths(Th2_lm, intercepts = FALSE)


# Takeaway: Moderation is equivalent to an interaction, but the language people 
#   to use to describe moderation analyses implies a stronger underlying theory.


#### Theory 3: GDP influences beer consumption indirectly via its influence on happiness ####
# Now our theory is becoming more complex. There are several claims embedded in this theory.
#  The big ones are:
#  - GDP influences beer consumption
#  - GDP influences happiness levels
#  - Happiness levels influence beer consumption

#** But there's also a subtly hidden claim here, let's draw the diagram and see if 
#   we can locate it


# Answer: The implicit assumption is that Happiness does NOT influence GDP

# Now that we're in mediation territory, let's fit the model:
Th3_med_spec <- '
   HappinessScore ~ a*GDP_PerCapita
   Beer_PerCapita ~ c*GDP_PerCapita + b*HappinessScore
   ind := a*b
   tot := ind+c
   prop := ind/tot'
Th3_med_lav <- lavaan::sem(Th3_med_spec, data = happiness_alc %>% mutate(across(where(is.numeric), scale)))  ## standardizing variables
semPaths(Th3_med_lav) #Path diagram makes the hypothesized relationships explicit

# If we assume that variable structure the estimated effects are:
semPaths(Th3_med_lav, what = 'est', intercepts = FALSE)
summary(Th3_med_lav)

Th3_med_summary <- summary(Th3_med_lav)$pe  ## we're saving the parameter estimate table from the summary to use later

# Let's interpret these results in stages.

# First of all, the direct effect:
#  The descriptive visualization we originally plotted implies a slight positive association
#  between GDP and beer consumption:
ggplot(happiness_alc, aes(x = GDP_PerCapita, y = Beer_PerCapita)) + geom_point() + ggtitle('Beer Consumption by GDP')

# We also saw this in our single regressions during the step-wise model building when evaluating Theory 1.
summary(lm(Beer_PerCapita ~ GDP_PerCapita, data = happiness_alc))

# HOWEVER, under the hypothesis that this relationship is "mediated by" happiness levels,
#  we see that this direct effect in our path model is insignificant.
Th3_med_summary %>% filter(label == 'c') %>% mutate(across(where(is.numeric), round, 2))
# So whats the deal?

# This pattern of results suggests the effect of GDP on beer consumption is
#  "fully mediated" by the effect of GDP on happiness, and happiness' subsequent
#   effect on beer consumption.

# Just as in our Theory 1 model, the mediation model gives us estimates for the
#  effects of beer ~ GDP & beer ~ happiness, fitted in a single model.
#  In fact, if we compare the standardized parameters from the Theory 1 model to the
#  beta estimates (which have already been standardized) for those paths in the Theory 3
#  model, we can see the estimates are nearly identical as well.

# beer ~ GDP, conditioned on happiness
Th3_med_summary %>% filter(label == 'c') %>% mutate(across(where(is.numeric), round, 2))
standardize_parameters(Th1_lm) %>% filter(Parameter == 'GDP_PerCapita')

# beer ~ happiness, conditioned on GDP
Th3_med_summary %>% filter(label == 'b') %>% mutate(across(where(is.numeric), round, 2))
standardize_parameters(Th1_lm) %>% filter(Parameter == 'HappinessScore')

# So how does the mediation model differ?

# It incorporates additional path estimates to evaluate our theory that
#  GDP influences happiness. These estimates show us that *given that GDP
#  influences happiness*, it has a positive effect on happiness, and subsequently 
#  beers per capita
Th3_med_summary %>% filter(label == 'a') %>% mutate(across(where(is.numeric), round, 2))

# Seen nicely in the left green arrow in the estimate version of the path diagram:
semPaths(Th3_med_lav, what = 'est')

# If we plot the effect of GDP on happiness, we clearly see a strong relationship
ggplot(happiness_alc, aes(x = GDP_PerCapita, y = HappinessScore)) + geom_point() + ggtitle('Happiness by GDP')

#Now if we look at beers per capita
Th3_med_summary %>% filter(label == 'b') %>% mutate(across(where(is.numeric), round, 2))

# Subtly in the bottom green arrow in the estimate version of the path diagram:
semPaths(Th3_med_lav, what = 'est')

# If we plot the effect of happiness on beers per capita, we see another strong relationship
ggplot(happiness_alc, aes(x = HappinessScore, y = Beer_PerCapita)) + geom_point() + ggtitle('Happiness by B_P')


# So Theory 3 is particularly "strong" as the association between GDP and happiness 
#   *acts in this direction* with GDP influencing happiness -- which then proceeds to 
#   *act in this direction* of happiness influencing Beers per-capita.

# Our story seems to hang together nicely, right?
#  "More GDP leads to more happiness, which leads to more beer consumption,
#  and thus happiness increases explain any apparent influence of GDP
#  on beer consumption."

# But does the model inform us if that is the *right* story?
# (No.)

# Discuss: Murders, Ice-Cream, and Summertime

# Let's examine one more theory to help understand why (not).
# In Theory 3, we operated under the assumption that GDP influences happiness.
#  But suppose we had a substantive reason to believe that it's the other way around: that happiness influences GDP.

# (Note: this is obviously a bad theory, but the true mediator in our own research may not be so obvious)

#### Theory 4: Happiness influences beer consumption indirectly through it's influence on GDP ####
ggplot(happiness_alc, aes(x = HappinessScore, y = GDP_PerCapita)) + geom_point() + ggtitle('GDP by Happiness')
# **Compelling! now lets draw the diagram

#### Activity (Fill in the blanks): Implement the mediation model diagrammed above: ####
Practice_Mediation  <- '
   $ ~ a*$
   $ ~ c*$ + b*$
   ind := a*b
   tot := ind+$
   prop := ind/$'
Prac_lav <- lavaan::sem(Practice_Mediation, data = happiness_alc %>% mutate(across(where(is.numeric), scale)))  ## standardizing variables

# Evaluate your path diagram
semPaths(Prac_lav)
# ------------------------------------------------#





# Answer Key
Th4_med_spec <- '
   GDP_PerCapita ~ a*HappinessScore
   Beer_PerCapita ~ c*HappinessScore + b*GDP_PerCapita
   ind := a*b
   tot := ind+c
   prop := ind/tot'
Th4_med_lav <- lavaan::sem(Th4_med_spec, data = happiness_alc %>% mutate(across(where(is.numeric), scale)))  ## standardizing variables

# Here is the new path diagram:
semPaths(Th4_med_lav, what = 'est')

# Obviously the major difference is the arrow directions.
#  Happiness is now an exogenous variable, and it influences GDP
#  and beer consumption. GDP also influences beer consumption, BUT
#  *GDP does not influence happiness*.

# Let's look at the estimates for this new model.
summary(Th4_med_lav)
Th4_med_summary <- summary(Th4_med_lav)$PE  ## again save the parameter estimate table from the summary to use later

# At a glance, these numbers seem *extremely* similar to the numbers in the Theory 3 model.

# If we plot the path diagrams next to each other, it becomes obvious what's different:
op <- par(mfrow = c(2, 1))
semPaths(Th3_med_lav, what = 'est')
semPaths(Th4_med_lav, what = 'est')
par(op)

# Although the strength of the estimated relationships are similar, the
#  interpretation has changed completely -- because the underlying theory has changed.

# Zooming in on the Theory 4 path diagram...
semPaths(Th4_med_lav, what = 'est')

# ...we would generate the following story for our results:
#  Happiness influences both beer consumption and GDP, but
#  the influence of happiness is direct; it is not indirectly
#  "mediated" through GDP and GDP's influence on beer consumption.
# Also, GDP is the mediator now.


## Summary ----

# Now, if that last story seems harder to wrap your head around, it's because it
#  doesn't really make as much sense to think about the variables this way. The
#  story in Theory 3 really more logical. That may be because Theory 3
#  is more "substantive" than Theory 4. Theory 4 feels like an intellectual exercise,
#  whereas the idea in Theory 3 that GDP influences happiness makes intuitive sense.

# But it is NOT the *models* that help us identify which story is correct. 
# It is the plausibility and defensibility of the *theory* that led us
#  to construct the particular path model we fit.

# Importantly, we did not do a causal experiment here. We cannot estimate a true causal
#  effect from these data. Remember: just because we fit a mediation model and describe 
#  estimated effects in a narrative form (in line with our theories), doesn't mean the models
#  we fit justify causal inference.

# This is what it means to have a "substantive theory", as Patrick often discusses in class. 
#  if you have such a theory, usually derived from the literature or known axioms/principles in your
#  field, then fitting a path model can provide immense flexibility and insight.
#  However, doing exploratory path modeling is a dicey proposition for the very same reason. That is,
#  the immense flexibility can lead to immensely false insights.
#  When deciding whether to use path models in your own analyses, remember interpreting a narrative 
#  is NOT the same as proof!

