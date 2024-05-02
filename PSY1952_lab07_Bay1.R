# Psy1952 -- Spring 2023
# Course Instructor:  Patrick Mair   <mair@fas.harvard.edu>
# Teaching Fellow: Johnny Castillo <jcastillo@g.harvard.edu>

## Lab 7: Probability Theory & Distribution Theory ##

#Load Packages
if (!require("pacman")) {install.packages("pacman"); require("pacman")}
p_load(mvtnorm, rgl, condMVNorm, MASS)
options(rgl.printRglwidget = TRUE)          ## for rgl plotting within RStudio

# "three-quarters of those infected in a massive Massachusetts COVID-19 outbreak, pivotal CDC study finds" 
# The Washington Post: https://twitter.com/washingtonpost/status/1421156015563759621
# However, recall: p(A|B) ≠ p(B|A)
# Specifically, p(vaccinated|COVID) -- which WaPo reported -- isn't equal to p(COVID|vaccine)

# So, what's the actual probability of getting COVID given that you've been vaccinated (with Pfizer)?

#### Conditional probability: Getting COVID, given vaccine #### 
# Source of data: A 2020 article on the Pfizer vaccine: https://www.nejm.org/doi/full/10.1056/nejmoa2034577

# Importing data from article
pfizertable <- matrix(c(8,21712,162,21566), ncol = 2, byrow = F)
colnames(pfizertable) <- c("Pfizer", "placebo")
rownames(pfizertable) <- c("Got COVID", "Healthy")
sum(pfizertable) #43448
pfizertable
# It's our friend the contingency table!

# p(COVID|Pfizer) = p(COVID ∩ Pfizer)/p(Pfizer)
pcovidgpfizer <- (pfizertable[1,1])/sum(pfizertable[,1])
pcovidgpfizer*100     #multiplying by 100 to get the percentage; .0368%

# p(COVID|placebo) = p(COVID ∩ placebo)/p(placebo); .746
pcovidgplacebo <- (pfizertable[1,2])/sum(pfizertable[,2])
pcovidgplacebo*100 #0.75%

# How much more likely is it to get COVID with a placebo vs. with Pfizer?
pcovidgplacebo/pcovidgpfizer #20x Posterior Odds


#### Bayes' theorem: Having COVID, given coughing #### 
# Imagine that you've started coughing! Is it COVID?

# About 67% of people with COVID had symptoms
# Source of data: https://www.acpjournals.org/doi/full/10.7326/M20-6976

# Of the people who have COVID symptoms, only 81% had mild symptoms (i.e., mild coughing)
# Source of data (Feb 2020): https://web.archive.org/web/20200302201644/https://www.cdc.gov/coronavirus/2019-ncov/hcp/clinical-guidance-management-patients.html

# What is the likelihood of only coughing, given that someone has COVID?
# Likelihood: p(coughing|COVID)
# *Here to simplify our calculations we treat them as conditionally independent*
0.67*0.81
# p(coughing|COVID) = 0.5427

# What is the prior?
# Imagine it was March 2021, when there were 30,000 active cases in Massachusetts
# Massachusetts has a population of 7 million.
30000/7000000
#P(COVID) = 0.004

# Now, what's the marginal probability (evidence)?
# Let's say p(coughing) = 0.20. See: https://www.umc.edu/Healthcare/Allergy/Chronic%20Cough.html 

# Now, calculate the posterior probability:
# p(COVID|coughing) = p(coughing|COVID)*p(COVID)/p(coughing)
(0.5427*0.004/0.2)*100  #P(COVID|COUGH) = 1.9% 





#### Estimating the parameters of a distribution ####
## Simulate Data for unconditional normal distribution ##
set.seed(109)
sim <- sort(rnorm(1000, 28, 10))

# Introduction to fitdistr()
# We're going to wstimate the parameters of our simulated data using fitdistr() from the 
#   'MASS' package. The fitdistr() function uses maximum likelihood estimation (MLE) to find the 
#   parameters of the specified distribution which best fit the given data. 
# The output contains the estimated parameters of the fitted distribution (mean and standard 
#     deviation of the normal distribution in the case below).

# Estimate the marginal distribution of the parameters
fitdistr(sim, "normal")

# Plot distribution
hist(sim, freq = FALSE, main = "N(28, 10) Distribution", breaks = 20)     ## Note: we say freq = FALSE (density y-scale)
f_x <- dnorm(sim, mean = 28, sd = 10)      # theoretical density values
lines(sim, f_x, col = "salmon", lwd = 2)    # theoretical distribution (density)



## Simulate Data for Bivariate normal distribution ##
# For this we use the rmvnorm() function from the mvtnorm package. 
# The rmvnorm() function generates random samples from a multivariate normal distribution, 
#   and allows you to specify the mean vector and covariance matrix of the joint distribution.


#-- No covariance --#
# Set the means and covariance matrix of the joint distribution
mu <- c(2, 3)
sigma <- matrix(c(1, 0, 0, 1), nrow = 2) 
sigma

# Generate predictor values
x <- seq(-5, 5, length.out = 100)       
y <- x
z <- matrix(0, nrow = 100, ncol = 100)

## compute density values 
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x[i], y[j]),  mean = mu, sigma = sigma)   ## f(x,y)
  }
}
contour(x, y, z, main = "Bivariate Normal (Contour)", xlab = "X", ylab = "Y")
persp3d(x, y, z, zlab = "f(x,y)", col = "gray")


#-- Strong covariance --# 
sigma2 <- matrix(c(1, .99, .99, 1), ncol = 2)    ## variance-covariance matrix
sigma2
for (i in 1:100) {      
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x[i], y[j]),  mean = mu, sigma = sigma2)   ## f(x,y)
  }
}
contour(x, y, z, main = "Bivariate Normal (Contour)", xlab = "X", ylab = "Y")
persp3d(x, y, z, zlab = "f(x,y)", col = "gray")
# Now that we have seen two potential conditional normal distributions, let's 
#   determine what the parameters for this distribution are


#-- Conditional Probability --# 
# Let's calculate the conditional probability which is easiest to visualize using a contour plot
contour(x, y, z, main = "Bivariate Normal (Contour)", xlab = "X", ylab = "Y")

## Let's create 2 slices: f(Y|X = 1) and f(Y|X = 3.6) --> we slice at 1 and 3.6
abline(v = c(1, 3.6), col = c("salmon", "cadetblue"))   

# Now we draw values from these conditional distributions using rcmvnorm() with arguments:
# Remember: X.given is the conditioning value (here: X = 1 and X = 3.6)
set.seed(123)
y_x1 <- sort(rcmvnorm(10000, mean = mu, sigma = sigma2, dependent.ind = 2, given.ind = 1, X.given = 1))
y_x2 <- sort(rcmvnorm(10000, mean = mu, sigma = sigma2, dependent.ind = 2, given.ind = 1, X.given = 3.6))

# Plot conditional probabilities
op <- par(mfrow = c(2,1))
hist(y_x1, main = "Conditional Density f(Y|X = 1)", xlab = "Y", freq = FALSE, xlim = c(1, 3), col = "salmon")
lines(density(y_x1), lwd = 2)                    
hist(y_x2, main = "Conditional Density f(Y|X = 3.6)", xlab = "Y", freq = FALSE, xlim = c(3, 5), col = "cadetblue")
lines(density(y_x2), col = "red")
