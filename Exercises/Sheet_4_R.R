##### Sheet 4 ##############################################################
############################################################################
##### Exercise 2 ###########################################################
############################################################################
# a)
library(nlme)
load("Europe.RData")
Europe <-  groupedData(log(Confirmed) ~ Day | Country, data = Europe)
plot(Europe)

# b) 
modelEU <- lme(log(Confirmed) ~ Day, data = Europe)
lme(log(Confirmed) ~ Day, random =~ Day | Country, data = Europe) 
# same fit due to grouped data structure defined before
# The random effects variance is estimated using the default REML, which corresponds to empirical Bayes.

# c) 
countrylist <- unique(Europe$Country)
for (thisc in countrylist){
  thiscountry <- Europe[Europe$Country == thisc,]
  plot(log(Confirmed) ~ Day, data = thiscountry, pch = 20, main = thisc, # plot data for this country
       xlim = range(Europe$Day), ylim = range(log(Europe$Confirmed)))
  if (thisc != "Greece"){   # Greece has only one data point, no linear model fit possible 
    countrymodel <- lm(log(Confirmed) ~ Day, data = thiscountry)
    abline(countrymodel)          # plot trend based on a linear model for just this country
  }
  LMMcoefscountry <- coef(modelEU)[thisc,] # returns sum of fixed and random effects for thisc
  abline(a = LMMcoefscountry[1, 1], b = LMMcoefscountry[1, 2], col = 2) # plot trend based on the LMM
  abline(modelEU$coef$fixed, col = 3)                                 # plot overall EU trend from the LMM
  legend(lty=1, col = 1:3, x = "bottomright", bty = "n",
         legend = c('country trend linear model', 'country trend LMM', 'EU trend LMM'))
}
# Not a lot of shrinkage, as country trends based on linear model and LMM are very similar.
# Variance for the random intercept is large compared to the residual variance and shrinkage thus small.
# Variance for the random slope is smaller, but not when multiplying by (the square of) several days. 
# Effects of shrinkage thus mostly just visible for countries with few observations (e.g. Finland), 
#    where the prior becomes more important.
# LMM most beneficial to stabilize estimation for countries with few observations, 
#    e.g. Greece (n = 1, no LM possible), Finland or Portugal. 
# Besides some outliers (e.g. Bulgaria), slopes relatively similar across countries (mean 0.25, sd 0.06).
log(2) / modelEU$coef$fixed[2] # numbers take on average 2.75 days in Europe to double.

# d) 
newdata <- data.frame(Day = 32, Country = "Germany")
exp(predict(modelEU, newdata)) # Alternative: exp(LMMcoefsGermany[1,1] + 32 * LMMcoefsGermany[1,2]) 
# Almost 600,000 cases predicted. Since Germany implemented measures on Day 14 to
# stop the exponential spread, it is not surprising that the predicted value is
# much larger than the observed value of approximately 67,000. However, the linear 
# (mixed) model might overpredict the number of cases when extrapolating, compare
# Exercise 3 on Sheet 1 and does not account for the count structure.

Germany <- Europe[Europe$Country == 'Germany',]
countrymodel <- lm(log(Confirmed) ~ Day, data = Germany)
exp(predict(countrymodel, newdata)) # similar prediction of 603,000 cases using linear model for Germany

library(lme4)
modelEU2 <- glmer(Confirmed ~ Day | Country, data = Europe, family = poisson) # Poisson mixed model
predict(modelEU2, newdata, type = "response") # Now prediction of about 440,000 confirmed cases.

countrymodel2 <- glm(Confirmed ~ Day, data = Germany, family = poisson)
predict(countrymodel2, newdata, type = "response") # similar prediction of 440,000 cases using Poisson model for Germany




##### Sheet 4 ##############################################################
############################################################################
##### Exercise 3 ###########################################################
############################################################################
# a) 
library(lars)
library(corrplot)
data(diabetes)
head(diabetes)
summary(diabetes$y)
hist(diabetes$y)

m0 <- lm(y ~ x, data = diabetes)
summary(m0)         # high effect estimates (note that columns in x are standardized!)
coefcov <- vcov(m0) # covariance matrix of effect estimates
sqrt(diag(coefcov)) # very standard errors (note that columns in x are standardized!)
coefcov       # very high covariances
corrplot(cov2cor(coefcov)) # some correlations near 1 or -1
# correlation for x
cor(diabetes$x)     # partly very high correlations, e.g. ldl and tc have correlation 0.9.
corrplot(cor(diabetes$x))

m1 <- lm(y ~ x2, data = diabetes)
summary(m1)         # only sex, bmi, map significant, many very high effect estimates and standard errors
hist(cor(diabetes$x2)) # some correlations very close to 1 or -1

2^10
2^64 
# Best subset regression would have to look at 1024 respectively 1.8 * 10^19 models!

# b) 
n <- nrow(diabetes)
set.seed(123)
valinds <- sample(1:n, size = 0.2 * n)
validation <- diabetes[valinds, ]
fitting <- diabetes[-valinds, ]

yval <- validation$y
nval <- length(yval)
prederror<- function(mymodel){
  predval <- predict(mymodel, validation)
  as.numeric(sqrt(crossprod(yval - predval) / nval))
}
m0 <- lm(y ~ x, data = fitting)
prederror(m0)
m1 <- lm(y ~ x2, data = fitting)
prederror(m1)
# The model without interaction leads to better prediction error out-of-sample

prederrors <- c(prederror(m0), prederror(m1))

# c) 
library("penalized")
# ridge regression model for x
m2 <- optL2(y, ~ x, standardize = TRUE, data = fitting, model = "linear", fold = 10)
# use predict function for refitted model with optimal lambda
predval <- predict(penalized(fitting$y, fitting$x, standardize = TRUE, 
                             model = "linear", lambda2 = m2$lambda), # fit model with optimal lambda
                   penalized = validation$x)[, 1] # compute predictions for validation data
prederrors <- c(prederrors, sqrt(as.numeric(crossprod(yval - predval) / nval)))
# ridge regression model for x2
m3 <- optL2(y, ~ x2, standardize = TRUE, data = fitting, model="linear", fold = 10)
predval <- predict(penalized(fitting$y, fitting$x2, standardize = TRUE, 
                             model = "linear", lambda2 = m3$lambda), # fit model with optimal lambda
                   penalized = validation$x2)[,1] # compute predictions for validation data
prederrors <- c(prederrors, sqrt(as.numeric(crossprod(yval - predval) / nval)))
# lasso regression model for x
m4 <- optL1(y, ~ x, standardize = TRUE, data = fitting, model = "linear", fold = 10)
predval <- predict(penalized(fitting$y, fitting$x, standardize = TRUE, 
                             model = "linear", lambda1 = m4$lambda), # fit model with optimal lambda
                   penalized = validation$x)[,1] # compute predictions for validation data
prederrors <- c(prederrors, sqrt(as.numeric(crossprod(yval - predval) / nval)))
# lasso regression model for x2
m5 <- optL1(y, ~ x2, standardize = TRUE, data = fitting, model = "linear", fold = 10)
predval <- predict(penalized(fitting$y, fitting$x2, standardize = TRUE, 
                             model = "linear", lambda1 = m5$lambda), # fit model with optimal lambda
                   penalized = validation$x2)[,1] # compute predictions for validation data
prederrors <- c(prederrors, sqrt(as.numeric(crossprod(yval - predval) / nval)))

### which of the methods has the minimum prediction error and what are the errors? ###
mymethods <-c("LS with x", "LS with x2", 
              "Ridge with x", "Ridge with x2", "Lasso with x", "Lasso with x2")
mymethods[which.min(prederrors)]
names(prederrors) <- mymethods
prederrors  
# For both least squares and the Ridge, adding interactions increases prediction 
#    error, although the Ridge outperforms simple linear regression.
# For the Lasso due to the model selection property, adding interactions improves 
#    performance, but even without interactions, the Lasso outperforms the other 
#    methods in terms of prediction error out-of-sample.
plot(factor(1:6, labels = mymethods), prederrors)

### plot predicted values and path for best method ###
plot(yval, predval)  
abline(a=0, b=1)
# predictions reasonable agreement with observed y values

coef(m5$fullfit)  
# 15 out of 64 variables/interactions selected, coefficients more reasonably 
# sized than without L1 penalty
pen <- penalized(fitting$y, fitting$x2, standardize = TRUE, model = "linear", 
                 lambda1 = 100, steps = 50)
plotpath(pen)
abline(v = m5$lambda, lty = 2)