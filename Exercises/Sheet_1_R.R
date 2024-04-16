##### Sheet 1 ##############################################################
############################################################################
##### Exercise 3 ###########################################################
############################################################################


cases_uk = read.csv("Covid19_Uk.csv")
head(cases_uk)
summary(cases_uk)
plot(cases_uk)

# d)
# construct the design matrix
n = nrow(cases_uk)
X = cbind(rep(1,n), 1:n)

# ML-estimate (same as OLS)
y = log(cases_uk$total_cases)
beta_ML  = solve(t(X) %*% X) %*% t(X)
beta_ML

#Via numerical optimisation:

# Function to calculate l(beta)
X = cbind(rep(1,n), 1:n)
y <- log(cases_uk$total_cases)

#log-likelihood to be optmised
l_beta <- function(beta) {           # beta is a 1x2 vector of parameters
  ll<-t(y-X%*%beta) %*% (y-X%*%beta) #proportional to actual log likelihood up to a constant
  return(ll)
}

max_beta = optim(c(1,1),l_beta,method = "BFGS")
max_beta

#OLS version
beta_ML <- solve(t(X) %*% X) %*% t(X) %*% y
beta_ML

# compare to build-in function
lm_covid <- lm(log(total_cases) ~ day, data = cases_uk)
lm_covid$coefficients

############################################################################
# e)
glm_covid <- glm(total_cases ~ day, family = poisson(link = "log"), data = cases_uk)
glm_covid$coefficients

############################################################################
# f)
lines(exp(predict(lm_covid)), col = "blue")
lines(predict(glm_covid, type = "response"), col = "red")

# prediction after 60 days
exp(predict(lm_covid, newdata = data.frame(day = 60)))
predict(glm_covid, newdata = data.frame(day = 60), type = "response")