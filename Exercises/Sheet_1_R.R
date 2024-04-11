##### Sheet 1 ##############################################################
############################################################################
##### Exercise 3 ###########################################################
############################################################################
# d)
cases_uk <- read.csv("Covid19_UK.csv")
plot(cases_uk)

# construct the design matrix
n <- nrow(cases_uk)
X <- cbind(rep(1, n), 1:n)

# ML-estimate
y <- log(cases_uk$total_cases)
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