##### Sheet 3 ##############################################################
############################################################################
##### Exercise 1 ###########################################################
############################################################################
# a)
set.seed(1949)
library(mvtnorm)
n <- 50
p <- 20
sigma <- 1
Sigma <- outer(1:p, 1:p, FUN = function(j, l) {(-0.7)^(abs(j - l))})
X <-  rmvnorm(n, sigma = Sigma)
eps <- rnorm(n, mean = 0, sd = sigma)
beta <- (-0.8)^(1:p)
y <- X %*% beta + eps

# b)
# normalize columns of X
X <- apply(X, 2, function(x) (x - mean(x)) / sd(x))
# center y
y <- y - mean(y)
# compute LS estimate for normalized X
betahat <- lm(y ~ X - 1)$coef # Alternative (manually): solve(t(X) %*% X, t(X) %*% y)
# compute ridge estimates
lambdavec <- 10^(-2:2)                    
betaridgefun <- function(lambda) {
  solve(t(X) %*% X + lambda * diag(p), t(X) %*% y)
}
betaridge <- sapply(lambdavec, FUN = "betaridgefun")   # ridge estimate; Alternative: sapply(lambdavec, function(l) penalized::penalized(y, X, lambda2 = l)@penalized)

# plot ridge estimates against LS estimate
plot(betahat, betahat, xlab = "LS estimates", ylab = "ridge estimates") 
abline(a=0, b=1)                                                          
abline(a=0, b=0, lty = 2)                                                 
for (k in 1:length(lambdavec)){
  points(betahat, betaridge[,k], col = k+1)
}
legend('bottomright', legend = lambdavec, col = 1 + 1:length(lambdavec), pch = 1, title = 'lambda')
# Shrinkage effect: estimates tend to be smaller in absolute value than LS. 
# Due to the negative correlations in X, this is not always the case.

# c)
# Compute MSEs
tr <- function(A) {
  sum(diag(A))  # compute the trace of a matrix
}
XtX <- t(X) %*% X                
MSE_LS <- sigma^2 * tr(solve(XtX))
MSE_ridge <- function(lambda) {
  mat1 <- XtX + lambda * diag(p)
  mat2 <- sigma^2 * XtX + lambda^2 * beta %*% t(beta)
  tr(solve(mat1, mat2) %*% solve(mat1))
}
lambdavec <- seq(0, 100, by = 0.2)   
MSE_lambda <- sapply(lambdavec, FUN = "MSE_ridge")

# Plot ridge MSEs depending on lambda
plot(lambdavec, MSE_lambda, xlab = "lambda", ylab = "MSE", type = "l")
lambdavec[which.min(MSE_lambda)] # MSE optimal lambda
abline(v = lambdavec[which.min(MSE_lambda)] , lty = 2)
# Compute optimal ridge estimates
betaridge_opt <- betaridgefun(lambdavec[which.min(MSE_lambda)])
# plot optimal ridge estimates and LS estimates against true coefficients
plot(beta, betahat, ylab ='estimates', xlab = 'true betas', pch = 'x') 
points(beta, betaridgefun(lambdavec[which.min(MSE_lambda)]), col = 2)
abline(a=0, b=1)
legend('bottomright', col = 1:2, pch = c('x','o'), legend = c('LS estimates','MSE optimal ridge estimates'))



##### Sheet 3 ##############################################################
############################################################################
##### Exercise 3 ###########################################################
############################################################################

library('nlme')

# a)
attach(Machines)
?Machines
head(Machines)
plot(as.numeric(as.vector(Worker)), score, pch = as.numeric(Machine), 
     xlab = "Worker", ylab = "score")
legend("bottomleft", legend = c("A", "B", "C"), pch = 1:3, title = "Machine")

# b) 
# Workers can be considered drawn from a population of workers. Their productivity 
# might not be of interest in itself here, but we want to take the repeated measures 
# correlation structure appropriately into account. There are only three machines 
# and they might be of interest in themselves here.

# c) 
m1Machine <- lme(score ~  - 1 + Machine, random =~ 1|Worker, data = Machines) # no intercept to allow one dummy per machine
summary(m1Machine)
fixef(m1Machine)
MachinesB <- Machines[which(Machines$Machine == "B"), ]
mean(MachinesB$score) # equal to fixed effect estimate for Machine B in this simple setting
# estimated average productivity on machines A, B and C is 52.4, 60.3 and 66.3, respectively
ranef(m1Machine)
# estimated average productivity of worker 6 is -8.7 (below average) etc.
ranef(m1Machine)['6',] + fixef(m1Machine)['MachineB']
# expected productivity for worker 6 on machine B would be 51.6
Machines6B <- Machines[which(Machines$Machine == "B" & Machines$Worker == "6"), ]
mean(Machines6B$score) 
# random intercept for worker 6 is shrunk towards zero, expected productivity is thus
# closer to average productivity for machine B (60.3) than simple average over scores
# for worker 6 on machine B (43.6)

# d)
m2Machine <- lm(score ~  - 1 + Machine, data = Machines) # no intercept to allow one dummy per machine
summary(m2Machine)
summary(m1Machine)
# In this simple setting, the fixed effects estimates are the same as in the linear 
# mixed model (not always the case). Standard errors are too small, however, as 
# the model ignores the correlation structure in the data and incorrectly assumes 
# too many independent observations. The model without random intercepts gives 
# unbiased estimates for the fixed effects, but a) estimates are not efficient b) 
# inference is incorrect due to assuming independence of observations on the same 
# worker.

# e)
# code worker effects as deviations from the overall mean (effect or deviation 
# coding) for simple comparison to random effects
contrasts(Machines$Worker) = contr.sum(6)
m3Machine <- lm(score ~  - 1 + Machine + Worker, data = Machines) # no intercept to allow one dummy per machine
summary(m3Machine)
summary(m1Machine)
# In this simple setting, the fixed effects estimates for the machines are the 
# same as in the linear mixed model (not always the case).
Workerbeta  <- coef(m3Machine)[4:8]
Workerbeta  <- c(Workerbeta, "Worker6" = -sum(Workerbeta)) 
Workerbeta
# In this coding, worker intercepts sum to zero, so beta for worker 6 is difference 
# to zero of the sum of the other betas
Workerbeta2 <- coef(m1Machine)[,4]
plot(Workerbeta, Workerbeta2, xlab = "fixed worker intercepts", ylab = "random worker intercepts")
abline(a=0, b=1)
abline(a=0, b=0, lty = 2) 
# small shrinkage effect when worker intercepts are estimated as random effects 
# compared to fixed effects; Shrinkage not strong, as variance for random intercept 
# estimated to be large (about 26 compared to 10 for the error term)

# Pros of modeling worker effects as random effects: fewer parameters, can learn about population of workers, 
#     can estimate effects of covariates that differ between workers (e.g. age, gender), shrinkage can 
#     stabilize estimation especially if there are few observations for some workers.
# Pros of modeling worker effects as fixed effects: no bias or shrinkage effect, can better learn about 
#     individual workers, 5 worker parameters not that many parameters (but a bit more complicated to center
#     worker intercepts around overall mean vs. automatically centered for the random intercepts)

