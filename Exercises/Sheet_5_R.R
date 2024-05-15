##### Sheet 5 ##############################################################
############################################################################
##### Exercise 1 ###########################################################
############################################################################
library(lars)
library(penalized)
data(diabetes)
n <- nrow(diabetes$x)
p <- ncol(diabetes$x)
coeff_names <- c("(Int)", colnames(diabetes$x))

# a) 
# LS fit and beta estimate
m0 <- lm(y ~ x, data = diabetes) 
beta_ls <- coef(m0) 
sd_ls <- summary(m0)$coef[, 2]
lower_ls <- beta_ls - 1.96 * sd_ls # compute confidence limits for CIs
upper_ls <- beta_ls + 1.96 * sd_ls

# Plot the estimated coefficients for intercept and 10 variables, plus CIs 
my_lims <- range(lower_ls, upper_ls) # make sure plot doesn't cut off CIs
plot(1:(p + 1), beta_ls, ylim = my_lims, xlim = c(1, p + 1.5), ylab = "estimates",
     xlab = "variables",  pch = 20, xaxt = "n")
axis(side = 1, at = 1:(p+1), labels = coeff_names) # plot variable names on x-axis
arrows(x0 = 1:(p + 1), y0 = lower_ls, y1 = upper_ls, angle = 90, length = 0.04, code = 3) # add CIs for LS
abline(h = 0, lty = 2, col = 3)

# b)
# Ridge regression fit and beta estimate using penalized package
set.seed(123)
fit_ridge <- optL2(y, ~ x, standardize = TRUE, data = diabetes, model = "linear", 
                   fold = 10)
m2 <- penalized(diabetes$y, diabetes$x, standardize = TRUE, model = "linear", 
                lambda2 = fit_ridge$lambda)
beta_ridge <- coef(m2)
# Alternative: Compute beta_ridge estimate manually (using notation of slide 48)
X <- matrix(1, n, 1) # design matrix for unpenalized, i.e., fixed effects (intercept only)
Z <- diabetes$x      # design matrix for penalized, i.e., random effects (all covariates)
C <- cbind(X, Z)     # overall design matrix
lambda <- fit_ridge$lambda / n # Use the hint
sigB <- diag(c(0, rep(lambda, p))) # intercept is unpenalized
beta_ridge_manually <- solve(t(C) %*% C + sigB, t(C) %*% diabetes$y)
summary(beta_ridge - beta_ridge_manually) # double-check approximate equality to penalized function 

# compute CIs 
sigma2 <- var(residuals(m2))
cov_ridge <- sigma2 * solve(t(C) %*% C + sigB) # using lambda = sigma^2 / tau^2
sd_ridge <- sqrt(diag(cov_ridge))
lower_ridge <- beta_ridge - 1.96 * sd_ridge
upper_ridge <- beta_ridge + 1.96 * sd_ridge
# plot ridge estimates and CIs 
points(0.1 + 1:(p + 1), beta_ridge, pch = 17, col = 2)
arrows(x0 = 0.1 + 1:(p + 1), y0 = lower_ridge, y1 = upper_ridge, angle = 90, 
       col = 2, length = 0.04, code = 3) # add CIs for LS

# c)
# Lasso fit and beta estimate
fit_lasso <- optL1(y, ~ x, standardize = TRUE, data = diabetes, model = "linear", 
                   fold = 10)
m1 <- penalized(diabetes$y, diabetes$x, standardize = TRUE, model = "linear", 
                lambda1 = fit_lasso$lambda)
beta_lasso <- coef(m1)
beta_lasso_0 <- rep(0, (p + 1)) # extend estimate by zeros for not selected variables
names(beta_lasso_0) <- c("(Intercept)", colnames(diabetes$x))
beta_lasso_0[names(beta_lasso)] <- beta_lasso
points(0.2 + 1:(p+1), beta_lasso_0, pch=18, col=4) # plot lasso estimates

# Lasso confidence intervals
library(selectiveInference)
CIs_lasso <- fixedLassoInf(x = diabetes$x, y = diabetes$y, beta = beta_lasso_0[-1],
                           lambda = fit_lasso$lambda / n)
selected_lasso <- CIs_lasso$vars
beta0 <- CIs_lasso$coef0
CIs_lasso <- CIs_lasso$ci
points(1.3 + selected_lasso, beta0, pch = 18, col = 6)
arrows(1.3 + selected_lasso, CIs_lasso[, 1], y1 = CIs_lasso[, 2], angle = 90, 
       col = 6, length = 0.04, code = 3) # add CIs for LS

# naive confidence intervals on active set
m3 <- lm(y ~ x[, selected_lasso], data = diabetes) 
beta_ls0 <- coef(m3)[-1] 
sd_ls0 <- summary(m3)$coef[-1,2]
lower_ls0 <- beta_ls0 - 1.96 * sd_ls0 # compute confidence limits for CIs
upper_ls0 <- beta_ls0 + 1.96 * sd_ls0
points(1.4 + selected_lasso, beta_ls0, pch=18, col=5)
arrows(1.4 + selected_lasso, lower_ls0, y1 = upper_ls0, angle = 90, length = 0.04, code=3, col=5) # add CIs for LS

legend("bottomright", col=c(1,2,4,6,5), pch=c(20,17,18,18,18), 
       legend=c("LS","Ridge","Lasso", "Lasso sel. CI", "Lasso naive CI"))