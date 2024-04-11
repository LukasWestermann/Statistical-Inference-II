library(mvtnorm)

Simus <- 5000

# Total MSE! (not component wise MSE)
MSE.TE <- vector(length=Simus, mode="numeric")
MSE.TEstar <- vector(length=Simus, mode="numeric")
MSE.TEtilde <- vector(length=Simus, mode="numeric")

for ( s in 1:Simus){
  
  # simulate data with variance 1 for three components with means 1, 2 and 0.9 (i.e. the second mean is 'more extreme')
  n <- 1
  m <- 3
  mu.true <- c(1,2,0.9)
  x <- rmvnorm(n=n, mean=mu.true, sigma=diag(x=1,nrow=3) )
  
  
  t <- colMeans(x)
  MSE.TE[s] <- crossprod( t - mu.true )[1]
  MSE.TEstar[s] <- crossprod((1- ((m-2)/n)/crossprod(t)[1] ) * t - mu.true)[1]
  MSE.TEtilde[s] <- crossprod(max(0, (1- ((m-2)/n)/crossprod(t)[1] )) * t - mu.true)[1]
}

cat("Empirical MSE of the usual estimator (mean):         ", mean(MSE.TE), "\n")
cat("Empirical MSE of the James-Stein-estimator:          ", mean(MSE.TEstar), "\n")
cat("Empirical MSE of the modified James-Stein-estimator: ", mean(MSE.TEtilde), "\n")


# Componentwise MSE
MSE.TE <- matrix(NA, ncol = m, nrow=Simus)
MSE.TEstar <- matrix(NA, ncol = m, nrow=Simus)
MSE.TEtilde <- matrix(NA, ncol = m, nrow=Simus)

for ( s in 1:Simus){
  
  # simulate data with variance 1 for three components with means 1, 2 and 0.9 (i.e. the second mean is 'more extreme')
  n <- 1
  m <- 3
  mu.true <- c(1,2,0.9)
  x <- rmvnorm(n=n, mean=mu.true, sigma=diag(x=1,nrow=3) )
  
  
  t <- colMeans(x)
  MSE.TE[s,] <- ( t - mu.true )^2
  MSE.TEstar[s,] <- ((1- ((m-2)/n)/crossprod(t)[1] ) * t - mu.true)^2
  MSE.TEtilde[s,] <- (max(0, (1- ((m-2)/n)/crossprod(t)[1] )) * t - mu.true)^2
}

cat("Empirical MSE of the usual estimator (mean):         ", colMeans(MSE.TE), "\n")
cat("Empirical MSE of the James-Stein-estimator:          ", colMeans(MSE.TEstar), "\n")
cat("Empirical MSE of the modified James-Stein-estimator: ", colMeans(MSE.TEtilde), "\n")

cat("Empirical MSE of the usual estimator (mean):         ", sum(colMeans(MSE.TE)), "\n")
cat("Empirical MSE of the James-Stein-estimator:          ", sum(colMeans(MSE.TEstar)), "\n")
cat("Empirical MSE of the modified James-Stein-estimator: ", sum(colMeans(MSE.TEtilde)), "\n")
