### Sheet 9, Exercise 1
#################################################################################################
### c)
find_limit <- function(P){
  A <- rbind(t(diag(3)-P), 1)
  b <- c(rep(0, nrow(P)),1)
  solve(t(A)%*%A)%*%t(A)%*%b
}

#compare to the solution in a)
P <- matrix(c(0.6, 0.1, 0, 0.4, 0.6, 0.2, 0, 0.3, 0.8), nrow = 3)
find_limit(P)
c(1/11, 4/11, 6/11)
#################################################################################################
### Exercise 2
#################################################################################################
### d)

simulate_path <- function(q, start_value, n){
  P <- matrix(c(1-4*q/3, q, q/2, q/3, 1-2*q, q/6, q, q, 1-2*q/3), nrow = 3)
  pi <- c(start_value, numeric(n))
  for(i in 1:n){
    pi[i+1] <- sample(1:3, size = 1, prob = P[pi[i],])
  }
  pi
}

plot(simulate_path(0.1, 1, 100), type = "l")
plot(simulate_path(0.2, 1, 100), type = "l")
plot(simulate_path(0.4, 1, 100), type = "l")

hist(simulate_path(0.1, 2, 100))
hist(simulate_path(0.4, 2, 100))


### Sheet 9, Exercise 3
library(fitdistrplus)
library(ggplot2)
data("danishuni")
hist(danishuni$Loss, breaks = 100)
#################################################################################################
### a)
log_posterior <- function(a,b){
  sum(dgamma(danishuni$Loss, shape = a, rate = b, log = TRUE)) -
    a/1000 - b/1000
}

a <- seq(0.1, 1.7, by = 0.05)
b <- seq(0.1, 0.6, by = 0.05)
z <- sapply(a, function(a){
  sapply(b, function(b) log_posterior(a,b))
})

filled.contour(x = a, y = b, t(z))
#################################################################################################
### b)
log_alpha <- function(a_new, b_new, a_old, b_old){
  if(a_new > 0 & b_new > 0){
    value <- min(log_posterior(a_new, b_new) - log_posterior(a_old, b_old), 0)# log_alpha (i.e. the log of the acceptance prob. of a_new, b_new) gets positive only when log_posterior(a_new, b_new) > log_posterior(a_old, b_old)
  }
  else value <- -Inf
  return(value)
}

metropolis <- function(start_param, sigma = 1, n_path = 100){
  a <- start_param[1]
  b <- start_param[2]
  n_accept <- 0
  for(i in 1:n_path){
    a_new <- a[1] + rnorm(1, 0, sigma)
    b_new <- b[1] + rnorm(1, 0, sigma)
    alpha <- exp(log_alpha(a_new, b_new, a[1], b[1]))
    if(sample(c(TRUE, FALSE), size = 1, prob = c(alpha, 1-alpha))){
      a <- cbind(a_new, a)#the new position of the MC  is appended at the vector beginning via cbind(a_new, a). At the end a reversion takes place, so that the best estimate is at the end of the chain.
      b <- cbind(b_new, b)
      n_accept <- n_accept + 1
    } else {
      a <- cbind(a[1], a)
      b <- cbind(b[1], b)
    }
  }
  print(paste("acceptance rate: ", n_accept/n_path))
  return(data.frame(a = rev(a), b = rev(b)))
}

#################################################################################################
### c)
# starting value (1.4, 0.4) from plot
path_data <- metropolis(start_param = c(1.4, 0.4), sigma = 1)
path_data <- metropolis(start_param = c(1.4, 0.4), sigma = 0.1)
path_data <- metropolis(start_param = c(1.4, 0.4), sigma = 0.01)
path_data <- metropolis(start_param = c(1,1), sigma = 0.05, n_path = 1000)
path_data <- metropolis(start_param = c(1,1), sigma = 0.03, n_path = 1000)

#################################################################################################
### d)
reduction_factor <- function(path_list){
  n <- nrow(path_list[[1]]) - 1
  n_half <- round(n/2)
  first_halfs_list <- lapply(path_list, function(path) path[1:n_half,])
  second_halfs_list <- lapply(path_list, function(path) path[n_half:(n + 1),] )
  halfs_list <- c(first_halfs_list, second_halfs_list)
  W <- rowMeans(sapply(halfs_list, function(path) diag(cov(path[,c(1,2)]))))
  B <- apply(sapply(halfs_list, function(path) colMeans(path[,c(1,2)])), 1, var)
  var_hat <- (n-1)/n*W + B
  R_hat <- sqrt(var_hat/W)
  R_hat
}

# discard first half of values
path_list_1 <- lapply(1:10, function(i){
  path <- metropolis(start_param = c(runif(2, min = c(0.5, 0.2), max = c(2, 1))), sigma = 0.03)
  cbind(path, id = i)
})

path_list_1_tails <- lapply(path_list_1, function(path) path[round(nrow(path)/2):nrow(path),])

reduc_fac_1 <- reduction_factor(path_list_1_tails)
reduc_fac_1

# plot the 10 different paths
path_data_1 <- do.call("rbind", path_list_1)
ggplot(data = path_data_1, aes(x = a, y = b, group = id)) + 
  geom_path(alpha = 0.4, linewidth = 1)
path_data_1_tails <- do.call("rbind", path_list_1_tails)
ggplot(data = path_data_1_tails, aes(x = a, y = b, group = id)) + 
  geom_path(alpha = 0.4, linewidth = 1)
####################################
# rerun the algorithm with n = 1000
path_list_2 <- lapply(1:10, function(i){
  path <- metropolis(start_param = c(runif(2, min = c(0.5, 0.2), max = c(2, 1))), 
                     sigma = 0.03, n_path = 1000)
  cbind(path, id = i)
})

# discard first half of values
path_list_2_tails <- lapply(path_list_2, function(path) path[round(nrow(path)/2):nrow(path),])
reduc_fac_2 <- reduction_factor(path_list_2_tails)
reduc_fac_2

# plot the 10 different paths
path_data <- do.call("rbind", path_list_2)
ggplot(data = path_data, aes(x = a, y = b, group = id)) + geom_path(alpha = 0.4, size = 1)
path_data_2_tails <- do.call("rbind", path_list_2_tails)
ggplot(data = path_data_2_tails, aes(x = a, y = b, group = id)) + geom_path(alpha = 0.4, size = 1)
#################################################################################################
### e)
path <- metropolis(start_param = c(1.4, 0.4), sigma = 0.03, n_path = 4000)
# discard warm-up of 500
path <- path[500:4000,]
ggplot(data = path, aes(x = a, y = b)) + geom_path(alpha = 0.4, size = 1)
ggplot(path, aes(a, b)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")
acf(path$a)
acf(path$b)
# thinning could be used to reduce autocorrelation
#################################################################################################
### f)
mh_with_gibbs <- function(start_param, sigma = 1, n_path = 100){
  a <- start_param[1]
  b <- start_param[2]
  n_accept <- 0
  for(i in 1:n_path){
    a_new <- a[1] + rnorm(1, 0, sigma)
    alpha <- exp(log_alpha(a_new, b[1], a[1], b[1]))
    if(sample(c(TRUE, FALSE), size = 1, prob = c(alpha, 1-alpha))){
      a <- cbind(a_new, a)
      n_accept <- n_accept + 1
    } else {
      a <- cbind(a[1], a)
    }
    b_new <- rgamma(1, shape = length(danishuni$Loss)*a + 1, 
                    rate = sum(danishuni$Loss) + 10^-3)
    b <- cbind(b_new, b)
  }
  print(paste("acceptance rate: ", n_accept/n_path))
  return(data.frame(a = rev(a), b = rev(b)))
}

path_data <- mh_with_gibbs(start_param = c(1.4, 0.4), sigma = 0.03, n_path = 1000)
path_data <- mh_with_gibbs(start_param = c(1.4, 0.4), sigma = 0.1, n_path = 1000)


path_list_3 <- lapply(1:10, function(i){
  path <- mh_with_gibbs(start_param = c(runif(2, min = c(0.5, 0.2), max = c(2, 1))), 
                        sigma = 0.1)
  cbind(path, id = i)
})

# discard first half of values
path_list_3_tails <- lapply(path_list_3, function(path) path[round(nrow(path)/2):nrow(path),])
reduction_factor(path_list_3_tails)


# plot the 10 different paths
path_data_3 <- do.call("rbind", path_list_3)
ggplot(data = path_data_3, aes(x = a, y = b, group = id)) + geom_path(alpha = 0.4, size = 1)
