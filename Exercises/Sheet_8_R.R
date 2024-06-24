################################################################################
# Statistical Inference II, Sheet 8
################################################################################
#########################
# Exercise 1
#########################
library(ggplot2)
library(manipulate)
#we assume x_0 = 0
simulate_ar_1 <- function(rho, sigma, n = 50){
  # input check
  if(sigma <= 0) stop("standard deviation sigma needs to be positive!")
  # initiate x
  x <- rep(0, n)
  x[1] <- rnorm(1, mean = 0, sd = sigma)
  # iteratively compute the path
  for(i in 2:n){
    x[i] <- rho*x[i-1] + rnorm(1, mean = 0, sd = sigma)
  }
  c(0, x)
}

################################################################################
# simulate 10 paths for 0 < rho < 1
n<- 100
sigma = 0.5
rho = 0.8
paths_list <- lapply(1:10, function(i){
  x <- simulate_ar_1(rho = rho, sigma = sigma, n = n)
  data.frame(id = i, t = 0:n, x = x)
})
# combine them in one data frame for ggplot
paths_data <- do.call("rbind", paths_list)
manipulate(ggplot(data = paths_data, mapping = aes(x = t, y = x, group = id)) + 
             geom_path(col = "darkgrey", size = 1) + 
             geom_path(data = paths_data[paths_data$id == i,], col = "red", size = 1.5),
           i = slider(1,10))
# estimate the variance of the limit distribution
boxplot(sapply(paths_list, function(path) var(path$x)))
var_limit <- sigma^2/(1- rho^2)
abline(var_limit, 0, col = "red", lwd =2)
################################################################################
# simulate 10 paths for -1 < rho < 0
n<- 100
sigma = 1
rho = -0.7
paths_list <- lapply(1:10, function(i){
  x <- simulate_ar_1(rho = rho, sigma = sigma, n = n)
  data.frame(id = i, t = 0:n, x = x)
})
# combine them in one data frame for ggplot
paths_data <- do.call("rbind", paths_list)
manipulate(ggplot(data = paths_data, mapping = aes(x = t, y = x, group = id)) + 
             geom_path(col = "darkgrey", size = 1) + 
             geom_path(data = paths_data[paths_data$id == i,], col = "red", size = 1.5),
           i = slider(1,10))
# estimate the variance of the limit distribution
boxplot(sapply(paths_list, function(path) var(path$x)))
var_limit <- sigma^2/(1- rho^2)
abline(var_limit, 0, col = "red", lwd =2)
################################################################################
# simulate 10 paths for 1 <= rho
n<- 30
paths_list <- lapply(1:10, function(i){
  x <- simulate_ar_1(rho = 1.1, sigma = 5, n = n)
  data.frame(id = i, t = 0:n, x = x)
})
# combine them in one data frame for ggplot
paths_data <- do.call("rbind", paths_list)
manipulate(ggplot(data = paths_data, mapping = aes(x = t, y = x, group = id)) + 
             geom_path(col = "darkgrey", size = 1) + 
             geom_path(data = paths_data[paths_data$id == i,], col = "red", size = 1.5),
           i = slider(1,10))
################################################################################
# simulate 10 paths for -1 >= rho
n<- 30
paths_list <- lapply(1:10, function(i){
  x <- simulate_ar_1(rho = -1.1, sigma = 2, n = n)
  data.frame(id = i, t = 0:n, x = x)
})
# combine them in one data frame for ggplot
paths_data <- do.call("rbind", paths_list)
manipulate(ggplot(data = paths_data, mapping = aes(x = t, y = x, group = id)) + 
             geom_path(col = "darkgrey", size = 1) + 
             geom_path(data = paths_data[paths_data$id == i,], col = "red", size = 1.5),
           i = slider(1,10))
################################################################################
# simulate 10 paths for rho = 1
n<- 50
paths_list <- lapply(1:10, function(i){
  x <- simulate_ar_1(rho = 1, sigma = 2, n = n)
  data.frame(id = i, t = 0:n, x = x)
})
# combine them in one data frame for ggplot
paths_data <- do.call("rbind", paths_list)
manipulate(ggplot(data = paths_data, mapping = aes(x = t, y = x, group = id)) + 
             geom_path(col = "darkgrey", size = 1) + 
             geom_path(data = paths_data[paths_data$id == i,], col = "red", size = 1.5),
           i = slider(1,10))