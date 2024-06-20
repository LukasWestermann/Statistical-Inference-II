################################################################################
# Statistical Inference II, Sheet 7
################################################################################
#########################
# Exercise 3
#########################
library(ggplot2)
library(manipulate)
simulate_random_walk <- function(p,q,r,n = 20){
  # input check
  if(round(p + q + r) != 1) stop("probabilities have to add up to 1")
  # draw z's
  z <- sample(c(1,-1,0), size = n, prob = c(p,q,r), replace = TRUE)
  # compute the path
  c(0, cumsum(z))
}


# simulate 10 paths and create a list of data frames
n<- 20
paths_list <- lapply(1:10, function(i){
  x <- simulate_random_walk(p = 1/2, q = 1/3, r = 1/6, n)
  data.frame(id = i, t = 0:n, x = x)
})
# combine them in one data frame for ggplot
paths_data <- do.call("rbind", paths_list)
manipulate({ggplot(data = paths_data, mapping = aes(x = t, y = x, group = id)) + 
    geom_path(col = "darkgrey", size = 1) + 
    geom_path(data = paths_data[paths_data$id == i,], col = "red", size = 1.5)},
    i = slider(1,10))