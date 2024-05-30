################################################################################
# Statistical Inference II, Sheet 6
################################################################################
#########################
# Exercise 1
#########################
# Simulate the data. n is the number of observations. 
# We assume a uniform distribution for weekday, zodiac and postcode
generate_data <- function(n){
  IQ <- rnorm(n, mean = 100, sd = 15)
  finger <- rnorm(n, mean = 7)
  weekday <- sample(c("MO", "TUE", "WED", "THU", "FRI", "SAT", "SUN"),
                    size = n, replace = TRUE)
  zodiac <- sample(c("Capricorn", "Aquarius", "Pisces", "Aries", 
                     "Taurus", "Gemini", "Cancer", "Leo", 
                     "Virgo", "Libra", "Scorpio", "Sagittarius"),
                   size = n, replace = TRUE)
  weight <- rnorm(n, mean = 3.5, sd = 0.5)
  postcode <- sample(0:9, size = n, replace = TRUE)
  
  data <- data.frame(IQ, 
                     weekday = factor(weekday, 
                                      labels = c("MO", "TUE", "WED", "THU", "FRI", "SAT", 
                                                 "SUN")), 
                     zodiac = factor(zodiac, labels = c("Capricorn", "Aquarius", "Pisces", 
                                                        "Aries", "Taurus", "Gemini", 
                                                        "Cancer", "Leo", "Virgo", "Libra", 
                                                        "Scorpio", "Sagittarius")),
                     finger,
                     
                     weight,
                     postcode = factor(postcode))
  data
}


# Fit the linear model
# Include weekday and zodiac as factor variables, the rest as numeric.
# What would change if postcode was treated as a factor as well?
set.seed(54321)
linear_model <- lm(IQ ~  weekday + zodiac + finger + weight + postcode,
                   data = generate_data(1000))
summary(linear_model)
# Please note that your results may differ. Coefficients are significant just by chance.

#########################
# Exercise 2
#########################
n <- 10
p_values <- 1 - pbinom(0:n - 1, n, prob = 0.5)
p_probs <- dbinom(0:n, n, prob = 0.5)
# sort p_values from small to large
p_values <- rev(p_values)
p_probs <- rev(p_probs)

plot(p_values, p_probs, type = "h")
points(p_values, p_probs)

# compute and plot cummulative distribution function
plot(p_values, cumsum(p_probs), type = "s")
# compare to uniform distribution function
segments(x0=0,y0=0,x1=1,y1=1,col="red")
######################################################################
n <- 40
p_values <- 1 - pbinom(0:n - 1, n, prob = 0.5)
p_probs <- dbinom(0:n, n, prob = 0.5)
# sort p_values from small to large
p_values <- rev(p_values)
p_probs <- rev(p_probs)

plot(p_values, p_probs, type = "h")
points(p_values, p_probs)

# compute and plot cummulative distribution function
plot(p_values, cumsum(p_probs), type = "s")
# compare to uniform distribution function
segments(x0=0,y0=0,x1=1,y1=1,col="red")
######################################################################
n <- 100
p_values <- 1 - pbinom(0:n - 1, n, prob = 0.5)
p_probs <- dbinom(0:n, n, prob = 0.5)
# sort p_values from small to large
p_values <- rev(p_values)
p_probs <- rev(p_probs)

plot(p_values, p_probs, type = "h")
points(p_values, p_probs)

# compute and plot cummulative distribution function
plot(p_values, cumsum(p_probs), type = "s")
# compare to uniform distribution function
segments(x0=0,y0=0,x1=1,y1=1,col="red")