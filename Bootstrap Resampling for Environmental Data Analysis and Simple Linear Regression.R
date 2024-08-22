getwd()
setwd("/Users/jalrc/Desktop/Ma 416")
getwd()
library(readr)
source("~/mlr.R")
data <- read.csv("Environmental.csv")
data

# Extracting average temperatures for tropical climate zones
S <- data[data$ClimateZone == 'Tropical', 'AverageTemperature']
summary(S)
head(S)


calculate_tropical_mean <- function(x) {
  mean(x)  
}

mean_tropical_temp <- calculate_tropical_mean(S)

# Bootstrap function
bootstrap <- function(x, g, B = 100) {
  n <- length(x)
  theta.star <- vector(length = B)
  for (i in 1:B) {
    x.star <- x[sample.int(n, replace = TRUE)]
    theta.star[i] <- g(x.star)
  }
  return(theta.star)
}

set.seed(1234)

bootstrap_means_1 <- bootstrap(S, calculate_tropical_mean,B=1) 
bootstrap_means_1
bootstrap_1 <- round(mean(bootstrap_means_1), 4)
bootstrap_1

bootstrap_means_2 <- bootstrap(S, calculate_tropical_mean, B = 1)
bootstrap_means_2 
bootstrap_2 <- round(mean(bootstrap_means_2), 4)
bootstrap_2
mean_tropical_temp

set.seed(1234)
bootstrap_means <- bootstrap(S, calculate_tropical_mean, B = 10000)
hist(bootstrap_means, breaks = 100, col = 'lightblue', main = 'Bootstrap Means Distribution', xlab = 'Mean Temperature')
alpha <- 0.08  

CI_lower <- quantile(bootstrap_means, alpha / 2)  
CI_upper <- quantile(bootstrap_means, 1 - alpha / 2)  
round(CI_lower, 4)
round(CI_upper, 4)

set.seed(1234)
library(e1071)

calculate_skewness <- function(x) {
  skewness(x, na.rm = TRUE) 
}


set.seed(1234)

bootstrap_skewness <- bootstrap(S, calculate_skewness, B = 10000)
hist(bootstrap_skewness, breaks = 100, col = 'lightblue', main = 'Bootstrap Skewness Distribution', xlab = 'Skewness')

alpha <- 0.08  


CI_lower <- quantile(bootstrap_skewness, alpha / 2) 
CI_upper <- quantile(bootstrap_skewness, 1 - alpha / 2)  

round(CI_lower, 4)
round(CI_upper, 4)

x1 = data$AnnualRainfall
Y = data$AirQualityIndex

set.seed(1234)

#Question 5
simple_linear_regression <- function(X, Y) {
 
  mean_X <- mean(X)
  mean_Y <- mean(Y)
  
  sum_xy <- sum((X - mean_X) * (Y - mean_Y))
  sum_xx <- sum((X - mean_X)^2)
  b1 <- sum_xy / sum_xx
  b0 <- mean_Y - b1 * mean_X
  return(list(intercept = b0, slope = b1))
}

regression_bootstrap <- function(x, y, niterations){
  n <- length(x)
  betas <- matrix(NA, nrow = niterations, ncol = 2)
  for(k in 1:niterations){
    sampled_indices <- sample(index, n, replace = TRUE)
    Sx <- x[sampled_indices]
    Sy <- y[sampled_indices]
    regression_result <- lm(Sy ~Sx)
    betas[k, ] <- coef(regression_result)
  }
  return(betas)
}

set.seed(1234)
bootstrap_regression_1 <- regression_bootstrap(x1, Y, niterations = 1000)
mean_bootstrap_regression_1 <- colMeans(bootstrap_regression_1)
intercept <- round(mean_bootstrap_regression_1[1], 4)
intercept
slope_b <- round(mean_bootstrap_regression_1[2], 4)
slope_b
cat("The associated least squares regression model is: Y =", intercept, "+", slope_b, "* X")


bootstrap_regression_2 <- regression_bootstrap(x1, Y, niterations = 1000)
length(x1)
mean_bootstrap_regression_2 <- colMeans(bootstrap_regression_2)
intercept_c <- round(mean_bootstrap_regression_2[1], 4)
slope_c <- round(mean_bootstrap_regression_2[2], 4)
intercept_c
slope_c

#Question 6
set.seed(1234)

bootstrap_slopes <- regression_bootstrap(x1, Y, niterations = 25000)
bootstrap_slopes

slopes <- bootstrap_slopes[,2]
hist(slopes, breaks = 150, main = "Histogram of Bootstrap Slopes", xlab = "Slope")
(1 - 0.86) / 2  
CI_lower <- quantile(bootstrap_slopes, alpha)
CI_upper <- quantile(bootstrap_slopes, 1 - alpha)
round(CI_lower, 4)
round(CI_upper, 4)
#Question 7
set.seed(1234)
bootstrap_intercepts <- regression_bootstrap(x1, Y, niterations = 25000)
bootstrap_intercepts
intercept <- bootstrap_intercepts[, 1]
intercept
hist(intercept, breaks = 150, main = "Histogram of Bootstrap Intercepts", xlab = "Intercept")
alpha <- (1 - 0.86) / 2  
CI_lower_intercept <- quantile(bootstrap_intercepts, alpha)
CI_upper_intercept <- quantile(bootstrap_intercepts, 1 - alpha)
round(CI_lower_intercept, 4)
round(CI_upper_intercept, 4)

#Question 8
set.seed(1234)

bootstrap <- function(x, y, B = 25000) {
  n <- length(x)
  corr_coefficients <- vector(length = B)
  for (i in 1:B) {
    sampled_indices <- sample.int(n, replace = TRUE)
    x_star <- x[sampled_indices]
    y_star <- y[sampled_indices]
    model <- lm(y_star ~ x_star)
    corr_coefficients[i] <- cor(x_star, y_star)
  }
  return(corr_coefficients)
}

correlation_coefficients <- bootstrap(x1, Y)
cor(x1, Y)


hist(correlation_coefficients, breaks = 150, main = "Histogram of Bootstrap Correlation Coefficients", xlab = "Correlation Coefficient")

conf_interval_corr <- quantile(correlation_coefficients, c(0.07, 0.93))


cat("86% Confidence Interval for the Correlation Coefficient: [", round(conf_interval_corr[1], 4), ",", round(conf_interval_corr[2], 4), "]")


