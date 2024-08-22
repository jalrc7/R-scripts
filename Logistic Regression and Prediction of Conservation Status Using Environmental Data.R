logit_function <- function(p){
  #ln(p/ 1- p)
  log(p /( 1 - p))
}

logit_1 <- logit_function(0.5)
logit_1
l_1 <-round(logit_1, 2)
l_1
logit_2 <- logit_function(0.3)
logit_2
l_1 <-round(logit_2, 2)
l_1
logit_3 <- logit_function(0.76)
logit_3
l_1 <-round(logit_3, 2)
l_1
logit_4 <- logit_function(0.95)
logit_4
l_1 <-round(logit_4, 2)
l_1

library(readr)
mydata <- read.csv("Environmental.csv")
standardize <- function(x){
  (x - mean(x)) / sd(x)
}
x1 <- standardize(mydata$AverageTemperature)
x2 <- standardize(mydata$AnnualRainfall)
x3 <- standardize(mydata$SpeciesCount)
x4 <- standardize(mydata$AirQualityIndex)
x5<- standardize(mydata$ForestAreaPercentage)
Y <- ifelse(mydata$ConservationStatus %in% c("Endangered", "CriticallyEndangered"), 1, 0)
Y

n <- nrow(mydata)
m1 = glm(Y~ x1 + x2 + x3 + x4 + x5, data = mydata, family = 'binomial')
summary(m1)

predicted_probabilities <- predict(m1, type = "response")
errors <- Y - predicted_probabilities
sse <- sum(errors^2)
sse

absolute_residuals <- abs(Y - predicted_probabilities)
absolute_residuals
predicted_probabilities
count_1 <- sum(absolute_residuals >= 0 & absolute_residuals <= 0.2)
count_1
count_2 <- sum(absolute_residuals >= 0.2 & absolute_residuals <= 0.4)
count_2
count_3 <- sum(absolute_residuals >= 0.4 & absolute_residuals <= 0.6)
count_3
count_4 <- sum(absolute_residuals >= 0.6 & absolute_residuals <= 0.8)
count_4
count_5 <- sum(absolute_residuals >= 0.8 & absolute_residuals <= 1.0)
count_5

new_data <- data.frame(
  x1 <- 25, 
  x2 <- 5,
  x3 <- 10,
  x4 <- 10, 
  x5 <- 12
)

predicted_probability <- predict(m1, newdata = new_data, type = "response")

predicted_probability_rounded <- round(predicted_probability, 4)
predicted_probability_rounded

