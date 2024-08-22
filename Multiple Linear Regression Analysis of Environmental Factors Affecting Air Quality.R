library(readr)
source("~/mlr.R")
mydata <- read.csv("Environmental.csv")

Y_original = mydata$AirQualityIndex
x1 = mydata$ForestAreaPercentage
x2 = mydata$AverageTemperature

x3 = mydata$SpeciesCount
x4 = mydata$AnnualRainfall
xk = cbind(x1, x2,x3, x4)

mean_Y = mean(Y_original)
sd_Y = sd(Y_original)
Y = (Y_original - mean_Y) / sd_Y

mean_X <- mean(x1)
mean_Y <- mean(Y)

b1 <- sum((x1 - mean_X) * (Y - mean_Y)) / sum((x1 - mean_X)^2)
b0 <- mean_Y - b1 * mean_X
Y_hat <- b0 + b1 * x1
SSM_X1 <- sum((Y_hat - mean_Y)^2)
SST <- sum((Y - mean_Y)^2)
SSM_X1
SST

MLR_X1_X2 = mlr(Y, cbind(x1, x2))
MLR_X1_X2
MLR_X1_X2_X3 = mlr(Y, cbind(x1, x2, x3))
MLR_X1_X2_X3 
MLR_Full = mlr(Y, cbind(x1, x2, x3, x4))
SSM_X1_X2 = MLR_X1_X2$SSM
SSM_X1_X2_X3 = MLR_X1_X2_X3$SSM 
SSM_Full = MLR_Full$SSM

SSM_X1_X2
SSM_X1_X2_X3
SSM_Full

percent_change_X1 <- (SSM_X1 / SSM_Full) * 100
percent_change_X2 <- ((SSM_X1_X2 - SSM_X1) /SSM_Full ) * 100
percent_change_X3 <- ((SSM_X1_X2_X3 - SSM_X1_X2) /SSM_Full) * 100
percent_change_X4 <- ((SSM_Full - SSM_X1_X2_X3) / SSM_Full) * 100

percent_change_X1
percent_change_X2
percent_change_X3 
percent_change_X4 

MLR_Excl_X1 <- mlr(Y, cbind(x2, x3, x4))
MLR_Excl_X2 <- mlr(Y, cbind(x1, x3, x4))
MLR_Excl_X3 <- mlr(Y, cbind(x1, x2, x4))
MLR_Excl_X4 <- mlr(Y, cbind(x1, x2, x3))
MLR_Excl_X1$SSM
MLR_Excl_X2$SSM
MLR_Excl_X3$SSM
MLR_Excl_X4$SSM

SSM_X1C <- SSM_Full - MLR_Excl_X1$SSM
SSM_X2C <- SSM_Full - MLR_Excl_X2$SSM
SSM_X3C <- SSM_Full - MLR_Excl_X3$SSM
SSM_X4C <- SSM_Full - MLR_Excl_X4$SSM

SSM_X1C
SSM_X2C
SSM_X3C 
SSM_X4C

percent_X1C <- (SSM_X1C / SSM_Full) * 100
percent_X2C <- (SSM_X2C / SSM_Full) * 100
percent_X3C <- (SSM_X3C / SSM_Full) * 100
percent_X4C <- (SSM_X4C / SSM_Full) * 100

percent_X1C
percent_X2C
percent_X3C
percent_X4C
