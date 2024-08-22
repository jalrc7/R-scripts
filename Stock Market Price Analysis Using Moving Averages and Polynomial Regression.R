library(ggplot2)
as.POSIXct(655849762, tz = "UTC")
as.POSIXct(4721749254,tz = "UTC")

mydata <- read.csv("Stock_Market.csv")

plot(as.POSIXct(mydata$Time), mydata$Stock.Price, pch=16, col='blue')
lines(as.POSIXct(mydata$Time), mydata$Stock.Price, pch=19, col='black')

avg_T2 <- mean(mydata$Stock.Price[1:3])
avg_T2

avg_T19 <- mean(mydata$Stock.Price[18:20])
avg_T19
avg_T78 <- mean(mydata$Stock.Price[77:79])
avg_T78

mydata$Moving_Average <- rep(NA, nrow(mydata))
mydata$Moving_Average
for (i in 2:(nrow(mydata) - 1)) {
  mydata$Moving_Average[i] <- mean(mydata$Stock.Price[(i - 1):(i + 1)], na.rm = TRUE)
}

mydata$Moving_Average <- round(mydata$Moving_Average, 2)


mean_moving_average <- mean(mydata$Moving_Average, na.rm = TRUE)


sd_moving_average <- sd(mydata$Moving_Average, na.rm = TRUE)


mean_moving_average
sd_moving_average

S <- mydata$Stock.Price
n <- length(mydata$Time)
T = seq(from = 1, to = n, by= 1)

T2 = T^2 
T3 = T^3 
T4 = T^4 
T5 = T^5


plot(T, S, pch =16, cez = .5, col = 'blue')
v1s = rep(1, n)
Tmatrix = cbind(v1s, T, T2, T3, T4, T5)
M = lm(S ~ T + T2 + T3 + T4 + T5)
betahat = coef(M)
betahat 

shat = Tmatrix%*%betahat 
shat
lines(T, shat, col = 'green')
lines(T, shat, col = 'red')

sse <- sum((S -shat )^2)
sse

rmse <- sqrt(sse/n)
rmse

sse_rounded <- round(sse, 4)
rmse_rounded <- round(rmse, 4)
sse_rounded
rmse_rounded
