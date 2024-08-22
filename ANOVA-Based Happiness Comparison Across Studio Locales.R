ibrary(readr)
data <- read_csv("/Users/jalrc/downloads/WeightLoss_Program_9_23(1).csv")

#Question 5
A <- data$Happiness_End[data$Studio_Locale == "Portland"]
B <- data$Happiness_End[data$Studio_Locale == "Worcester"] C <- data$Happiness_End[data$Studio_Locale == "Brattleboro"] overall <- data$Happiness_End
A_posthappy <- mean(A) B_posthappy <- mean(B) C_posthappy <- mean(C) overall_post <- mean(overall)
print(A_posthappy) print(B_posthappy) print(C_posthappy) print(overall_post)

#Question 6, Variance
A_var <- sum((A - A_posthappy)^2)/(length(A) - 1)
B_var <- sum((B - B_posthappy)^2)/(length(B) - 1)
C_var <- sum((C - C_posthappy)^2)/(length(C) - 1)
overall_var <- sum((overall - overall_post)^2) / (length(overall) - 1)
round(A_var, 4) round(B_var, 4) round(C_var, 4) round(overall_var, 4)

#Question 7, Metrics
SST <- sum((overall - overall_post)^2) print(SST)
options(digits = 10)
SSE_A <- sum((A - A_posthappy)^2) SSE_B <- sum((B - B_posthappy)^2) SSE_C <- sum((C - C_posthappy)^2) SSE <- SSE_A + SSE_B + SSE_C
print(SSE)
SSM <- SST - SSE print(SSM)

#Question 8, Anova
k <- length(unique(data$Studio_Locale)) print(k)
n <- nrow(data)
print(n)
dfmodel<- k-1 print(dfmodel)
MSM <- SSM/dfmodel round(MSM, 4)
dferror <- n - k print(dferror)
MSE <- SSE/dferror round(MSE, 4)
dftotal <- n - 1 print(dftotal)
MST <- SST/dftotal round(MST, 4)

#Question 9
Fstat <- MSM/MSE
print(Fstat)
pval <- pf(Fstat,dfmodel , dferror,lower.tail = FALSE) print(pval)
#pval > alpha = 0.12
# fail to reject null hypothesis #can assume its about the same
