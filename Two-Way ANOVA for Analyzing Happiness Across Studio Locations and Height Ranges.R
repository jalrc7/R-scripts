library(readr)
data <- read_csv("/Users/jalrc/downloads/WeightLoss_Program_9_23(1).csv")

#Ho for (Location) : no significant difference in the mean happiness values across
#different studios
#Ho for (Height): no significant difference in the mean happiness values 
#across different heights
#Ho: interaction between Factor 1 and Factor 2: There is no interaction effect
#between studios and height range

#Factor A - Question 2
n1 <- sum(data$Studio_Locale == "Portland")
print(n1)
n2 <- sum(data$Studio_Locale == "Worcester")
print(n2)
n3 <- sum(data$Studio_Locale == "Brattleboro")
print(n3)
# Factor B - Question 3
b1 <- sum(data$Height_cm >= 150 & data$Height_cm <= 158)
print(b1)
b2 <- sum(data$Height_cm >= 159 & data$Height_cm <= 166)
print(b2)
b3 <- sum(data$Height_cm >= 167 & data$Height_cm <= 177)
print(b3)
b4 <- sum(data$Height_cm >= 178 & data$Height_cm <= 190)
print(b4)

#Question 4

A1B1 <- sum(data$Studio_Locale == "Portland" & data$Height_cm >= 150 & data$Height_cm <= 158)
A1B2 <- sum(data$Studio_Locale == "Portland" & data$Height_cm >= 159 & data$Height_cm <= 166)
A1B3 <- sum(data$Studio_Locale == "Portland" & data$Height_cm >= 167 & data$Height_cm <= 177)
A1B4 <- sum(data$Studio_Locale == "Portland" & data$Height_cm >= 178 & data$Height_cm <= 190)

A2B1 <- sum(data$Studio_Locale == "Worcester" & data$Height_cm >= 150 & data$Height_cm <= 158)
A2B2 <- sum(data$Studio_Locale == "Worcester" & data$Height_cm >= 159 & data$Height_cm <= 166)
A2B3 <- sum(data$Studio_Locale == "Worcester" & data$Height_cm >= 167 & data$Height_cm <= 177)
A2B4 <- sum(data$Studio_Locale == "Worcester" & data$Height_cm >= 178 & data$Height_cm <= 190)

A3B1 <- sum(data$Studio_Locale == "Brattleboro" & data$Height_cm >= 150 & data$Height_cm <= 158)
A3B2 <- sum(data$Studio_Locale == "Brattleboro" & data$Height_cm >= 159 & data$Height_cm <= 166)
A3B3 <- sum(data$Studio_Locale == "Brattleboro" & data$Height_cm >= 167 & data$Height_cm <= 177)
A3B4 <- sum(data$Studio_Locale == "Brattleboro" & data$Height_cm >= 178 & data$Height_cm <= 190)


table <- list(
  A1B1 = A1B1,
  A1B2 = A1B2,
  A1B3 = A1B3,
  A1B4 = A1B4, 
  A2B1 = A2B1,
  A2B2 = A2B2,
  A2B3 = A2B3,
  A2B4 = A2B4,
  A3B1 = A3B1,
  A3B2 = A3B2,
  A3B3 = A3B3,
  A3B4 = A3B4 
)

print(table)
#Question 5

total <- nrow(data)
print(total)
overall_happy <- mean(data$Happiness_End)
print(overall_happy)
sd <- sd(data$Happiness_End)
print(sd)

#Question 6
A <- data$Happiness_End[data$Studio_Locale == "Portland"]
B <- data$Happiness_End[data$Studio_Locale == "Worcester"]
C <- data$Happiness_End[data$Studio_Locale == "Brattleboro"]

A_posthappy <- mean(A)
B_posthappy <- mean(B)
C_posthappy <- mean(C)

SST <- sum((data$Happiness_End - overall_happy)^2)
print(SST)

SSM1 <- (n1 *(A_posthappy - overall_happy)^2)+ (n2 *(B_posthappy - overall_happy)^2 ) + (n3 *(C_posthappy - overall_happy)^2)
print(SSM1)

#mean of B 
mean_B1 <- mean(data$Happiness_End[data$Height_cm >= 150 & data$Height_cm <= 158])
mean_B2 <- mean(data$Happiness_End[data$Height_cm >= 159 & data$Height_cm <= 166])
mean_B3 <- mean(data$Happiness_End[data$Height_cm >= 167 & data$Height_cm <= 177])
mean_B4 <- mean(data$Happiness_End[data$Height_cm >= 178 & data$Height_cm <= 190])

SSM2 <- (b1 * (mean_B1 - overall_happy)^2) + (b2 * (mean_B2 - overall_happy)^2) + (b3 * (mean_B3 - overall_happy)^2) + (b4 * (mean_B4 - overall_happy)^2)
print(SSM2)
#Question 7

SSE <- 0
if(A1B1 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Portland" & data$Height_cm >= 150 & data$Height_cm <= 158] - mean_A1B1)^2)
}

if(A1B2 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Portland" & data$Height_cm >= 159 & data$Height_cm <= 166] - mean_A1B2)^2)
}

if(A1B3 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Portland" & data$Height_cm >= 167 & data$Height_cm <= 177] - mean_A1B3)^2)
}

if(A1B4 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Portland" & data$Height_cm >= 178 & data$Height_cm <= 190] - mean_A1B4)^2)
}

if(A2B1 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Worcester" & data$Height_cm >= 150 & data$Height_cm <= 158] - mean_A2B1)^2)
}

if(A2B2 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Worcester" & data$Height_cm >= 159 & data$Height_cm <= 166] - mean_A2B2)^2)
}

if(A2B3 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Worcester" & data$Height_cm >= 167 & data$Height_cm <= 177] - mean_A2B3)^2)
}

if(A2B4 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Worcester" & data$Height_cm >= 178 & data$Height_cm <= 190] - mean_A2B4)^2)
}

if(A3B1 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Brattleboro" & data$Height_cm >= 150 & data$Height_cm <= 158] - mean_A3B1)^2)
}

if(A3B2 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Brattleboro" & data$Height_cm >= 159 & data$Height_cm <= 166] - mean_A3B2)^2)
}

if(A3B3 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Brattleboro" & data$Height_cm >= 167 & data$Height_cm <= 177] - mean_A3B3)^2)
}

if(A3B4 > 1) {
  SSE <- SSE + sum((data$Happiness_End[data$Studio_Locale == "Brattleboro" & data$Height_cm >= 178 & data$Height_cm <= 190] - mean_A3B4)^2)
}

print(SSE)

#Question 8

SSI <- SST - SSM1 - SSM2 - SSE
print(SSI)

#Question 9

df_studio <- 3 - 1
df_heights <- 4 - 1
df_interaction <- (3-1)* (4-1)
n <- nrow(data)
df_error <- n - 3 * 4
df_total <- n -1

MSMA <- SSM1/ df_studio
MSMB <- SSM2/df_heights
MSI <- SSI/ df_interaction
MSE <- SSE/df_error

print(df_studio)
print(df_heights)
print(df_interaction)
print(df_error)
print(df_total)

print(MSMA)
print(MSMB)
print(MSI)
print(MSE)

F_Studio <- MSMA / MSE
F_Height <- MSMB / MSE
F_I <- MSI / MSE


p_valS <- pf(F_Studio, df_studio, df_error, lower.tail = FALSE)
p_valH <- pf(F_Height, df_heights, df_error, lower.tail = FALSE)
p_valI <- pf(F_I, df_interaction, df_error, lower.tail = FALSE)

print(p_valS)
print(p_valH)
print(p_valI)
