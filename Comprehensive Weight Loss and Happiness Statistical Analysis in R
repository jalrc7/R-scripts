library(readr)
data <- read_csv("/Users/jalrc/Downloads/WeightLoss_Program_9_23(1).csv") 
data$weight_diff <- data$Weight_Start_lb - data$Weight_End_lb
average_weight_loss <- mean(data$weight_diff)
print(average_weight_loss)

SE <- sd(data$weight_diff) / sqrt(length(data$weight_diff))
print(SE)

# b) Critical value for two-tail 3.48%-alpha
alpha <- 0.0348
z_crit <- qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)  
print(z_crit)

# c) Confidence interval for the mean difference in weight loss
CI_lower <- mean(data$weight_diff) - z_crit * SE
CI_upper <- mean(data$weight_diff) + z_crit * SE
print(CI_lower)
print(CI_upper)
# d) Check for significance
significant <- ifelse(CI_lower > 0 | CI_upper < 0, "yes", "no")
print(significant)

list(SE = round(SE, 4), Z_crit = round(z_crit, 4), CI = c(round(CI_lower, 4), round(CI_upper, 4)), Significant = significant)
data_A <- subset(data, Age < 30)
data_B <- subset(data, Age >= 30)

point_estimate_A <- mean(data_A$Happiness_Start)
point_estimate_B <- mean(data_B$Happiness_Start)

difference_point_estimate <- abs(point_estimate_A - point_estimate_B)

# Output the results
point_estimate_A
point_estimate_B
difference_point_estimate

# Split the data based on age
group_A <- data[data$Age < 30,]$Happiness_Start
group_B <- data[data$Age >= 30,]$Happiness_Start

# Test for homoscedasticity
f_stat <- var(group_A) / var(group_B)
p_val <- pf(f_stat, df1 = length(group_A) - 1, df2 = length(group_B) - 1, lower.tail = FALSE)


s_A <- sd(data_A$Happiness_Start)
s_B <- sd(data_B$Happiness_Start)
n_A <- length(data_A$Happiness_Start)
n_B <- length(data_B$Happiness_Start)

SE <- sqrt((s_A^2/n_A) + (s_B^2/n_B))
df <- n_A + n_B - 2
t_critical <- qt(1 - 0.0745/2, df)

difference_in_sample_means <- point_estimate_A - point_estimate_B

lower_bound <- difference_in_sample_means - t_critical * SE
upper_bound <- difference_in_sample_means + t_critical * SE

significant_difference <- ifelse((lower_bound > 0 & upper_bound > 0) | 
                                   (lower_bound < 0 & upper_bound < 0), "yes", "no")

# Output the results
SE
df
t_critical
lower_bound
upper_bound
significant_difference


s_A <- sd(data_A$Happiness_Start)
s_B <- sd(data_B$Happiness_Start)
n_A <- length(data_A$Happiness_Start)
n_B <- length(data_B$Happiness_Start)

SE <- sqrt((s_A^2/n_A) + (s_B^2/n_B))
df <- ((s_A^2/n_A + s_B^2/n_B)^2) / ((s_A^4/(n_A^2*(n_A-1))) + (s_B^4/(n_B^2*(n_B-1))))
t_critical <- qt(1 - 0.0745/2, df)

difference_in_sample_means <- point_estimate_A - point_estimate_B

lower_bound <- difference_in_sample_means - t_critical * SE
upper_bound <- difference_in_sample_means + t_critical * SE

# Output the results
SE
df
t_critical
lower_bound
upper_bound
