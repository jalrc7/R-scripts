

x <- c(-3, 1, 5, 7, 9)
p <- c(0.13, 0.21, 0.19, 0.43, 0.04)

E <- sum (x * p)
E
Var <- sum((x - E)^2 * p)
Var 

cdf_values <- cumsum(p)
a <- ifelse(-5 < min(x), 0, NA)
b <- sum(p[x <= 1])
c <- sum(p[x <= 7.3])
d <- sum(p[x <= 8.9999])
e <- sum(p[x <= 9.3])

cat("FX(-5) =", a, "\n")
cat("FX(1) =", b, "\n")
cat("FX(7.3) =", c, "\n")
cat("FX(8.9999) =", d, "\n")
cat("FX(9.3) =", e, "\n")

lambda <- 0.782
E_X <- 1 / lambda
Var_X <- 1 / lambda^2
cat("E(X) =", round(E_X, 2), "\n")
cat("Var(X) =", round(Var_X, 2), "\n")
n <- 10
probs <- seq(1/(2*n), 1-1/(2*n), by=1/n)
z <- qnorm(probs)
round(z, 4)

lambda <- 0.782
FX <- function(x) {
  ifelse(x >= 0, 1 - exp(-lambda * x), 0)
}
x_values <- c(0.2, 0.4, 0.5, 1.2, 1.9, 4.6)
cdf_values <- sapply(x_values, FX)
round(cdf_values, 2)

n <- 16

F <- function(k) {
  return(k/n)
}

x_values <- c(1, 5, 11, 16)

cdf_values <- sapply(x_values, F)
round(cdf_values, 4)

S <- c(9, 12, 15, 16, 19, 21, 22, 29, 31, 32)
n <- length(S)

F <- function(j) {
  return(j/n)
}

j_values <- c(1, 4, 7, 10)

cdf_values <- sapply(j_values, F)
round(cdf_values, 4)

S <- c(9, 12, 15, 16, 19, 21, 22, 29, 31, 32)
n <- length(S)

# Determine the probabilities to partition the standard normal distribution
p <- seq(1/n/2, by=1/n, length.out=n)

# Compute the quantiles
zk <- qnorm(p)
round(zk, 4)

S <- c(9, 12, 15, 16, 19, 21, 22, 29, 31, 32)

# Define a function to compute ECDF
my_ecdf <- function(x, data) {
  sum(data <= x) / length(data)
}

# Get ECDF values for specific points
a <- my_ecdf(9, S)
b <- my_ecdf(16, S)
c <- my_ecdf(22, S)
d <- my_ecdf(max(S), S)

# Print the values
cat("F(9) =", a, "\n")
cat("F(16) =", b, "\n")
cat("F(22) =", c, "\n")
cat("F(Smax) =", d, "\n")

n <- 10
# Create sequence from 1/(2n) to (2n-1)/(2n) for the middle points
p_values <- seq(1/(2*n), (2*n-1)/(2*n), by = 1/n)

# Obtain the quantiles for the standard normal distribution
z_values <- qnorm(p_values)

# Extract specific values
a <- z_values[1]
b <- z_values[4]
c <- z_values[7]
d <- z_values[10]

# Print the values
cat("z1(x) =", a, "\n")
cat("z4(x) =", round(b, 4), "\n")
cat("z7(x) =", round(c, 4), "\n")
cat("zn(x) =", round(d, 4), "\n")

n <- 10 # Sample size 

# Create the sequence for Psis values
Psis = seq(from = 1, to = n, by = 1) / (n + 1)

# Calculate the z values based on the Psis values
z_values <- qnorm(Psis)

# Extract specific values
a <- z_values[1]
b <- z_values[4]
c <- z_values[7]
d <- z_values[10]

# Print the values
cat("z1(x) =", round(a, 4), "\n")
cat("z4(x) =", round(b, 4), "\n")
cat("z7(x) =", round(c, 4), "\n")
cat("zn(x) =", round(d, 4), "\n")
