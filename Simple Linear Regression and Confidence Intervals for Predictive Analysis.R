y = c(113, 117, 124, 122, 123, 121, 120, 128, 124, 116, 135, 121, 126, 123, 134)
x = c(4.3, 3.9, 6.2, 6.1, 6.7, 5.8, 6.7, 5.8, 6.1, 4.8, 4.2, 7.5, 6.5, 5.4, 6.7)
data = cbind(x, y)
avg = mean(y)
avg
n = length(x)

rho = sum((x-mean(x))*(y - mean(y)))/((n -1)* sd(x)* sd(y))
print(rho)
r = cor(x,y)

rho^2
model1 <- lm(y ~ x)
coefficients = coef(model1)
coefficients

beta = r*sd(y)/sd(x)
print(r)
beta0 = mean(y) - beta * mean(x)
print(c(beta, beta0))

sst = var(y)* (n -1)
sse = var(y)* (n -1)* (1 -rho^2)
ssm = sst - sse
sigma2 = sse/(n -2)
print(c(sst, sse, ssm, sigma2))

#6
f0 = (ssm/1)/(sse/(n-2))
print(f0)
pf(f0, df1 = 1, df2 = n-2, lower.tail = FALSE)


sqrt(sigma2)
print(sigma2)
se1 = 1/sd(x) * sqrt(sse/((n-1)* (n-2)))
se0 = sqrt(sse/(n-2))* sqrt(1/n + mean(x)^2/((n-1)*var(x)))
print(c(se1, se0))


cise = sqrt(sigma2*(1/n + (8- mean(x))^2/((n-1)* var(x))))
pise = sqrt(sigma2* ( 1 + 1/n + (8 - mean(x))^2/((n-1)* var(x))))
crit = qt(0.072/2, df = n - 2, lower.tail = FALSE)
yhat = 8*beta + beta0
print(c(beta, beta0))

c(yhat - crit*cise, yhat + crit*cise)
c(yhat - crit*pise, yhat + crit*pise)


ystar = mean(x)*beta + beta0
crit = qt(0.035/2, df = n - 2, lower.tail = FALSE)
cise = sqrt(sigma2*(1/n))
pise = sqrt(sigma2*(1 + 1/n))

c(ystar - crit*cise, ystar + crit*cise)
c(ystar - crit*pise, ystar +crit*pise)
