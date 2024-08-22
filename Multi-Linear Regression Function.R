mlr = function(Y, xk){
  n = length(Y)
  v1s = rep(1, n)
  
  p = dim(xk)[2]
 
  x = cbind(v1s, xk) #typical design matrix
  betahat = solve(t(x)%*%x)%*%t(x)%*%Y

  H = x%*%solve(t(x)%*%x)%*%t(x)
  lev = diag(H)
  
  yhat = x%*%betahat
  resid = Y - yhat 
  SSE = sum(resid^2)
  #we do not know p 
  
  MSE = SSE/(n -p-1)
  SST = var(Y)*(n - 1)
  SSM = SST - SSE
  MST = SST/(n - 1)
  MSM = SSM/p
  Fstat = MSM/MSE
  pval = pf(Fstat, p, n - p -1 , lower.tail = F) #how do you know if it is lower tail or right tail
  r2 = 1 - SSE/SST
  r2adj = 1 - MSE/MST
  sig = sqrt(MSE)
  sresid = resid/(sig* sqrt(1- lev))
  # sresid = resid/sig/sqrt(1- lev) corect but dont do
  sebhat = sig*sqrt(diag(solve(t(x)%*%x )))
  results = list("bhat" = betahat,
                 "sebhat"= sebhat,
                 "r2" = r2,
                 "r2adj" = r2adj,
                 "pval" = pval,
                 "sresid" = sresid, 
                 "lev"= lev)
  return(results)
}
