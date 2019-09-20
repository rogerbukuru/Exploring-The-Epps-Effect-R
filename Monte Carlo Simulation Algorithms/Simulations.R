## Author: Patrick Chang

# Script file of all the simulation algorithms to be called by other scripts

#---------------------------------------------------------------------------

### Cholskey Function

cholesky = function(sigma) {
  d = NROW(sigma)
  vi = rep(0, d)
  A = matrix(0, d, d)
  for (j in 1:d) {
    for (i in j:d) {
      vi[i] = sigma[i, j]
      if (j>1){
        for (k in 1:(j-1)) {
          vi[i] = vi[i] - A[j,k]*A[i,k]
        }
      }
      A[i,j] = vi[i] / sqrt(vi[j])
    }
  }
  return(A)
}

#---------------------------------------------------------------------------
## Code for Geometric Brownian Motion using the method from Paul Glasserman
#  in his book - Monte Carlo Methods in Financial Engineering

BM = function(n, mu, sigma, startprice, seed = 1) {
  # n - simlulation length
  # mu - vector input of the drift component
  # sigma - covariance matrix of the stocks
  
  k = NROW(sigma)
  mu = matrix(mu, k, 1)
  sigma = as.matrix(sigma)
  sigma2 = matrix(diag(sigma), k , 1)
  
  P = matrix(NA, n, dim(sigma)[2])
  P[1,] = startprice
  
  A = cholesky(sigma)
  b = mu - sigma2 / 2
  
  set.seed(seed)
  Z = matrix(rnorm(k*(n-1)), k, n-1)
  
  for (i in 2:n) {
    z = matrix(Z[,i-1], k, 1)
    X = b + A %*% z
    # X = mu + A %*% Z
    P[i,] = P[i-1,] * exp(X)
  }
  return(P)
}


#---------------------------------------------------------------------------
## Building the Merton Model using the method of Paul Glasserman in his book 
#   Monte Carlo Methods in Financial Engineering
#   This method allows me to build the path in 1 unit time increments
#   and there is no compensator in this method


# Multivariate Merton Model without the compensator

Merton = function(n, mu, sigma, lambda, a, b, startprice, seed = 1) {
  # n - simulation length
  # mu - vector input of the drift component for the underlying GBM
  # sigma - covariance matrix of the stocks
  # lambda - vector input of the poisson process intensity
  # a - vector input of mean of normal jump size
  # b - vector input of std dev of normal jump size
  
  # ensure all variable inputs are of the right structure
  sigma = as.matrix(sigma)
  k = nrow(sigma)
  sigma2 = matrix(diag(sigma), k ,1)
  mu = matrix(mu, k, 1)
  a = matrix(a, k, 1)
  b = matrix(b, k, 1)
  
  # initialise matrix of price to be returned
  lnS = matrix(NA, n, k)
  lnS[1,] = log(startprice)
  
  A = cholesky(sigma)
  d = mu - sigma2 / 2
  
  set.seed(seed)
  Z = matrix(rnorm(k*(n-1)), k, n-1)
  
  for (i in 2:n) {
    M = matrix(0, k, 1)
    N = matrix(0, k, 1)
    
    for (j in 1:k) {
      set.seed(seed+j+i+n)
      N[j] = rpois(1, lambda[j])
    }
    
    set.seed(seed+i)
    Z2 = matrix(rnorm(k, 0, 1), k, 1)
    M = a * N + b * sqrt(N) * Z2
    
    z = matrix(Z[,i-1], k, 1)
    
    lnS[i, ] = lnS[i-1, ] + d + A %*% z + M
  }
  P = exp(lnS)
  return(P)
}

#---------------------------------------------------------------------------
## Merton with Cauchy Jumps

Merton_Cauchy = function(n, mu, sigma, lambda, location, scale, startprice, seed = 1) {
  # n - simulation length
  # mu - vector input of the drift component for the underlying GBM
  # sigma - covariance matrix of the stocks
  # lambda - vector input of the poisson process intensity
  # a - vector input of mean of normal jump size
  # b - vector input of std dev of normal jump size
  
  # ensure all variable inputs are of the right structure
  sigma = as.matrix(sigma)
  k = nrow(sigma)
  sigma2 = matrix(diag(sigma), k ,1)
  mu = matrix(mu, k, 1)
  location = matrix(location, k, 1)
  scale = matrix(scale, k, 1)
  
  # initialise matrix of price to be returned
  lnS = matrix(NA, n, k)
  lnS[1,] = log(startprice)
  
  A = cholesky(sigma)
  d = mu #- sigma2 / 2
  
  set.seed(seed)
  Z = matrix(rnorm(k*(n-1)), k, n-1)
  
  for (i in 2:n) {
    M = matrix(0, k, 1)
    
    for (j in 1:k) {
      set.seed(seed+j+i*n)
      M[j] = sum(rcauchy(rpois(1, lambda[j]), location[j], scale[j]))
    }
    
    z = matrix(Z[,i-1], k, 1)
    
    lnS[i, ] = lnS[i-1, ] + d + A %*% z + M
  }
  P = exp(lnS)
  return(lnS)
}

#---------------------------------------------------------------------------

# Multivariate Merton Model with the compensator

Merton_compensated = function(n, mu, sigma, lambda, a, b, startprice, seed = 1) {
  # n - simulation length
  # mu - vector input of the drift component for the underlying GBM
  # sigma - covariance matrix of the stocks
  # lambda - vector input of the poisson process intensity
  # a - vector input of mean of normal jump size
  # b - vector input of std dev of normal jump size
  
  # ensure all variable inputs are of the right structure
  sigma = as.matrix(sigma)
  k = nrow(sigma)
  sigma2 = matrix(diag(sigma), k ,1)
  mu = matrix(mu, k, 1)
  a = matrix(a, k, 1)
  b = matrix(b, k, 1)
  
  # initialise matrix of price to be returned
  lnS = matrix(NA, n, k)
  lnS[1,] = log(startprice)
  
  zeta = exp(a + b^2/2) - 1
  A = cholesky(sigma)
  d = mu - sigma2 / 2 - lambda * zeta
  
  set.seed(seed)
  Z = matrix(rnorm(k*(n-1)), k, n-1)
  
  for (i in 2:n) {
    M = matrix(0, k, 1)
    N = matrix(0, k, 1)
    
    for (j in 1:k) {
      set.seed(seed+j+i*n)
      N[j] = rpois(1, lambda[j])
    }
    
    set.seed(seed+i)
    Z2 = matrix(rnorm(k, 0, 1), k, 1)
    M = a * N + b * sqrt(N) * Z2
    
    z = matrix(Z[,i-1], k, 1)
    
    lnS[i, ] = lnS[i-1, ] + d + A %*% z + M
  }
  P = exp(lnS)
  return(P)
}

#---------------------------------------------------------------------------

# GARCH(1,1)
garch = function(n, theta, lambda, w, correlation, starting, seed){
  lam1 = lambda[1]
  lam2 = lambda[2]
  w1 = w[1]
  w2 = w[2]
  theta1 = theta[1]
  theta2 = theta[2]
  
  set.seed(seed)
  Z = matrix(rnorm(4*(n-1), 0, 1), 4, n-1)
  
  ## Sigma2 path
  Sigma2 = matrix(NA, 2, n)
  Sigma2[,1] = starting
  
  for (i in 2:n){
    Sigma2[1, i] = Sigma2[1, i-1] + theta1 * (w1 - Sigma2[1, i-1])/86400 + sqrt(2*lam1*theta1*Sigma2[1, i-1]/86400)*Z[3, i-1]
    Sigma2[2, i] = Sigma2[2, i-1] + theta2 * (w2 - Sigma2[2, i-1])/86400 + sqrt(2*lam2*theta2*Sigma2[2, i-1]/86400)*Z[4, i-1]    
  }
  
  ## Price path
  P = matrix(NA, n, 2)
  P[1,] = log(c(100, 100))
  #P[1,] = starting
  
  for (i in 2:n){
    Sigma = matrix(c(Sigma2[1, i], sqrt(Sigma2[1, i]*Sigma2[2, i]) * correlation,
                     sqrt(Sigma2[1, i]*Sigma2[2, i]) * correlation, Sigma2[2, i]), 2, 2)
    P[i,] = chol(Sigma) %*% Z[1:2, i-1] * sqrt(1/86400) + P[i-1,]
  }
  return(exp(P))
}

# Reno
garch_reno = function(n, theta, lambda, w, correlation, starting, seed){
  lam1 = lambda[1]
  lam2 = lambda[2]
  w1 = w[1]
  w2 = w[2]
  theta1 = theta[1]
  theta2 = theta[2]
  
  set.seed(seed)
  Z = matrix(rnorm(4*(n-1), 0, 1), 4, n-1)
  
  ## Sigma2 path
  Sigma2 = matrix(NA, 2, n)
  Sigma2[,1] = starting
  
  for (i in 2:n){
    Sigma2[1, i] = Sigma2[1, i-1] + lam1 * (w1 - Sigma2[1, i-1])/86400 + sqrt(2*lam1*theta1*Sigma2[1, i-1]/86400)*Z[3, i-1]
    Sigma2[2, i] = Sigma2[2, i-1] + lam2 * (w2 - Sigma2[2, i-1])/86400 + sqrt(2*lam2*theta2*Sigma2[2, i-1]/86400)*Z[4, i-1]    
  }
  
  ## Price path
  P = matrix(NA, n, 2)
  P[1,] = log(c(100, 100))
  #P[1,] = starting
  
  for (i in 2:n){
    Sigma = matrix(c(Sigma2[1, i], sqrt(Sigma2[1, i]*Sigma2[2, i]) * correlation,
                     sqrt(Sigma2[1, i]*Sigma2[2, i]) * correlation, Sigma2[2, i]), 2, 2)
    P[i,] = chol(Sigma) %*% Z[1:2, i-1] * sqrt(1/86400) + P[i-1,]
  }
  return(exp(P))
}

#---------------------------------------------------------------------------
## Variance Gamma

variance_gamma = function(n, mu, sigma, beta, startprice, seed){
  # n - simlulation length
  # mu - vector input of the drift component
  # sigma - covariance matrix of the stocks
  # beta - scale for gamma
  
  k = NROW(sigma)
  mu = matrix(mu, k, 1)
  sigma = as.matrix(sigma)
  
  P = matrix(NA, n, dim(sigma)[2])
  P[1,] = startprice
  
  A = chol(sigma)
  
  set.seed(seed)
  Z = matrix(rnorm(k*(n-1)), k, n-1)
  set.seed(seed)
  Y = matrix(rgamma(k*(n-1), shape = 1/beta, scale = beta), k, n-1)
  
  for (i in 2:n) {
    z = matrix(Z[,i-1], k, 1)
    y = matrix(Y[,i-1], k, 1)
    P[i,] = P[i-1,] + mu * y + A %*% (sqrt(y)*z)
  }
  return(P)
}

#---------------------------------------------------------------------------
## Ornstein Uhlenbeck simulation

OU = function(n, mu, sigma, theta, startprice, seed){
  # n - simulation length
  # mu - vector of long term price averages
  # sigma - covariance matrix of the stocks
  # theta - reversion parameter
  
  # ensure all variable inputs are of the right structure
  sigma = as.matrix(sigma)
  k = nrow(sigma)
  
  # initialise matrix of price to be returned
  P = matrix(NA, n, k)
  P[1,] = log(startprice)
  
  set.seed(seed)
  Z = matrix(rnorm(k*(n-1)), k, n-1)
  
  for (i in 2:n) {
    z = matrix(Z[,i-1], k, 1)
    P[i,] = P[i-1,] + theta * (log(mu) - P[i-1,]) + chol(sigma) %*% z
  }
  return(exp(P))
}

