## Author: Patrick Chang
# Script file to simulate multivariate Merton Model
# includes an example to demonstrate the code

#---------------------------------------------------------------------------
## Cholskey Function
# Algorithm for this function is provided by Paul Glasserman

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
## Demonstration (Univariate)

library(ggplot2)
library(gridExtra)
library(latex2exp)

n = 500
mu = 0.01/86400
sigma = 0.1/86400
startprice = 100
a = 0
b = 100/86400
lambda = 0.2

Mert_Price = Merton(n, mu, sigma, lambda, a, b, startprice, 1)

#plots
Mert_P = ggplot(data=data.frame(Time = 1:n, Price = Mert_Price[,1]),
                aes(x=Time, y=Price)) + labs(y = "Price (P)") +
  geom_line(colour="red") + theme_bw() 
Mert_R = ggplot(data=data.frame(Time = 2:n, Return = diff(log(Mert_Price[,1]))),
                aes(x=Time, y=Return)) + labs(y = TeX("$Return ( \\log \\Delta P )$")) +
  geom_line(colour="red") + theme_bw()
Mert_QQ = ggplot(data.frame(qq = diff(log(Mert_Price[,1]))), aes(sample = qq)) +
  stat_qq() + stat_qq_line() + labs(x = "Theoretical N(0,1)",
                                    y = "Sample (Merton)")

lay <- rbind(c(1,1,1,3,3),
             c(2,2,2,3,3))
grid.arrange(Mert_P, Mert_R, Mert_QQ, layout_matrix = lay,
             top="Merton Model")