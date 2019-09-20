## Author: Patrick Chang
# Script file to simulate multivariate Geometric Brownian motion
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
## Code for Geometric Brownian Motion using the method from Paul Glasserman
#  in his book - Monte Carlo Methods in Financial Engineering

# timesteps (dt) can be controlled by scaling mu and sigma accordingly

BM = function(n, mu, sigma, startprice, seed = 1) {
  # n - simlulation length
  # mu - vector input of the drift component
  # sigma - covariance matrix of the stocks
  # startprice - starting price for the assets
  
  # NB: all inputs must have appropriate dimensions
  
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
## Demonstration (Univariate)

library(ggplot2)
library(gridExtra)
library(latex2exp)

n = 500
mu = 0.01/86400
sigma = 0.1/86400
startprice = 100

GBM_Price = BM(n, mu, sigma, startprice, 1)

#plots
GBM_P = ggplot(data=data.frame(Time = 1:n, Price = GBM_Price),
               aes(x=Time, y=Price)) + labs(y = "Price (P)") +
  geom_line(colour="blue") + theme_bw() 
GBM_R = ggplot(data=data.frame(Time = 2:n, Return = diff(log(GBM_Price))),
               aes(x=Time, y=Return)) + labs(y = TeX("$Return ( \\log \\Delta P )$")) +
  geom_line(colour="blue") + theme_bw()
GBM_QQ = ggplot(data.frame(qq = diff(log(GBM_Price))), aes(sample = qq)) +
  stat_qq() + stat_qq_line() + labs(x = "Theoretical N(0,1)",
                                    y = "Sample (GBM)")

lay <- rbind(c(1,1,1,3,3),
             c(2,2,2,3,3))
grid.arrange(GBM_P, GBM_R, GBM_QQ, layout_matrix = lay,
             top="Geometric Brownian Motion")