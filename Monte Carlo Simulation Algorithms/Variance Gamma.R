## Author: Patrick Chang
# Script file to simulate multivariate Variance Gamma
# includes an example to demonstrate the code

#---------------------------------------------------------------------------
## Building the Variance Gamma using the method of Paul Glasserman in his book 
#   Monte Carlo Methods in Financial Engineering
#   This method allows me to build the path in 1 unit time increments


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
## Demonstration (Univariate)

library(ggplot2)
library(gridExtra)
library(latex2exp)

n = 500
mu = 0.01/86400
sigma = 0.1/86400
b = 1
startprice = 100

VG_Price = variance_gamma(n, mu, sigma, b, startprice, 1)

#plots
VG_P = ggplot(data=data.frame(Time = 1:n, Price = VG_Price[,1]),
              aes(x=Time, y=Price)) + labs(y = "Price (P)") +
  geom_line(colour="orange") + theme_bw() 
VG_R = ggplot(data=data.frame(Time = 2:n, Return = diff(log(VG_Price[,1]))),
              aes(x=Time, y=Return)) + labs(y = TeX("$Return ( \\log \\Delta P )$")) +
  geom_line(colour="orange") + theme_bw()
VG_QQ = ggplot(data.frame(qq = diff(log(VG_Price[,1]))), aes(sample = qq)) +
  stat_qq() + stat_qq_line() + labs(x = "Theoretical N(0,1)",
                                    y = "Sample (Variance Gamma)")

lay <- rbind(c(1,1,1,3,3),
             c(2,2,2,3,3))
grid.arrange(VG_P, VG_R, VG_QQ, layout_matrix = lay,
             top="Variance Gamma")