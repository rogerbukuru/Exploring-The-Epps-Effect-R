## Author: Patrick Chang
# Script file to simulate multivariate Ornstein Uhlenbeck
# includes an example to demonstrate the code

#---------------------------------------------------------------------------
## Simulation works via first order Euler discretization

# Ornstein Uhlenbeck

OU = function(n, mu, sigma, theta, startprice, seed){
  # n - simulation length
  # mu - long term price average
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

#---------------------------------------------------------------------------
## Demonstration (Univariate)

library(ggplot2)
library(gridExtra)
library(latex2exp)

n = 500
mu = 100
sigma = 0.1/86400
theta = 0.035
startprice = 100

OU_Price = OU(n, mu, sigma, theta, startprice, 1)

#plots
OU_P = ggplot(data=data.frame(Time = 1:n, Price = OU_Price[,1]),
              aes(x=Time, y=Price)) + labs(y = "Price (P)") +
  geom_line(colour="purple") + theme_bw() 
OU_R = ggplot(data=data.frame(Time = 2:n, Return = diff(log(OU_Price[,1]))),
              aes(x=Time, y=Return)) + labs(y = TeX("$Return ( \\log \\Delta P )$")) +
  geom_line(colour="purple") + theme_bw()
OU_QQ = ggplot(data.frame(qq = diff(log(OU_Price[,1]))), aes(sample = qq)) +
  stat_qq() + stat_qq_line() + labs(x = "Theoretical N(0,1)",
                                    y = "Sample (Ornstein Uhlenbeck)")

lay <- rbind(c(1,1,1,3,3),
             c(2,2,2,3,3))
grid.arrange(OU_P, OU_R, OU_QQ, layout_matrix = lay,
             top="Ornstein Uhlenbeck")