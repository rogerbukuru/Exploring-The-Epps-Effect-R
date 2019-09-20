## Author: Patrick Chang
# Script file to simulate bivariate GARCH (1,1)
# includes an example to demonstrate the code

#---------------------------------------------------------------------------
## Building the bivariate GARCH (1,1) as specified by
# Roberto Reno - 2001
# simulation works via first order Euluer discretization

# Bivariate GARCH(1,1)

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

#---------------------------------------------------------------------------
## Demonstration (Univariate)

library(ggplot2)
library(gridExtra)
library(latex2exp)

n = 500
theta = c(0.035, 0.054)
lambda = c(0.296, 0.48)
w = c(0.636, 0.476)
starting = c(0.5, 0.6) 


GarchA_Price = garch(n ,theta, lambda, w, 0.35, starting, 1)

#plots
GarchA_P = ggplot(data=data.frame(Time = 1:n, Price = GarchA_Price[,1]),
                  aes(x=Time, y=Price)) + labs(y = "Price (P)") +
  geom_line(colour="darkgreen") + theme_bw() 
GarchA_R = ggplot(data=data.frame(Time = 2:n, Return = diff(log(GarchA_Price[,1]))),
                  aes(x=Time, y=Return)) + labs(y = TeX("$Return ( \\log \\Delta P )$")) +
  geom_line(colour="darkgreen") + theme_bw()
GarchA_QQ = ggplot(data.frame(qq = diff(log(GarchA_Price[,1]))), aes(sample = qq)) +
  stat_qq() + stat_qq_line() + labs(x = "Theoretical N(0,1)",
                                    y = "Sample (Garch(1,1))")

lay <- rbind(c(1,1,1,3,3),
             c(2,2,2,3,3))
grid.arrange(GarchA_P, GarchA_R, GarchA_QQ, layout_matrix = lay,
             top="GARCH (1,) - Andersen")