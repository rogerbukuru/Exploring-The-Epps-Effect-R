## Author: Patrick Chang
# Scirpt file to create the price paths, volatility plots and 
# CDFs for all the processes considered

#---------------------------------------------------------------------------

## Source the file Simulations - for the functions to simulate the
# various processes
source("Simulation Algorithms/Simulations.R")

#---------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(latex2exp)

## GBM
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

## Merton
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

## Garch(1,1) - Reno
n = 500
theta = c(0.035, 0.054)
lambda = c(0.296, 0.48)
w = c(0.636, 0.476)
starting = c(0.5, 0.6) 


Garch_Price = garch_reno(n ,theta, lambda, w, 0.35, starting, 1)

#plots
Garch_P = ggplot(data=data.frame(Time = 1:n, Price = Garch_Price[,1]),
                 aes(x=Time, y=Price)) + labs(y = "Price (P)") +
  geom_line(colour="green") + theme_bw() 
Garch_R = ggplot(data=data.frame(Time = 2:n, Return = diff(log(Garch_Price[,1]))),
                 aes(x=Time, y=Return)) + labs(y = TeX("$Return ( \\log \\Delta P )$")) +
  geom_line(colour="green") + theme_bw()
Garch_QQ = ggplot(data.frame(qq = diff(log(Garch_Price[,1]))), aes(sample = qq)) +
  stat_qq() + stat_qq_line() + labs(x = "Theoretical N(0,1)",
                                    y = "Sample (Garch(1,1))")

lay <- rbind(c(1,1,1,3,3),
             c(2,2,2,3,3))
grid.arrange(Garch_P, Garch_R, Garch_QQ, layout_matrix = lay,
             top="GARCH (1,1) - Reno")

## Garch(1,1) - Andersen
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

## Ornstein Uhlenbeck
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

## Variance Gamma
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
