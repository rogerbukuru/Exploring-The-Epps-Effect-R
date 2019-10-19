## Author: Patrick Chang
# Scipt file to recover the plot for 
# "Effect of Volatility and Mean Reversion"

#---------------------------------------------------------------------------

library(ggplot2)
library(latex2exp)
library(svMisc)
source("Estimators/ftcorr.R")
source("Monte Carlo Simulation Algorithms/Simulations.R")

#---------------------------------------------------------------------------
## GARCH(1,1) - Andersen specification
# Parameters from Andersen

theta = c(0.035, 0.054)
lambda = c(0.296, 0.48)
w = c(0.636, 0.476)
starting = c(0.5, 0.6) # for variance

Slength = 21
correlation = seq(from = -0.99, to = 0.99, length.out = Slength)

HY_Garch_Syn = rep(NA, Slength)
MM_Garch_Syn = rep(NA, Slength)
HY_Garch_Asyn = rep(NA, Slength)
MM_Garch_Asyn = rep(NA, Slength)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  p = garch(10000, theta, lambda, w, correlation[i], starting, i)
  t = cbind(1:10000, 1:10000)
  
  set.seed(i)
  rm1 = sample(1:10000, 10000*0.2)
  set.seed(i + Slength)
  rm2 = sample(1:10000, 10000*0.2)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  pasyn = cbind(asset1[,2], asset2[,2])
  tasyn = cbind(asset1[,1], asset2[,1]) 
  
  HY_Garch_Syn[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM_Garch_Syn[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  HY_Garch_Asyn[i] = ftcorr(pasyn, tasyn, "HY", F)$Correlation[1,2]
  MM_Garch_Asyn[i] = ftcorr(pasyn, tasyn, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
## Ornstein Uhlenbeck

HY_OU_Syn = rep(NA, Slength)
MM_OU_Syn = rep(NA, Slength)
HY_OU_Asyn = rep(NA, Slength)
MM_OU_Asyn = rep(NA, Slength)

mu = c(100, 100) # long term price average
s1 = 0.1/86400
s2 = 0.2/86400
theta = c(0.035, 0.054) 
startprice = c(100, 100)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = OU(10000, mu, sigma, theta, startprice, i)
  t = cbind(1:10000, 1:10000)
  
  set.seed(i)
  rm1 = sample(1:10000, 10000*0.2)
  set.seed(i + Slength)
  rm2 = sample(1:10000, 10000*0.2)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  pasyn = cbind(asset1[,2], asset2[,2])
  tasyn = cbind(asset1[,1], asset2[,1]) 
  
  HY_OU_Syn[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM_OU_Syn[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  HY_OU_Asyn[i] = ftcorr(pasyn, tasyn, "HY", F)$Correlation[1,2]
  MM_OU_Asyn[i] = ftcorr(pasyn, tasyn, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
#### Plots in ggplot

## GARCH(1,1)
Garch = data.frame(
  Induced = correlation,
  MMSyn = MM_Garch_Syn,
  HYSyn = HY_Garch_Syn,
  MMAsyn = MM_Garch_Asyn,
  HYAsyn = HY_Garch_Asyn,
  simulation = 1:Slength
)

Garch_long = melt(Garch, id="simulation")
names(Garch_long) = c("simulation", "Method" ,  "correlation")

ggplot(data=Garch_long,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash", "longdash", "dashed")) +
  labs(title = "(a) GARCH (1,1)")+
  scale_color_manual(values=c('black', 'blue', 'red', 'purple', 'orange'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))

## Ornstein Uhlenbeck
OU = data.frame(
  Induced = correlation,
  MMSyn = MM_OU_Syn,
  HYSyn = HY_OU_Syn,
  MMAsyn = MM_OU_Asyn,
  HYAsyn = HY_OU_Asyn,
  simulation = 1:Slength
)

OU_long = melt(OU, id="simulation")
names(OU_long) = c("simulation", "Method" ,  "correlation")

ggplot(data=OU_long,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash", "longdash", "dashed")) +
  labs(title = "(b) Ornstein Uhlenbeck")+
  scale_color_manual(values=c('black', 'blue', 'red', 'purple', 'orange'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))

