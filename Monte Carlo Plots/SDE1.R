## Author: Patrick Chang
# Scipt file to recover the plot for 
# "The effect of various diffusion processes"

#---------------------------------------------------------------------------

library(ggplot2)
library(latex2exp)
library(svMisc)
source("ftcorr.R")
source("Simulation Algorithms/Simulations.R")

#---------------------------------------------------------------------------
## case lambda = 0

lambda = c(0, 0)

Slength = 21
correlation = seq(from = -0.99, to = 0.99, length.out = Slength)
# be careful, cant set final value = 1, 
# otherwise cholesky will produce NaN or matrix wont be positive definite

HY_lam0 = rep(NA, Slength)
MM_lam0 = rep(NA, Slength)
HY_lam0_asyn = rep(NA, Slength)
MM_lam0_asyn = rep(NA, Slength)

mu = c(0.01/86400, 0.01/86400)
s1 = 0.1/86400
s2 = 0.2/86400
a = c(0,0)
b = c(100/86400, 100/86400)
startprice = c(100, 100)

seed = 1:Slength

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = Merton(10000, mu, sigma, lambda, a, b, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  
  set.seed(seed[i])
  rm1 = sample(1:10000, 10000*0.2)
  set.seed(seed[i] + Slength)
  rm2 = sample(1:10000, 10000*0.2)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  pasyn = cbind(asset1[,2], asset2[,2])
  tasyn = cbind(asset1[,1], asset2[,1])  
  
  HY_lam0[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM_lam0[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  HY_lam0_asyn[i] = ftcorr(pasyn, tasyn, "HY", F)$Correlation[1,2]
  MM_lam0_asyn[i] = ftcorr(pasyn, tasyn, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
## case lambda = 0.2

lambda = c(0.2, 0.2)

HY_lam.2 = rep(NA, Slength)
MM_lam.2 = rep(NA, Slength)
HY_lam.2_asyn = rep(NA, Slength)
MM_lam.2_asyn = rep(NA, Slength)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = Merton(10000, mu, sigma, lambda, a, b, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  
  set.seed(seed[i])
  rm1 = sample(1:10000, 10000*0.2)
  set.seed(seed[i] + Slength)
  rm2 = sample(1:10000, 10000*0.2)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  pasyn = cbind(asset1[,2], asset2[,2])
  tasyn = cbind(asset1[,1], asset2[,1])  
  
  HY_lam.2[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM_lam.2[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  HY_lam.2_asyn[i] = ftcorr(pasyn, tasyn, "HY", F)$Correlation[1,2]
  MM_lam.2_asyn[i] = ftcorr(pasyn, tasyn, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
## case lambda = 0.5

lambda = c(0.5, 0.5)

HY_lam.5 = rep(NA, Slength)
MM_lam.5 = rep(NA, Slength)
HY_lam.5_asyn = rep(NA, Slength)
MM_lam.5_asyn = rep(NA, Slength)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = Merton(10000, mu, sigma, lambda, a, b, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  
  set.seed(seed[i])
  rm1 = sample(1:10000, 10000*0.2)
  set.seed(seed[i] + Slength)
  rm2 = sample(1:10000, 10000*0.2)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  pasyn = cbind(asset1[,2], asset2[,2])
  tasyn = cbind(asset1[,1], asset2[,1]) 
  
  HY_lam.5[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM_lam.5[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  HY_lam.5_asyn[i] = ftcorr(pasyn, tasyn, "HY", F)$Correlation[1,2]
  MM_lam.5_asyn[i] = ftcorr(pasyn, tasyn, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
## Variance Gamma

HY_lam_pure = rep(NA, Slength)
MM_lam_pure = rep(NA, Slength)
HY_lam_pure_asyn = rep(NA, Slength)
MM_lam_pure_asyn = rep(NA, Slength)

mu = c(0.01/86400, 0.01/86400)
s1 = 0.1/86400
s2 = 0.2/86400
b = c(1, 1)
startprice = c(100, 100)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = variance_gamma(10000, mu, sigma, b, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  
  set.seed(seed[i])
  rm1 = sample(1:10000, 10000*0.2)
  set.seed(seed[i] + Slength)
  rm2 = sample(1:10000, 10000*0.2)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  pasyn = cbind(asset1[,2], asset2[,2])
  tasyn = cbind(asset1[,1], asset2[,1]) 
  
  HY_lam_pure[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM_lam_pure[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  HY_lam_pure_asyn[i] = ftcorr(pasyn, tasyn, "HY", F)$Correlation[1,2]
  MM_lam_pure_asyn[i] = ftcorr(pasyn, tasyn, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}


#---------------------------------------------------------------------------
#### Plots

## lambda = 0
lam0 = data.frame(
  Induced = correlation,
  MMSyn = MM_lam0,
  HYSyn = HY_lam0,
  MMAsyn = MM_lam0_asyn,
  HYAsyn = HY_lam0_asyn,
  simulation = 1:Slength
)

lam0_long = melt(lam0, id="simulation")
names(lam0_long) = c("simulation", "Method" ,  "correlation")

ggplot(data=lam0_long,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash", "longdash", "dashed")) +
  labs(title = TeX('(a) $\\lambda = 0$'))+
  scale_color_manual(values=c('black', 'blue', 'red', 'purple', 'orange'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


## lambda = 0.2
lam1 = data.frame(
  Induced = correlation,
  MMSyn = MM_lam.2,
  HYSyn = HY_lam.2,
  MMAsyn = MM_lam.2_asyn,
  HYAsyn = HY_lam.2_asyn,
  simulation = 1:Slength
)

lam1_long = melt(lam1, id="simulation")
names(lam1_long) = c("simulation", "Method" ,  "correlation")

ggplot(data=lam1_long,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash", "longdash", "dashed")) +
  labs(title = TeX('(b) $\\lambda = 0.2$'))+
  scale_color_manual(values=c('black', 'blue', 'red', 'purple', 'orange'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


## lambda = 0.5
lam5 = data.frame(
  Induced = correlation,
  MMSyn = MM_lam.5,
  HYSyn = HY_lam.5,
  MMAsyn = MM_lam.5_asyn,
  HYAsyn = HY_lam.5_asyn,
  simulation = 1:Slength
)

lam5_long = melt(lam5, id="simulation")
names(lam5_long) = c("simulation", "Method" ,  "correlation")

ggplot(data=lam5_long,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash", "longdash", "dashed")) +
  labs(title =  TeX('(c) $\\lambda = 0.5$'))+
  scale_color_manual(values=c('black', 'blue', 'red', 'purple', 'orange'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


## pure jump
lam_pure = data.frame(
  Induced = correlation,
  MMSyn = MM_lam_pure,
  HYSyn = HY_lam_pure,
  MMAsyn = MM_lam_pure_asyn,
  HYAsyn = HY_lam_pure_asyn,
  simulation = 1:Slength
)

lam_pure_long = melt(lam_pure, id="simulation")
names(lam_pure_long) = c("simulation", "Method" ,  "correlation")

ggplot(data=lam_pure_long,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash", "longdash", "dashed")) +
  labs(title = "(d) Variance Gamma")+
  scale_color_manual(values=c('black', 'blue', 'red', 'purple', 'orange'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))
