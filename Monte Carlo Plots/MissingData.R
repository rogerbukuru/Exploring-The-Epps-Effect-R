## Author: Patrick Chang
# Scipt file to recover the plot for 
# "The effect of missing data"

#---------------------------------------------------------------------------

library(ggplot2)
library(latex2exp)
library(svMisc)
source("ftcorr.R")
source("Monte Carlo Simulation Algorithms/Simulations.R")

#---------------------------------------------------------------------------
## Case with no missing data

Slength = 21
correlation = seq(from = -0.99, to = 0.99, length.out = Slength)
# be careful, cant set final value = 1, 
# otherwise cholesky will produce NaN or matrix wont be positive definite

HY = rep(NA, Slength)
MM = rep(NA, Slength)

mu = c(0.01/86400, 0.01/86400)
s1 = 0.1/86400
s2 = 0.2/86400
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
  
  p = BM(10000, mu, sigma, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  
  HY[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
## Case with 10% missing data

HY.1 = rep(NA, Slength)
MM.1 = rep(NA, Slength)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = BM(10000, mu, sigma, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  set.seed(seed[i])
  rm1 = sample(1:10000, 10000*0.1)
  set.seed(seed[i] + Slength)
  rm2 = sample(1:10000, 10000*0.1)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  p = cbind(asset1[,2], asset2[,2])
  t = cbind(asset1[,1], asset2[,1])
  
  HY.1[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM.1[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
## Case with 20% missing data

HY.2 = rep(NA, Slength)
MM.2 = rep(NA, Slength)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = BM(10000, mu, sigma, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  set.seed(seed[i])
  rm1 = sample(1:10000, 10000*0.2)
  set.seed(seed[i] + Slength)
  rm2 = sample(1:10000, 10000*0.2)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  p = cbind(asset1[,2], asset2[,2])
  t = cbind(asset1[,1], asset2[,1])
  
  HY.2[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM.2[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
## Case with 40% missing data

HY.4 = rep(NA, Slength)
MM.4 = rep(NA, Slength)

for (i in 1:Slength) {
  progress(i, progress.bar = TRUE, char = "-")
  
  covariance = correlation[i] * sqrt(s1) * sqrt(s2)
  sigma = matrix(NA, 2, 2)
  sigma[1, 1] = s1
  sigma[2, 2] = s2
  sigma[1, 2] = covariance
  sigma[2, 1] = covariance
  
  p = BM(10000, mu, sigma, startprice, seed[i])
  t = cbind(1:10000, 1:10000)
  set.seed(seed[i])
  rm1 = sample(1:10000, 10000*0.4)
  set.seed(seed[i] + Slength)
  rm2 = sample(1:10000, 10000*0.4)
  asset1 = cbind(t[,1], p[,1])
  asset2 = cbind(t[,2], p[,2])
  asset1[rm1,] = NaN
  asset2[rm2,] = NaN
  p = cbind(asset1[,2], asset2[,2])
  t = cbind(asset1[,1], asset2[,1])
  
  HY.4[i] = ftcorr(p, t, "HY", F)$Correlation[1,2]
  MM.4[i] = ftcorr(p, t, "ComplexExpFejer", F)$Correlation[1,2]
  
  if (i == Slength) cat("Done!\n")
}

#---------------------------------------------------------------------------
#### Plots

## Synchronous
synchronous = data.frame(
  Induced = correlation,
  MM = MM,
  HY = HY,
  simulation = 1:Slength
)

synchronous_long = melt(synchronous, id="simulation")
names(synchronous_long) = c("simulation", "Method" ,  "correlation")

ggplot(data=synchronous_long,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash")) +
  labs(title = "(a) Synchronous")+
  scale_color_manual(values=c('black','blue', 'red'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


## Asynchronous (10%)
asynchronous.1 = data.frame(
  Induced = correlation,
  MM = MM.1,
  HY = HY.1,
  simulation = 1:Slength
)

asynchronous_long.1 = melt(asynchronous.1, id="simulation")
names(asynchronous_long.1) = c("simulation", "Method" ,  "correlation")

ggplot(data=asynchronous_long.1,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash")) +
  labs(title = "(b) Asynchronous (10%) Missing")+
  scale_color_manual(values=c('black','blue', 'red'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


## Asynchronous (20%)
asynchronous.2 = data.frame(
  Induced = correlation,
  MM = MM.2,
  HY = HY.2,
  simulation = 1:Slength
)

asynchronous_long.2 = melt(asynchronous.2, id="simulation")
names(asynchronous_long.2) = c("simulation", "Method" ,  "correlation")

ggplot(data=asynchronous_long.2,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash")) +
  labs(title = "(c) Asynchronous (20%) Missing")+
  scale_color_manual(values=c('black','blue', 'red'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


## Asynchronous (40%)
asynchronous.4 = data.frame(
  Induced = correlation,
  MM = MM.4,
  HY = HY.4,
  simulation = 1:Slength
)

asynchronous_long.4 = melt(asynchronous.4, id="simulation")
names(asynchronous_long.4) = c("simulation", "Method" ,  "correlation")

ggplot(data=asynchronous_long.4,
       aes(x=simulation, y=correlation, colour=Method, linetype = Method)) +
  geom_line() + theme_bw()+ labs(y = TeX("\\textbf{correlation} ($\\rho$)")) +
  scale_fill_discrete(name = "Method") + 
  scale_linetype_manual(values=c("dotted", "twodash", "dotdash")) +
  labs(title = "(d) Asynchronous (40%) Missing")+
  scale_color_manual(values=c('black','blue', 'red'))+
  theme(legend.key.size = unit(1.1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


