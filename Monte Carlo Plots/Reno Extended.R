## Author: Patrick Chang
# Scipt file to extend the results from Reno (2001)
# This script file produces the plot Effect of asynchrony

#--------------------------------------------------------------------------

library(ggplot2)
library(latex2exp)
library(svMisc)
source("Estimators/ftcorr-Reno.R")
source("Monte Carlo Simulation Algorithms/Simulations.R")

#--------------------------------------------------------------------------
## Andersen Boller Specification of GARCH (1,1)
N = 1000
M = seq(10, 160, length.out = 16)

GeoGarchMM = matrix(NA, N, length(M))
GeoGarchHY = matrix(NA, N, 1)
GeoGarchMMSyn = matrix(NA, N, length(M))
GeoGarchHYSyn = matrix(NA, N, 1)

# parameters
theta = c(0.035, 0.054)
lambda = c(0.296, 0.48)
w = c(0.636, 0.476)
starting = c(0.5, 0.6)

t = cbind(1:10000, 1:10000)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  
  P = garch(10000, theta, lambda, w, 0.35, starting, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 10000) {
    t1[k] = t1[k-1] + rexp(1, 1/30)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 10000) {
    t2[k] = t2[k-1] + rexp(1, 1/45)
    k=k+1
  }
  
  t1 = t1[-length(t1)]
  t2 = t2[-length(t2)]
  
  p1 = P[floor(t1)+1, 1]
  p2 = P[floor(t2)+1, 2]
  
  #--------------------------------------------------
  # Syncrhonous Sampling Times
  psyn = P[floor(t2)+1, ]
  tsyn = cbind(t2, t2)
  
  D = max(length(t1), length(t2)) - min(length(t1), length(t2))
  
  if(length(t1) < length(t2)){
    t1 = c(t1, rep(NaN, D))
    p1 = c(p1, rep(NaN, D))
  } else{
    t2 = c(t2, rep(NaN, D))
    p2 = c(p2, rep(NaN, D))
  }
  
  p = cbind(p1, p2)
  time = cbind(t1, t2)
  
  GeoGarchHY[i] = ftcorrReno(p, time, "HY", F, 100, log = T)$Correlation[1,2]
  GeoGarchHYSyn[i] = ftcorrReno(psyn, tsyn, "HY", F, 100, log = T)$Correlation[1,2]
  
  for (j in 1:length(M)){
    GeoGarchMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    GeoGarchMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}


#--------------------------------------------------------------------------
## Reno Specification of GARCH (1,1)

N = 1000
M = seq(10, 160, length.out = 16)

GarchMM = matrix(NA, N, length(M))
GarchHY = matrix(NA, N, 1)
GarchMMSyn = matrix(NA, N, length(M))
GarchHYSyn = matrix(NA, N, 1)

# parameters
theta = c(0.035, 0.054)
lambda = c(0.296, 0.48)
w = c(0.636, 0.476)
starting = c(0.5, 0.6)

t = cbind(1:10000, 1:10000)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  
  P = garch_reno(10000, theta, lambda, w, 0.35, starting, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 10000) {
    t1[k] = t1[k-1] + rexp(1, 1/30)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 10000) {
    t2[k] = t2[k-1] + rexp(1, 1/45)
    k=k+1
  }
  
  t1 = t1[-length(t1)]
  t2 = t2[-length(t2)]
  
  p1 = P[floor(t1)+1, 1]
  p2 = P[floor(t2)+1, 2]
  
  #--------------------------------------------------
  # Syncrhonous Sampling Times
  psyn = P[floor(t2)+1, ]
  tsyn = cbind(t2, t2)
  
  D = max(length(t1), length(t2)) - min(length(t1), length(t2))
  
  if(length(t1) < length(t2)){
    t1 = c(t1, rep(NaN, D))
    p1 = c(p1, rep(NaN, D))
  } else{
    t2 = c(t2, rep(NaN, D))
    p2 = c(p2, rep(NaN, D))
  }
  
  p = cbind(p1, p2)
  time = cbind(t1, t2)
  
  GarchHY[i] = ftcorrReno(p, time, "HY", F, 100, log = T)$Correlation[1,2]
  GarchHYSyn[i] = ftcorrReno(psyn, tsyn, "HY", F, 100, log = T)$Correlation[1,2]
  
  for (j in 1:length(M)){
    GarchMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    GarchMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}

#--------------------------------------------------------------------------
## GBM

N = 1000
M = seq(10, 160, length.out = 16)

GBMMM = matrix(NA, N, length(M))
GBMHY = matrix(NA, N, 1)
GBMMMSyn = matrix(NA, N, length(M))
GBMHYSyn = matrix(NA, N, 1)

# parameters
mu = c(0.01/86400, 0.01/86400)
s1 = 0.1/86400
s2 = 0.2/86400
sigma = matrix(c(s1, sqrt(s1*s2)*0.35,
                 sqrt(s1*s2)*0.35, s2), 2, 2)
startprice = c(100, 100)

t = cbind(1:10000, 1:10000)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  
  P = BM(10000, mu, sigma, startprice, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 10000) {
    t1[k] = t1[k-1] + rexp(1, 1/30)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 10000) {
    t2[k] = t2[k-1] + rexp(1, 1/45)
    k=k+1
  }
  
  t1 = t1[-length(t1)]
  t2 = t2[-length(t2)]
  
  p1 = P[floor(t1)+1, 1]
  p2 = P[floor(t2)+1, 2]
  
  #--------------------------------------------------
  # Syncrhonous Sampling Times
  psyn = P[floor(t2)+1, ]
  tsyn = cbind(t2, t2)
  
  D = max(length(t1), length(t2)) - min(length(t1), length(t2))
  
  if(length(t1) < length(t2)){
    t1 = c(t1, rep(NaN, D))
    p1 = c(p1, rep(NaN, D))
  } else{
    t2 = c(t2, rep(NaN, D))
    p2 = c(p2, rep(NaN, D))
  }
  
  p = cbind(p1, p2)
  time = cbind(t1, t2)
  
  GBMHY[i] = ftcorrReno(p, time, "HY", F, 100, log = T)$Correlation[1,2]
  GBMHYSyn[i] = ftcorrReno(psyn, tsyn, "HY", F, 100, log = T)$Correlation[1,2]
  
  for (j in 1:length(M)){
    GBMMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    GBMMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}

#--------------------------------------------------------------------------
## Merton Model

N = 1000
M = seq(10, 160, length.out = 16)

MertMM = matrix(NA, N, length(M))
MertHY = matrix(NA, N, 1)
MertMMSyn = matrix(NA, N, length(M))
MertHYSyn = matrix(NA, N, 1)

# parameters
mu = c(0.01/86400, 0.01/86400)
s1 = 0.1/86400
s2 = 0.2/86400
sigma = matrix(c(s1, sqrt(s1*s2)*0.35,
                 sqrt(s1*s2)*0.35, s2), 2, 2)
a = c(0,0)
b = c(100/86400, 100/86400)
lambda = c(0.2, 0.2)
startprice = c(100, 100)

t = cbind(1:10000, 1:10000)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  
  P = Merton(10000, mu, sigma, lambda, a, b, startprice, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 10000) {
    t1[k] = t1[k-1] + rexp(1, 1/30)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 10000) {
    t2[k] = t2[k-1] + rexp(1, 1/45)
    k=k+1
  }
  
  t1 = t1[-length(t1)]
  t2 = t2[-length(t2)]
  
  p1 = P[floor(t1)+1, 1]
  p2 = P[floor(t2)+1, 2]
  
  #--------------------------------------------------
  # Syncrhonous Sampling Times
  psyn = P[floor(t2)+1, ]
  tsyn = cbind(t2, t2)
  
  D = max(length(t1), length(t2)) - min(length(t1), length(t2))
  
  if(length(t1) < length(t2)){
    t1 = c(t1, rep(NaN, D))
    p1 = c(p1, rep(NaN, D))
  } else{
    t2 = c(t2, rep(NaN, D))
    p2 = c(p2, rep(NaN, D))
  }
  
  p = cbind(p1, p2)
  time = cbind(t1, t2)
  
  MertHY[i] = ftcorrReno(p, time, "HY", F, 100, log = T)$Correlation[1,2]
  MertHYSyn[i] = ftcorrReno(psyn, tsyn, "HY", F, 100, log = T)$Correlation[1,2]
  
  for (j in 1:length(M)){
    MertMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    MertMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}

#---------------------------------------------------------------------
# Variance Gamma

N = 1000
M = seq(10, 160, length.out = 16)

VGMM = matrix(NA, N, length(M))
VGHY = matrix(NA, N, 1)
VGMMSyn = matrix(NA, N, length(M))
VGHYSyn = matrix(NA, N, 1)

# parameters
mu = c(0.01/86400, 0.01/86400)
s1 = 0.1/86400
s2 = 0.2/86400
sigma = matrix(c(s1, sqrt(s1*s2)*0.35,
                 sqrt(s1*s2)*0.35, s2), 2, 2)
b = c(1, 1)
startprice = c(100, 100)

t = cbind(1:10000, 1:10000)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  
  P = variance_gamma(10000, mu, sigma, b, startprice, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 10000) {
    t1[k] = t1[k-1] + rexp(1, 1/30)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 10000) {
    t2[k] = t2[k-1] + rexp(1, 1/45)
    k=k+1
  }
  
  t1 = t1[-length(t1)]
  t2 = t2[-length(t2)]
  
  p1 = P[floor(t1)+1, 1]
  p2 = P[floor(t2)+1, 2]
  
  #--------------------------------------------------
  # Syncrhonous Sampling Times
  psyn = P[floor(t2)+1, ]
  tsyn = cbind(t2, t2)
  
  D = max(length(t1), length(t2)) - min(length(t1), length(t2))
  
  if(length(t1) < length(t2)){
    t1 = c(t1, rep(NaN, D))
    p1 = c(p1, rep(NaN, D))
  } else{
    t2 = c(t2, rep(NaN, D))
    p2 = c(p2, rep(NaN, D))
  }
  
  p = cbind(p1, p2)
  time = cbind(t1, t2)
  
  VGHY[i] = ftcorrReno(p, time, "HY", F, 100, log = T)$Correlation[1,2]
  VGHYSyn[i] = ftcorrReno(psyn, tsyn, "HY", F, 100, log = T)$Correlation[1,2]
  
  for (j in 1:length(M)){
    VGMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    VGMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}

#---------------------------------------------------------------------
# Ornstein Uhlenbeck

N = 1000
M = seq(10, 160, length.out = 16)

OUMM = matrix(NA, N, length(M))
OUHY = matrix(NA, N, 1)
OUMMSyn = matrix(NA, N, length(M))
OUHYSyn = matrix(NA, N, 1)

# parameters
mu = c(100, 100)
s1 = 0.1/86400
s2 = 0.2/86400
sigma = matrix(c(s1, sqrt(s1*s2)*0.35,
                 sqrt(s1*s2)*0.35, s2), 2, 2)
theta = c(0.035, 0.054)
startprice = c(100, 100)

t = cbind(1:10000, 1:10000)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  
  P = OU(10000, mu, sigma, theta, startprice, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 10000) {
    t1[k] = t1[k-1] + rexp(1, 1/30)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 10000) {
    t2[k] = t2[k-1] + rexp(1, 1/45)
    k=k+1
  }
  
  t1 = t1[-length(t1)]
  t2 = t2[-length(t2)]
  
  p1 = P[floor(t1)+1, 1]
  p2 = P[floor(t2)+1, 2]
  
  #--------------------------------------------------
  # Syncrhonous Sampling Times
  psyn = P[floor(t2)+1, ]
  tsyn = cbind(t2, t2)
  
  D = max(length(t1), length(t2)) - min(length(t1), length(t2))
  
  if(length(t1) < length(t2)){
    t1 = c(t1, rep(NaN, D))
    p1 = c(p1, rep(NaN, D))
  } else{
    t2 = c(t2, rep(NaN, D))
    p2 = c(p2, rep(NaN, D))
  }
  
  p = cbind(p1, p2)
  time = cbind(t1, t2)
  
  OUHY[i] = ftcorrReno(p, time, "HY", F, 100, log = T)$Correlation[1,2]
  OUHYSyn[i] = ftcorrReno(psyn, tsyn, "HY", F, 100, log = T)$Correlation[1,2]
  
  for (j in 1:length(M)){
    OUMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    OUMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}


###########################################################################
#### Plots

Nyquist = 1/(30*2) * 10000 /(2*pi)

#------------------------------------------------------------------------------------------------
## Garch (1,1) - Reno plot
GarchR = data.frame(K = M,
                    Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                    Means = c(colMeans(GarchMM), colMeans(GarchMMSyn)),
                    Std = c(apply(GarchMM, 2, sd), apply(GarchMMSyn, 2, sd)))

GarchRlines = data.frame(yintercept = c(colMeans(GarchHY), colMeans(GarchHYSyn), 0.35),
                         HY = c("HY Asyn", "HY Syn", "Induced"))

StdHY = apply(GarchHY, 2, sd)
StdHYSyn = apply(GarchHYSyn, 2, sd)

ggplot(GarchR, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), GarchRlines) +
  geom_vline(xintercept = Nyquist, linetype = "dashed") +
  annotate("text", x = Nyquist + 25, y = 0.15, label = "Nyquist Frequency", size = 3) + 
  theme_bw()+ 
  labs(title = "(a) GARCH (1,1) - Reno")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+
  geom_errorbar(aes(ymin=Means-Std, ymax=Means+Std), width=.2,
                position=position_dodge(0.05))+
  geom_linerange(x = 160, ymin = GarchRlines[1, 1] - StdHY, ymax = GarchRlines[1, 1] + StdHY, col = "orange", show.legend = FALSE) + 
  geom_linerange(x = 150, ymin = GarchRlines[2, 1] - StdHYSyn, ymax = GarchRlines[2, 1] + StdHYSyn, col = "yellowgreen", show.legend = FALSE) 


#------------------------------------------------------------------------------------------------
## Garch (1,1) - Andersen
GeoGarch = data.frame(K = M,
                      Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                      Means = c(colMeans(GeoGarchMM), colMeans(GeoGarchMMSyn)),
                      Std = c(apply(GeoGarchMM, 2, sd), apply(GeoGarchMMSyn, 2, sd)))

GeoGarchlines = data.frame(yintercept = c(colMeans(GeoGarchHY), colMeans(GeoGarchHYSyn), 0.35),
                           HY = c("HY Asyn", "HY Syn", "Induced"))

StdHY = apply(GeoGarchHY, 2, sd)
StdHYSyn = apply(GeoGarchHYSyn, 2, sd)


ggplot(GeoGarch, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), GeoGarchlines) +
  geom_vline(xintercept = Nyquist, linetype = "dashed") +
  annotate("text", x = Nyquist + 25, y = 0.15, label = "Nyquist Frequency", size = 3) + 
  theme_bw()+ 
  labs(title = "(b) GARCH (1,1) - Anderson")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+
  geom_errorbar(aes(ymin=Means-Std, ymax=Means+Std), width=.2,
                position=position_dodge(0.05))+
  geom_linerange(x = 160, ymin = GeoGarchlines[1, 1] - StdHY, ymax = GeoGarchlines[1, 1] + StdHY, col = "orange", show.legend = FALSE) + 
  geom_linerange(x = 150, ymin = GeoGarchlines[2, 1] - StdHYSyn, ymax = GeoGarchlines[2, 1] + StdHYSyn, col = "yellowgreen", show.legend = FALSE) 

#------------------------------------------------------------------------------------------------
## GBM
GBM = data.frame(K = M,
                 Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                 Means = c(colMeans(GBMMM), colMeans(GBMMMSyn)),
                 Std = c(apply(GBMMM, 2, sd), apply(GBMMMSyn, 2, sd)))

GBMlines = data.frame(yintercept = c(colMeans(GBMHY), colMeans(GBMHYSyn), 0.35),
                      HY = c("HY Asyn", "HY Syn", "Induced"))

StdHY = apply(GBMHY, 2, sd)
StdHYSyn = apply(GBMHYSyn, 2, sd)

ggplot(GBM, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), GBMlines) +
  geom_vline(xintercept = Nyquist, linetype = "dashed") +
  annotate("text", x = Nyquist + 25, y = 0.15, label = "Nyquist Frequency", size = 3) + 
  theme_bw()+ 
  labs(title = "(c) GBM")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+
  geom_errorbar(aes(ymin=Means-Std, ymax=Means+Std), width=.2,
                position=position_dodge(0.05))+
  geom_linerange(x = 160, ymin = GBMlines[1, 1] - StdHY, ymax = GBMlines[1, 1] + StdHY, col = "orange", show.legend = FALSE) + 
  geom_linerange(x = 150, ymin = GBMlines[2, 1] - StdHYSyn, ymax = GBMlines[2, 1] + StdHYSyn, col = "yellowgreen", show.legend = FALSE) 


#------------------------------------------------------------------------------------------------
## Mert
Mert = data.frame(K = M,
                  Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                  Means = c(colMeans(MertMM), colMeans(MertMMSyn)),
                  Std = c(apply(MertMM, 2, sd), apply(MertMMSyn, 2, sd)))

Mertlines = data.frame(yintercept = c(colMeans(MertHY), colMeans(MertHYSyn), 0.35),
                       HY = c("HY Asyn", "HY Syn", "Induced"))

StdHY = apply(MertHY, 2, sd)
StdHYSyn = apply(MertHYSyn, 2, sd)

ggplot(Mert, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), Mertlines) +
  geom_vline(xintercept = Nyquist, linetype = "dashed") +
  annotate("text", x = Nyquist + 25, y = 0.1, label = "Nyquist Frequency", size = 3) + 
  theme_bw()+ 
  labs(title = "(d) Merton Model")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+
  geom_errorbar(aes(ymin=Means-Std, ymax=Means+Std), width=.2,
                position=position_dodge(0.05))+
  geom_linerange(x = 160, ymin = Mertlines[1, 1] - StdHY, ymax = Mertlines[1, 1] + StdHY, col = "orange", show.legend = FALSE) + 
  geom_linerange(x = 150, ymin = Mertlines[2, 1] - StdHYSyn, ymax = Mertlines[2, 1] + StdHYSyn, col = "yellowgreen", show.legend = FALSE) 


#------------------------------------------------------------------------------------------------
## VG
VG = data.frame(K = M,
                Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                Means = c(colMeans(VGMM), colMeans(VGMMSyn)),
                Std = c(apply(VGMM, 2, sd), apply(VGMMSyn, 2, sd)))

VGlines = data.frame(yintercept = c(colMeans(VGHY), colMeans(VGHYSyn), 0.35),
                     HY = c("HY Asyn", "HY Syn", "Induced"))

StdHY = apply(VGHY, 2, sd)
StdHYSyn = apply(VGHYSyn, 2, sd)

ggplot(VG, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), VGlines) +
  geom_vline(xintercept = Nyquist, linetype = "dashed") +
  annotate("text", x = Nyquist + 25, y = 0.2, label = "Nyquist Frequency", size = 3) + 
  theme_bw()+ 
  labs(title = "(e) Variance Gamma")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+
  geom_errorbar(aes(ymin=Means-Std, ymax=Means+Std), width=.2,
                position=position_dodge(0.05))+
  geom_linerange(x = 160, ymin = VGlines[1, 1] - StdHY, ymax = VGlines[1, 1] + StdHY, col = "orange", show.legend = FALSE) + 
  geom_linerange(x = 150, ymin = VGlines[2, 1] - StdHYSyn, ymax = VGlines[2, 1] + StdHYSyn, col = "yellowgreen", show.legend = FALSE) 


#------------------------------------------------------------------------------------------------
## OU
OU = data.frame(K = M,
                Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                Means = c(colMeans(OUMM), colMeans(OUMMSyn)),
                Std = c(apply(OUMM, 2, sd), apply(OUMMSyn, 2, sd)))

OUlines = data.frame(yintercept = c(colMeans(OUHY), colMeans(OUHYSyn), 0.35),
                     HY = c("HY Asyn", "HY Syn", "Induced"))

StdHY = apply(OUHY, 2, sd)
StdHYSyn = apply(OUHYSyn, 2, sd)

ggplot(OU, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), OUlines) +
  geom_vline(xintercept = Nyquist, linetype = "dashed") +
  annotate("text", x = Nyquist + 25, y = 0, label = "Nyquist Frequency", size = 3) + 
  theme_bw()+ 
  labs(title = "(f) Ornstein Uhlenbeck")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+
  geom_errorbar(aes(ymin=Means-Std, ymax=Means+Std), width=.2,
                position=position_dodge(0.05))+
  geom_linerange(x = 160, ymin = OUlines[1, 1] - StdHY, ymax = OUlines[1, 1] + StdHY, col = "orange", show.legend = FALSE) + 
  geom_linerange(x = 150, ymin = OUlines[2, 1] - StdHYSyn, ymax = OUlines[2, 1] + StdHYSyn, col = "yellowgreen", show.legend = FALSE) 


