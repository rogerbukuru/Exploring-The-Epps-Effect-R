## Author: Patrick Chang
# Scipt file to recover the plots from Reno (2001)

# We recover the results with the ComplexExpFejer
# rather than the Trig - what Reno did

#--------------------------------------------------------------------------

library(ggplot2)
library(latex2exp)
library(svMisc)
source("Estimators/ftcorr-Reno.R")
source("Monte Carlo Simulation Algorithms/Simulations.R")

#--------------------------------------------------------------------------

## Recovering Reno with GARCH(1,1) specificed by Anderson

#--------------------------------- Garch (1,1) ----------------------------

N = 10000
M = seq(10, 160, length.out = 16)

RenoGarchMM = matrix(NA, N, length(M))
RenoGarchMMSyn = matrix(NA, N, length(M))

# parameters
theta = c(0.035, 0.054)
lambda = c(0.296, 0.48)
w = c(0.636, 0.476)
starting = c(0.5, 0.6)

#t = cbind(1:86400, 1:86400)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  P = garch(86400, theta, lambda, w, 0.35, starting, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 86400) {
    t1[k] = t1[k-1] + rexp(1, 1/15)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 86400) {
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

  for (j in 1:length(M)){
    RenoGarchMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    RenoGarchMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}

#--------------------------------------------------------------------------

## Recovering Reno with GARCH(1,1) specificed by Reno

#--------------------------------- Garch (1,1) ----------------------------

N = 10000
M = seq(10, 160, length.out = 16)

RenoActualGarchMM = matrix(NA, N, length(M))
RenoActualGarchMMSyn = matrix(NA, N, length(M))

# parameters
theta = c(0.035, 0.054)
lambda = c(0.296, 0.48)
w = c(0.636, 0.476)
starting = c(0.5, 0.6)

#t = cbind(1:86400, 1:86400)

for (i in 1:N){
  progress(i, progress.bar = TRUE, char = "-")
  
  P = garch_reno(86400, theta, lambda, w, 0.35, starting, i)
  
  #--------------------------------------------------
  # Asynchronous Sample Times
  t1 = NULL
  t1[1] = 0
  k=2
  while (t1[length(t1)] < 86400) {
    t1[k] = t1[k-1] + rexp(1, 1/15)
    k=k+1
  }
  
  t2 = NULL
  t2[1] = 0
  k=2
  while (t2[length(t2)] < 86400) {
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

  for (j in 1:length(M)){
    RenoActualGarchMM[i, j] = ftcorrReno(p, time, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
    RenoActualGarchMMSyn[i, j] = ftcorrReno(psyn, tsyn, "ComplexExpFejer", F, M[j], log = T)$Correlation[1,2]
  }
  if (i == N) cat("Done!\n")
}


#--------------------------------------------------------------------------
#### Plots

## Andersen specification
RenoGarch = data.frame(K = M,
                       Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                       Means = c(colMeans(RenoGarchMM), colMeans(RenoGarchMMSyn)),
                       Std = c(rep(0, 14), apply(RenoGarchMM, 2, sd)[15:16], rep(0, 14), apply(RenoGarchMMSyn, 2, sd)[15:16]))

RenoGarchlines = data.frame(yintercept = c(0.35),
                            HY = c("Induced"))


ggplot(RenoGarch, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), RenoGarchlines) +
  theme_bw()+ 
  labs(title = "(b) GARCH (1,1) - Andersen")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))


## Reno specification
RenoActualGarch = data.frame(K = M,
                             Method = c(rep("MM Asyn", 16), rep("MM Syn", 16)),
                             Means = c(colMeans(RenoActualGarchMM), colMeans(RenoActualGarchMMSyn)),
                             Std = c(rep(0, 14), apply(RenoActualGarchMM, 2, sd)[15:16], rep(0, 14), apply(RenoActualGarchMMSyn, 2, sd)[15:16]))

RenoActualGarchLines = data.frame(yintercept = c(0.35),
                                  HY = c("Induced"))

ggplot(RenoActualGarch, aes(x=K, y=Means, group=Method, linetype = Method, colour = Method)) + 
  geom_line() +
  geom_point() +
  labs(y = TeX("\\textbf{Correlation} ($\\rho$)"), x = TeX("\\textbf{Max Fourier Coefficients} (N)")) +
  geom_hline(aes(yintercept = yintercept, linetype = HY, colour = HY), RenoGarchlines) +
  theme_bw()+ 
  labs(title = "(a) GARCH (1,1) - Reno")+
  theme(legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))