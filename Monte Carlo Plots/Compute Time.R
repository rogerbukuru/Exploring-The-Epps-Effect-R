## Author: Patrick Chang
# Script file to obtain the distribution of 
# the compute time of MM Trig, Complex Exp and HY

#---------------------------------------------------------------------------

library(microbenchmark)
library(ggplot2)
library(latex2exp)
library(svMisc)
source("ftcorr.R")
source("Simulation Algorithms/Simulations.R")

#--------------------------------------------------------------------------
## Simulate the data first

# parameters
mu = c(0.01/86400, 0.01/86400)
s1 = 0.1/86400
s2 = 0.2/86400
sigma = matrix(c(s1, sqrt(s1*s2)*0.35,
                 sqrt(s1*s2)*0.35, s2), 2, 2)
startprice = c(100, 100)

# synchronous
p = BM(500, mu, sigma, startprice, 1)
t = cbind(1:500, 1:500)

# asynchronous sampled from the synchronous with 
# exponential inter-arrival times
t1 = NULL
t1[1] = 0
k=2
while (t1[length(t1)] < 500) {
  t1[k] = t1[k-1] + rexp(1, 1/15)
  k=k+1
}

t2 = NULL
t2[1] = 0
k=2
while (t2[length(t2)] < 500) {
  t2[k] = t2[k-1] + rexp(1, 1/15)
  k=k+1
}

t1 = t1[-length(t1)]
t2 = t2[-length(t2)]

p1 = p[floor(t1)+1, 1]
p2 = p[floor(t2)+1, 2]

D = max(length(t1), length(t2)) - min(length(t1), length(t2))

if(length(t1) < length(t2)){
  t1 = c(t1, rep(NaN, D))
  p1 = c(p1, rep(NaN, D))
} else{
  t2 = c(t2, rep(NaN, D))
  p2 = c(p2, rep(NaN, D))
}

pasyn = cbind(p1, p2)
tasyn = cbind(t1, t2)

#--------------------------------------------------------------------------
# Synchronous Comparison 

mbm_syn = microbenchmark("MM Trig" = { 
                        res = ftcorr(p, t, "TrigFejer", F)
                      },
                      "MM Complex" = {
                        res = ftcorr(p, t, "ComplexExpFejer", F)
                      },
                      "HY" = {
                        res = ftcorr(p, t, "HY", F)
                      },
                      times = 100)

autoplot(mbm_syn) + 
  labs(title = "(a) Synchronous Run Time") +
  theme(axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))

#--------------------------------------------------------------------------
# Asynchronous Comparison 

mbm_asyn = microbenchmark("MM Trig" = { 
  res = ftcorr(pasyn, tasyn, "TrigFejer", F)
},
"MM Complex" = {
  res = ftcorr(pasyn, tasyn, "ComplexExpFejer", F)
},
"HY" = {
  res = ftcorr(pasyn, tasyn, "HY", F)
},
times = 100)

autoplot(mbm_asyn) + 
  labs(title = "(b) Asynchronous Run Time") +
  theme(axis.text.y = element_text(face="bold", size=10),
        axis.text.x = element_text(face="bold", size=10, angle = 0),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))
