## Author: Patrick Chang
# Script file for the MM Complex Fourier Transform and Hayashi Yoshida
# designed to specifically recover Reno's result;
# more dynamic function allowing the option to specify the 
# number of Fourier coefficients and whether price inputs need to be logged

# Supporting Algorithms are at the end of the script
#  Include:
#           - Rcpp code to increase computation time for MM Complex FT
#           - Scale function to re-scale time to [0, 2 \pi]
# The Kanatani Weight matrix is included inside the HY computation

#---------------------------------------------------------------------------

library(Rcpp)
library(RcppArmadillo)
library(pracma)
library(DescTools)
library(matlab)

#---------------------------------------------------------------------------

### Data Format:
## p = [n x 2] matrix of prices, log returns are computed in the function
# non-trading times are indicated by NaNs
## t = [n x 2] matrix of trading times, non-trading times are indicated by NaNs
# dimensions of p and t must match.
## fourierMethod = determines the algorithm to be used
#   Include: 
#            CompelxExpFejer - MM Complex Fourier Transform
#            HY - HY estimator
## onlyOverlapping = logical input to determine if only synchronous samples
# are kept or if data should be kept asynchronous
## M = N in Malliavin Mancino (2009), the number of Fourier coefficients
# to be used
## log = TRUE => p is price inputs
#  log = FALSE => p is logged price inputs

## NB: the integrated covariance for HY is not scaled correctly
# currently N0 is not [0, T], therefore scaling is wrong
# correlation however is fine

#---------------------------------------------------------------------------

ftcorrReno = function(p, t, fourierMethod, onlyOverlapping, M, log) {
  ## preallocate arrays and check data
  np = dim(p)[1]
  mp = dim(p)[2]
  nt = dim(t)[1]
  if (nt != np){
    stop("Incorrect Data")
  } 
  
  ## rescale domain to [0, PI]
  tau = scale.tau(t)
  
  # --------------------------------------------------------------------------------
  
  if (log) {
    p = log(p)
  }
  
  # --------------------------------------------------------------------------------
  
  if (onlyOverlapping) {
    idNanTau = matrix(0, dim(tau)[1], dim(tau)[2])
    # identify missing times (indicated as nan)
    idNanTau = is.nan(tau) * 1
    # eliminate times and prices associated with non-overlapping times
    tau = repmat(cbind(tau[idNanTau[,1]==0 & idNanTau[,2]==0, 1]), 1, 2)
    price1 = p[,1]
    price2 = p[,2]
    price1 = price1[idNanTau[,1]==0 & idNanTau[,2]==0]
    price2 = price2[idNanTau[,1]==0 & idNanTau[,2]==0]
    p = cbind(price1, price2)
  }
  
  # compute the average minimum time change
  dtau = cbind(diff(tau[,1]), diff(tau[,2]))
  dtau[dtau==0] = NaN
  
  taumin = min(dtau, na.rm = TRUE)
  taumax = 2* pi
  N0 = M
  k = matrix(-M:M, 1, length(-M:M))
  
  # --------------------------------------------------------------------------------
  
  Sigma = switch(fourierMethod,
                 "ComplexExpFejer" = {
                   e_t_dp.PosComplexExpDiffPrice = matrix(0, mp, length(k))
                   e_t_dp.NegComplexExpDiffPrice = matrix(0, mp, length(k))
                   
                   for (i in 1:mp) {
                     # nonuniformly sampled data from sparse matrix
                     psii = which(p[ ,i] != 0)
                     end = length(psii)
                     psi = matrix(p[psii, i], end, 1)
                     # slice the data first
                     tsi = tau[ ,i]
                     # sampling times from the rescaled domain
                     tsi = matrix(tsi[psii], end, 1)
                     
                     P = psi # unevenly sampled prices on [0, pi]
                     Time = tsi  # unevenly sampled times
                     K = k   # wave numbers
                     
                     # initialise gpuArrays for calculations
                     E_t_dp.PosComplexExpDiffPrice = matrix(0, 1, length(k))
                     E_t_dp.NegComplexExpDiffPrice = matrix(0, 1, length(k))

                     DiffP = diff(P)    # diff isnt supported by gpuR
                     
                     # ------- Fourier Transform on inhomogeneously sampled data -------
                     
                     # create complex exponential coefficients, multiply with
                     # log-price differences
                     e_t_dp.PosComplexExpDiffPrice[i, ] = ComplexExp(DiffP, Time[2:end, ], K)
                     e_t_dp.NegComplexExpDiffPrice[i, ] = ComplexExp(DiffP, -Time[2:end, ], K)
                   }
                   Sigma = matrix(NA, mp, mp)
                   Sigma[1, 1] = 1 / (2*N0+1) * (sum(e_t_dp.PosComplexExpDiffPrice[1, ]
                                                     * e_t_dp.NegComplexExpDiffPrice[1, ]))
                   Sigma[1, 2] = 1 / (2*N0+1) * (sum(e_t_dp.PosComplexExpDiffPrice[1, ]
                                                     * e_t_dp.NegComplexExpDiffPrice[2, ]))
                   Sigma[2, 1] = Sigma[1, 2]
                   Sigma[2, 2] = 1 / (2*N0+1) * (sum(e_t_dp.PosComplexExpDiffPrice[2, ] 
                                                     * e_t_dp.NegComplexExpDiffPrice[2, ]))
                   Sigma
                 },
                 "HY" = {
                   
                   ## rescale domain to [0, PI]
                   tau = scale.tau(t)
                   
                   # -----------------------------------------------------------------
                   
                   idNanTau = is.nan(tau) * 1
                   # extract position of time with trades
                   t1 = tau[which(idNanTau[,1]==0), 1]
                   t2 = tau[which(idNanTau[,2]==0), 2]
                   # extract position of price with trades
                   price1 = p[,1]
                   price2 = p[,2]
                   price1 = price1[idNanTau[,1]==0]
                   price2 = price2[idNanTau[,2]==0]
                   
                   end1 = length(t1)
                   end2 = length(t2)
                   
                   price1 = matrix(diff(price1), end1-1, 1)
                   price2 = matrix(diff(price2), end2-1, 1)
                   
                   # -----------------------------------------------------------------
                   
                   # when i = j, it is simply QV
                   Sigma = matrix(NA, mp, mp)
                   Sigma[1, 1] = t(price1) %*% price1
                   Sigma[2, 2] = t(price2) %*% price2
                   # create W matrix using Kanatani weighted realised quadratic covariation
                   W = matrix(NA, end1-1, end2-1)
                   
                   for (i in 1:(end1-1)) {
                     for (j in 1:(end2-1)) {
                       if ( Overlap(c(t1[i], t1[i+1]),
                                    c(t2[j], t2[j+1]))) {
                         W[i, j] = 1
                       } else{
                         W[i, j] = 0
                       }
                     }
                   }
                   
                   Sigma[1, 2] = t(price1) %*% W %*% price2
                   Sigma[2, 1] = Sigma[1, 2]
                   Sigma
                 })
  Sigma = Re(Sigma)
  var = diag(Sigma)
  sigma = sqrt(var)
  rho = Re(Sigma / sigma %*% t(sigma))
  Sigma_rescaled = 1 / N0 * Sigma
  
  return(list(Correlation = rho,
              Integrated_Covariance = Sigma,
              Covariance = Sigma_rescaled,
              Variance = var,
              StdDev = sigma))
}



scale.tau = function(t) {
  maxt = max(t, na.rm = TRUE)
  mint = min(t, na.rm = TRUE)
  
  tau = 2 * pi * (t - mint) / (maxt - mint)
  return(tau)
}

cppFunction("arma::cx_rowvec ComplexExp(arma::rowvec R, arma::colvec T, arma::rowvec K) {
  std::complex<double> ii(0,1);
  return R * exp(T * K * ii);}",
            depends="RcppArmadillo")