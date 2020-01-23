## Author: Patrick Chang
# Script file for the MM Complex Fourier Transform and Hayashi Yoshida
# designed to compute correlation for the Real Data.
# This is a supporting function that computes the 2x2 correlation 
# for the multiple asset function


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
## p = [n x 2] log returns are computed in the function
# non-trading times are indicated by NaNs
## t = [n x 2] matrix of trading times, non-trading times are indicated by NaNs
# dimensions of p and t must match.
## fourierMethod = determines the algorithm to be used
#   Include: 
#            CompelxExpFejer - MM Complex Fourier Transform
#            HY - HY estimator
## onlyOverlapping = logical input to determine if only synchronous samples
# are kept or if data should be kept asynchronous (should rarely be used)

## NB: This function takes in logged-returns for prices
# this is because it is not practical to remove overnight returns
# inside this function; therefore from the raw data, prices are converted
# to logged returns and overnight returns are removed first 
# before passing it through this function

#---------------------------------------------------------------------------

ftcorr_supp = function(p, t, fourierMethod, onlyOverlapping) {
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
  dtau = c(diff(tau[!is.na(tau[,1]), 1]), diff(tau[!is.na(tau[,2]), 2]))
  
  taumin = min(dtau, na.rm = TRUE)
  taumax = 2* pi
  # compute the number of Fourier coefficients
  N0 = max(taumax / taumin)
  # use Nyquist and compute the wavenumber range for each stock
  k = matrix(1:floor(N0/2), 1, length(1:floor(N0/2)))  # minimum k range
  N = length(k)
  
  # --------------------------------------------------------------------------------
  
  Sigma = switch(fourierMethod,
                 "ComplexExpFejer" = {
                   e_t_dp.PosComplexExpDiffPrice = matrix(0, mp, N)
                   e_t_dp.NegComplexExpDiffPrice = matrix(0, mp, N)
                   C0 = matrix(0, mp, 1)
                   
                   idNanTau = is.na(tau) * 1
                   
                   for (i in 1:mp) {
                     # nonuniformly sampled data from sparse matrix
                     psii = which(idNanTau[,i]==0)
                     # first price is not a return
                     psii = psii[-1]
                     end = length(psii)
                     psi = matrix(p[psii, i], end, 1)
                     # slice the data first
                     tsi = tau[ ,i]
                     # sampling times from the rescaled domain
                     tsi = matrix(tsi[psii], end, 1)
                     
                     P = psi # unevenly sampled prices on [0, pi]
                     Time = tsi  # unevenly sampled times
                     K = k   # wave numbers
                     
                     # ------- Fourier Transform on inhomogeneously sampled data -------
                     
                     # create complex exponential coefficients, multiply with
                     # log-price differences
                     #for (j in 1:length(K)) {
                    #   e_t_dp.PosComplexExpDiffPrice[i, j] = ComplexExp(P, Time, K[j])
                    # }
                     
                     C0[i] = sum(P)
                     
                     for (j in 1:N) {
                       e_t_dp.NegComplexExpDiffPrice[i, j] = ComplexExp(P, -Time, K[j])
                     }
                     e_t_dp.PosComplexExpDiffPrice[i, ] = Conj(e_t_dp.NegComplexExpDiffPrice[i, ])
                   }
                   
                   c_pos = matrix(0, mp, 2*N+1)
                   c_neg = matrix(0, mp, 2*N+1)
                   
                   c_pos[1,] = c(e_t_dp.NegComplexExpDiffPrice[1,], C0[1], e_t_dp.PosComplexExpDiffPrice[1,])
                   c_pos[2,] = c(e_t_dp.NegComplexExpDiffPrice[2,], C0[2], e_t_dp.PosComplexExpDiffPrice[2,])
                   c_neg[1,] = c(e_t_dp.PosComplexExpDiffPrice[1,], C0[1], e_t_dp.NegComplexExpDiffPrice[1,])
                   c_neg[2,] = c(e_t_dp.PosComplexExpDiffPrice[2,], C0[2], e_t_dp.NegComplexExpDiffPrice[2,])
                   
                   Sigma = matrix(NA, mp, mp)
                   Sigma[1, 1] = 1 / (2*N+1) * (sum(c_pos[1, ] * c_neg[1, ]))
                   Sigma[1, 2] = 1 / (2*N+1) * (sum(c_pos[1, ] * c_neg[2, ]))
                   Sigma[2, 1] = Sigma[1, 2]
                   Sigma[2, 2] = 1 / (2*N+1) * (sum(c_pos[2, ] * c_neg[2, ]))
                   Sigma
                 },
                 "HY" = {
                   
                   ## rescale domain to [0, PI]
                   tau = scale.tau(t)
                   
                   # -----------------------------------------------------------------
                   
                   idNanTau = is.na(tau) * 1
                   # extract position of time with trades
                   t1 = tau[which(idNanTau[,1]==0), 1]
                   t2 = tau[which(idNanTau[,2]==0), 2]
                   # extract position of price with trades
                   price1 = p[,1]
                   price2 = p[,2]
                   price1 = price1[idNanTau[,1]==0]
                   price2 = price2[idNanTau[,2]==0]
                   
                   # the first price is not a return
                   price1 = price1[-1]
                   price2 = price2[-1]
                   
                   end1 = length(t1)
                   end2 = length(t2)
                   
                   price1 = matrix(price1, end1-1, 1)
                   price2 = matrix(price2, end2-1, 1)
                   
                   # -----------------------------------------------------------------
                   
                   # when i = j, it is simply QV
                   Sigma = matrix(NA, mp, mp)
                   Sigma[1, 1] = t(price1) %*% price1
                   Sigma[2, 2] = t(price2) %*% price2
                   
                   ## Kanatani Weight matrix
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

cppFunction("arma::cx_rowvec ComplexExp(arma::rowvec R, arma::colvec T, int K) {
  int t = T.n_cols;
  std::complex<double> ii(0,1);
  arma::cx_mat L(t, 1, arma::fill::zeros);
  L = T * K * ii;
  return R * exp(L);}",
            depends="RcppArmadillo")
