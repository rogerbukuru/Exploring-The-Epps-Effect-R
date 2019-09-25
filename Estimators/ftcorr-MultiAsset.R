## Author: Patrick Chang
# Script file for the MM Complex Fourier Transform and Hayashi Yoshida
# designed to compute correlation for the Real Data.
# This function computes the [m x m] integrated vol/corr
# calls the supporting function

#---------------------------------------------------------------------------

library(Rcpp)
library(RcppArmadillo)
library(pracma)
library(DescTools)
library(matlab)

#---------------------------------------------------------------------------

### Data Format:
## p = [n x m] log returns are computed in the function
# non-trading times are indicated by NaNs
## t = [n x m] matrix of trading times, non-trading times are indicated by NaNs
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

source("Estimators/ftcorr-RealData.R")

#---------------------------------------------------------------------------

ftcorr = function(price, time, fourierMethod, onlyOverlapping){
  ## preallocate arrays and check data
  np = dim(p)[1]
  mp = dim(p)[2]
  nt = dim(t)[1]
  if (nt != np){
    stop("Incorrect Data")
  }
  # Initialise integrated covariance matrix
  Sigma = matrix(NA, mp, mp)
  Sigma_rescaled = matrix(NA, mp, mp)
  for (i in 1:(mp-1)) {
    for(j in (i+1):mp) {
      p = price[,c(i,j)]
      t = time[,c(i,j)]
      
      sol = ftcorrgpu(p, t, fourierMethod, onlyOverlapping)
      
      Sigma[i,j] = sol$Integrated_Covariance[1,2]
      Sigma[j,i] = sol$Integrated_Covariance[1,2]
      Sigma[i,i] = sol$Integrated_Covariance[1,1]
      Sigma[j,j] = sol$Integrated_Covariance[2,2]
      
      Sigma_rescaled[i,j] = sol$Covariance[1,2]
      Sigma_rescaled[j,i] = sol$Covariance[1,2]
      Sigma_rescaled[i,i] = sol$Covariance[1,1]
      Sigma_rescaled[j,j] = sol$Covariance[2,2]
    }
  }
  Sigma = Re(Sigma)
  var = diag(Sigma)
  sigma = sqrt(var)
  rho = Re(Sigma / sigma %*% t(sigma))
  
  return(list(Correlation = rho,
              Integrated_Covariance = Sigma,
              Covariance = Sigma_rescaled,
              Variance = var,
              StdDev = sigma))
}