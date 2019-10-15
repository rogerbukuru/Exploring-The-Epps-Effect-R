# An Exercise in R: High Frequency Covariance estimation using Mallivan-Mancino and Hyashi-Yoshida estimators
A group research project completed by Patrick Chang and Roger Bukuru and supervised by Professor Tim Gebbie, submitted in fulfilment of the requirements for the degree of Bachelor of Science Honours Specialising in Statistical Sciences in the Department of Statistical Sciences at the University of Cape Town.

The code serves to encapsulate two main distinct areas within the project. The two areas involves the following

- Monte Carlo Simulations
- Trade Quote(TAQ) Data Engineering and Data Science


## Monte Carlo Simulations

The results in here are all simulated stochastic differential equations and they include the following: 

- Geometric Brownian Motion
- Merton Model
- Ornstein Uhlenbeck
- Variance Gamma
- Garch Anderson
- Garch Reno

To obtain any of the following simulated results, simply run the appropriate script that can be found in the **Monte Carlo Simulation Algorithms** directory and one should be able to recover the results as per the report. 

At the heart of the project is one being able to implement the estimators of various forms of data. Irrespective of whether the data is simulated or real financial data once the data is ready it is split into two matrices, namely the price matrix and the time matrix therefater the estimators are implemented. Using dummy data, below we illustrate a simple usage case.


### Estimators Example Usage 

```{.r}

source("ftcorr-RealData.R")
source("ftcorr-MultiAsset.R")

t = matrix(c(1, NaN, 5, NaN, NaN, 15, 2, NaN, NaN, 10, 12, 20),6, 2)
p = matrix(c(10, NaN, 9, NaN, NaN, 7, 2, NaN, NaN, 9, 8, 10),6, 2)

MMEstimator = ftcorr(p,t,"ComplexExpFejer",F) # Mallivan-Mancino estimator 
HYEstimator = ftcorr(p,t,"HY",F) # Hyashi-Yoshida estimator

```


## TAQ Data Engineering and Data Science

The estimators can be applied to various forms of aggregated TAQ data across various sampling intervals. The aggregation methods fall into two main frameworks namely Calendar Time and Intrinsic Time, within Calendar Time the following TAQ aggregated formats exists:

- Closing Prices 
- Volume Weighted Average Price (VWAP)

and within the Intrinsic time framework the following formats of aggregated TAQ data exists:

- Derman Framework
- Lining Up Events

The is no limitations to the sampling intervals i.e it could be 1 minute, 10 minutes, 1 hour, 1 day, 1 week etc.

### Data Setup and Directory

Before one can be able to implement any of the examples one first has to download and load the intraday data used for this project, the data can be downloaded from ... Once the data has been downloaded it should follow the following hierachical strucure for ease of use.

- Data should be stored one level outside of this directory(once cloned), in the following exact directory pattern 'Raw Data Store/Intraday'.


**Below we show example usages of how to create aggregated data, however in the folder Aggregated Data Examples, there ready to use scripts that implement fully composed examples for both Calendar Time and Intrinsic Time, the scripts create aggregted data, run the estimators and finally plot correlation heatmaps for both the estimators**


### Generating Calendar Time Aggregated TAQ Data Example Usage

```{.r}
source("TradeDataMain.R")
tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 
init_env(tickers)

#--------------------------------- Closing Bar Data --------------------------------------------

closing_1Min = generate_data(starting_month = starting_months[6], 
                  frequency=1,
                  frequency_unit = "weeks", 
                  asyncPrice = F,
                  syncData = T, 
                  asyncData = F,
                  bar_frequency=1,
                  bar_frequency_units = "mins")

closing_1Min = closing_1Min$bar_data


#--------------------------------- VWAP Bar Data--------------------------------------------

vwap_1Min = generate_data(starting_month = starting_months[6], 
                             frequency=1,
                             frequency_unit = "weeks", 
                             asyncPrice = F,
                             syncData = T, 
                             asyncData = F,
                             bar_frequency=1,
                             vwap = T,
                             bar_frequency_units = "mins")

vwap_1Min = vwap_1Min$bar_data
```

### Generating Intrinsic Time Aggregated TAQ Data Example Usage

```{.r}

source("TradeDataMain.R")
tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 
init_env(tickers)

#--------------------------------- Derman Framework --------------------------------------------
derman_480_vb = generate_data(starting_month = starting_months[6], 
                                         frequency=1,
                                         frequency_unit = "weeks", 
                                         asyncData = F,
                                         volumeBucket = T,
                                         bucket_frequency = 480)
                                         
#--------------------------------- Lining Up Events --------------------------------------------                                         
lue_480_vb = generate_data(starting_month = starting_months[6], 
                                         frequency=1,
                                         frequency_unit = "weeks", 
                                         asyncData = F,
                                         volumeBucket = T,
                                         dermanFramework = F,
                                         bucket_frequency = 480)
                                         
final_lue_480_vb = lue_480_vb$volume_bucket_returns 

```

**Note the script file ExampleDataSets.R has more examples on how to create data sets for many more sampling intervals**


### Running Estimators on Aggregated Data

```{.r}

source("TradeDataMain.R")
source("DataEstimatedCorrelations")
tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 
init_env(tickers)

correlation_estimators = get_estimated_correlations(aggreagted_data)

MMEstimator = correlation_estimators[[1]]
HYEstimator = correlation_estimators[[2]]

```

