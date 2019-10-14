# An Exercise in R: High Frequency Covariance estimation using Mallivan-Mancino and Hyashi-Yoshida estimators
A group research project completed by Patrick Chang and Roger Bukuru and supervised by Professor Tim Gebbie, submitted in fulfilment of the requirements for the degree of Bachelor of Science Honours Specialising in Statistical Sciences in the Department of Statistical Sciences at the University of Cape Town.

The code serves to encapsulate two main distinct areas within the project. The two areas involves the following

- Monte Carlo Simulations
- Trade Quote(TAQ) Data Engineering and Data Science


## Monte Carlo Simulations


## TAQ Data Engineering and Data Science

The estimators can be applied to various forms of aggregated TAQ data across various sampling intervals. The aggregation methods fall into two main frameworks namely Calendar Time and Intrinsic Time, within Calendar Time the following TAQ aggregated formats exists:

- Closing Prices 
- Volume Weighted Average Price (VWAP)

and within the Intrinsic time framework the following formats of aggregated TAQ data exists:

- Derman Framework
- Lining Up Events

The is no limitations to the sampling intervals i.e it could be 1 minute, 10 minutes, 1 hour, 1 day, 1 week etc.

#### Generating Calendar Time Aggregated TAQ Data Example Usage

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

#### Generating Intrinsic Time Aggregated TAQ Data Example Usage

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


