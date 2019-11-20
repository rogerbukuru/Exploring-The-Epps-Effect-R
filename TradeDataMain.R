# Author: Roger Bukuru


rm(list = ls())
#------------------------------------------------ Install Packages --------------------------------------------#
list.of.packages <- c(
  "tidyverse", "Rcpp","RcppArmadillo",
  "pracma","DescTools","matlab",
  "doParallel","pacman","viridis","knitr","tictoc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(pacman)
p_load(tidyverse,doParallel,tictoc,Rcpp,RcppArmadillo)
#--------------------------------------------------------------------------------------------------------------#


#------------------------------------ Load All Necessary Files ------------------------------------------------#
source("LoadTradeData.R")
source("Data Creation Algorithms/AsynchronousData.R")
source("Data Creation Algorithms/SynchronousData.R")
source("Data Creation Algorithms/IntrinsicTimeVolumeBuckets.R")
source("Data Creation Algorithms/EventTimeVolumeBuckets.R")
source("Estimators/ftcorr-RealData.R")
source("Estimators/ftcorr-MultiAsset.R")
source("HY_MM_DataFormat.R")

#--------------------------------------------------------------------------------------------------------------


# auxilary function to get various periods of the sampled data
# includes last week
# last day
# last month 
extract_data_last_period = function(asset_data,period_frequency,period){
  end_date   = as.POSIXct(asset_data[nrow(asset_data),1], tz="UTC",origin="1970-01-01")
  switch (period,
          
          "week" = {
            # Last week of the month 
            frequency = period_frequency
            frequency_units = "weeks"
            last_week = end_date-as.difftime(frequency,units =frequency_units,format = "%Y-%m-%d %H:%M:%OS")
            last_trading_week = asset_data%>%filter(Date>=last_week)
            return(last_trading_week)
          },
          
          "day" = {
            # Last day of the month 
            frequency = period_frequency
            frequency_units = "days"
            last_day = end_date-as.difftime(frequency,units =frequency_units,format = "%Y-%m-%d %H:%M:%OS")
            last_trading_day = asset_data%>%filter(Date>=last_day)
            return(last_trading_day)
          },
          
          "hour" ={
            # Last 1 hour of the month
            frequency = period_frequency
            frequency_units = "hours"
            last_hour = end_date-as.difftime(frequency,units =frequency_units,format = "%Y-%m-%d %H:%M:%OS")
            last_trading_hour = asset_data%>%filter(Date>=last_hour)
            return(last_trading_hour)
          }
  )
}

tickers = NULL
starting_months = NULL
# This function initializes the environment needed before data can be generated
init_env = function(tickers= c("BTI","NPN")){
  
  trade_starting_dates = sapply(tickers, function(ticker){ # all starting dates
    stock_data = stock_data_stream[[ticker]]%>%filter(condcode=="AT")
    return(stock_data$times[1])
  })
  
  trade_ending_dates = sapply(tickers, function(ticker){ # ending dates
    stock_data = stock_data_stream[[ticker]]%>%filter(condcode=="AT")
    return(stock_data$times[nrow(stock_data)])
  })
  
  
  trade_starting_dates = na.omit(trade_starting_dates)
  starting_date = as.POSIXct(min(trade_starting_dates), tz="UTC",origin="1970-01-01")
  trade_ending_dates = na.omit(trade_ending_dates)
  ending_date = as.POSIXct(max(trade_ending_dates), tz="UTC",origin="1970-01-01")
  starting_months = seq.POSIXt(starting_date,ending_date,by="month", format = "%Y-%m-%d %H:%M:%OS")
  tickers <<- tickers
  starting_months <<- starting_months
  
}




generate_data = function(asyncData=TRUE,
                         syncData=FALSE, 
                         volumeBucket=FALSE,
                         dermanFramework=TRUE,
                         starting_month,
                         frequency=1,
                         frequency_unit="weeks",
                         asyncPrice=TRUE,
                         asyncVolume=FALSE,
                         bar_frequency=1, 
                         bar_frequency_units="mins",
                         bucket_frequency = 480,
                         vwap=FALSE){
  
  if((asyncData & syncData & volumeBucket)
     ||(asyncData & syncData)
     ||(asyncData & volumeBucket)
     ||(syncData & volumeBucket)
     ){
    return(stop("Only one type of data can be created at a time"))
  }else{
    
    if(asyncData){
  
      return(create_async_data(starting_month,frequency,frequency_unit,asyncPrice,asyncVolume))
    }
    
    if(syncData){
      return(create_sync_data (starting_month,frequency,frequency_unit,bar_frequency,bar_frequency_units,vwap,asyncPrice))
    }
    
    if(volumeBucket){
      return(create_volume_buckets(starting_month,frequency, frequency_unit,bucket_frequency,dermanFramework))
    }
  }

}

# Data is automatically written to an excel file 
# Frequency can be any finite number typically 1 should be fine
# Frequency units includes the following: secs, mins, hours, days, weeks
# If price is true prices are returned
# If volume is true volumes are returned both cannot be true

create_async_data = function (starting_month,frequency, frequency_unit,price,volumes){
 
  if(price & volumes){
    return(stop("Either price or volumes should be selected not both"))
  }
  data_sample = create_data_sample(tickers,starting_month,frequency,frequency_unit,price,volumes)
  return(data_sample)
 
}


# Data is automatically written to an excel file 
# Frequency can be any finite number typically 1 should be fine
# Frequency units includes the following: secs, mins, hours, days, weeks
# Bar frequency along with the Bar Frequency Units indicates the frequency of the bars i.e the default setting is 1 min bars for a 1 week sample

create_sync_data = function(starting_month,frequency, frequency_unit, bar_frequency,bar_frequency_units, vwap,prices) {
  bar_data_sample = create_bar_data(tickers,starting_month,frequency,frequency_unit,bar_frequency,bar_frequency_units, vwap, prices)
  #period_data = bar_data_sample$vwap_bar_data
  return(bar_data_sample)
}



# Frequency can be any finite number typically 1 should be fine
# Frequency units includes the following: secs, mins, hours, days, weeks
# Bucket frequency should be provided e.g 8 48, 480
# Derman Framework used by default, set to False to use Lining Up Events

create_volume_buckets = function(starting_month,frequency, frequency_unit,bucket_frequency, dermanFramework) {

  stock_prices  = create_async_data(starting_month,frequency, frequency_unit,price=TRUE,volumes=FALSE)
  stock_volumes = create_async_data(starting_month,frequency, frequency_unit,price=FALSE,volumes=TRUE)
  if(dermanFramework){
    volume_bucket_data = create_derman_volume_buckets(tickers,bucket_frequency,stock_prices,stock_volumes)
    return(volume_bucket_data)
  }
  volume_bucket_data = create_volume_bucket_lining_up_events(tickers,bucket_frequency,stock_prices,stock_volumes)
  return(volume_bucket_data)
  
}



