# Author: Roger Bukuru
# Script to clean data into MM and HY data format  i.e into prices and times matrix 

require(pacman)
p_load(viridis,tidyverse,knitr)

HYMMDataSets = function(stock_asset_data, tickers,start_date,end_date){
  
  asset_prices = lapply(tickers, function(ticker){
    asset_price = data.frame("Price"=stock_asset_data[,ticker])
    colnames(asset_price) = c(paste(ticker,"Price"))
    return(data.frame(asset_price))
  })
  names(asset_prices) = tickers
  
  asset_times = lapply(asset_prices, function(asset_price){
    asset_na_indices = which(is.na(asset_price))
    asset_time = data.frame(stock_asset_data[,"Date"])
    colnames(asset_time) = c("Date")
    asset_time[,"Date"] = as.numeric(asset_time[,"Date"])
    asset_time[asset_na_indices,] = NaN
    return(data.frame(asset_time))
  })
  names(asset_times) = tickers
  
  p = do.call("cbind",asset_prices)
  t = do.call("cbind",asset_times)
  colnames(p) = c(paste(tickers,"Price"))
  colnames(t) = c(paste(tickers,"Date"))
  stocks = combine_words(unlist(tickers), sep="", and="", after="_")

  #write.csv(p,file=paste0("Cleaned Data/Asynchronous Data/HY_MM Formats/","Prices",stocks,start_date,"-",end_date,".csv"), row.names = FALSE)
   
  #write.csv(t,file=paste0("Cleaned Data/Asynchronous Data/HY_MM Formats/","Times",stocks,start_date,"-",end_date,".csv"), row.names = FALSE)
  data = list(p=p,t=t)
  return(data)

}
