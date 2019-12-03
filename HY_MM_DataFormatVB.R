# Author: Roger Bukuru
# Script to clean data into MM and HY data format  i.e into prices and times matrix 
# For Event Time


HYMMDataSets = function(stock_asset_data, tickers,start_date,end_date){
  
  asset_prices = lapply(tickers, function(ticker){
    asset_price = data.frame("Price"=stock_asset_data[,ticker])
    colnames(asset_price) = c(paste(ticker,"Price"))
    return(data.frame(asset_price))
  })
  names(asset_prices) = tickers
  
  asset_times = lapply(stock_asset_data, function(asset_price){
    asset_na_indices = which(is.na(asset_price))
    asset_time = data.frame(stock_asset_data[,"volume_ticks"])
    colnames(asset_time) = c("volume_ticks")
    asset_time[,"volume_ticks"] = as.numeric(asset_time[,"volume_ticks"])
    asset_time[asset_na_indices,] = NaN
    return(data.frame(asset_time))
  })
  #names(asset_times) = tickers
  asset_times = asset_times[-1]
  
  p = do.call("cbind",asset_prices)
  t = do.call("cbind",asset_times)
  colnames(p) = c(paste(tickers,"Price"))
  colnames(t) = c(paste(tickers,"volume_ticks"))
  stocks = combine_words(unlist(tickers), sep="", and="", after="_")
  
  #write.csv(p,file=paste0("Cleaned Data/Asynchronous Data/HY_MM Formats/","Prices",stocks,start_date,"-",end_date,".csv"), row.names = FALSE)
  
  #write.csv(t,file=paste0("Cleaned Data/Asynchronous Data/HY_MM Formats/","Times",stocks,start_date,"-",end_date,".csv"), row.names = FALSE)
  data = list(p=p,t=t)
  return(data)
  
}
