# Author: Roger Bukuru
# This script generates synchronous data after the data has been aggregated to remove repeated simulataneous trades

require(pacman)
p_load(tidyverse)

create_bar_data = function(tickers,start_date,frequency, frequency_units, bar_frequency,bar_frequency_units,vwap=FALSE){
  # period of data
  end_date = start_date+as.difftime(frequency,units =frequency_units)
  #bar period i.e 1 week, 5min etc
  bar_period = seq.POSIXt(from = start_date, to=end_date, by=paste(bar_frequency,bar_frequency_units))
  field = NULL
  if(vwap){
    field = "vwap"
  }else{
    field = "close"
  }
  
  aggregated_stock_data= lapply(tickers, function(ticker){
    aggregated_stock = stock_data_stream[[ticker]]%>%filter(condcode=="AT")%>%
      filter(times>= start_date & times<=end_date)%>%
      aggregate_data()%>%
      convert_prices_to_returns()
    return(data.frame(aggregated_stock))
  })
  stock_bar_data = lapply(aggregated_stock_data,bar_data,bar_period)
  vwap_bar_data = lapply(stock_bar_data, function(bar_data){
    matched_indexes = sapply(as.numeric(bar_data[,"times"]),get_matching_indexes,bar_period)
    new_bar_data = data.frame(Date=  as.POSIXct(bar_period, origin="1970-01-01", tz="UCT"),
                              vwap = vector(mode = "numeric",length(bar_period)))
    colnames(new_bar_data) = c("Date",field)
    new_bar_data[matched_indexes,2]  = bar_data[,field]
    new_bar_data[-matched_indexes,2] = NaN
    return(new_bar_data[,field])
  })
  names(stock_bar_data) = tickers
  names(vwap_bar_data) = tickers
  
  vwap_stock_bar_data_period = data.frame(Date=  as.POSIXct(bar_period, origin="1970-01-01", tz="UCT"))
  joined_vwap_stock_bar_data = do.call("cbind", vwap_bar_data)
  joined_vwap_stock_bar_data = cbind(vwap_stock_bar_data_period,joined_vwap_stock_bar_data)
  
  na_only_rows = which(rowSums(is.na(joined_vwap_stock_bar_data))==length(tickers)) # NA on all columns
  joined_vwap_stock_bar_data = joined_vwap_stock_bar_data[-na_only_rows,] # remove NA's 
  
  data = list(stock_bar_data=stock_bar_data,vwap_bar_data = joined_vwap_stock_bar_data)
  return(data) 
  
}
#-----------------------------------------------------------------------------------------------------------

bar_data = function(ticker_data,bar_period){
  
  bar_data = data.frame(
    times=bar_period[1], 
    open=vector(mode = "numeric",1),
    high=vector(mode = "numeric",1),
    low=vector(mode = "numeric",1),
    close=vector(mode = "numeric",1),
    volume=vector(mode = "numeric",1),
    vwap = vector(mode = "numeric",1))
  
  first_price_entered = FALSE;
  pos = 1
  for(i in 2:length(bar_period)){
    
    open = NA
    high = NA
    low =  NA
    close = NA
    volume = NA
    
    stock_data = ticker_data %>% filter(times>bar_period[i-1] & times<=bar_period[i])
    
    if(nrow(stock_data)>0){
      if(!first_price_entered){
        open = stock_data[1,3] # initial opening price not yet set
        first_price_entered = TRUE
      }else{
        open = bar_data[pos-1,"close"] # previous closing price
      }
      high = max(stock_data[,"stock_returns"])
      low =  min(stock_data[,"stock_returns"])
      close = stock_data[nrow(stock_data),"stock_returns"]
      volume = sum(stock_data[,"size"])
      bar_data[pos,"times"]   = bar_period[i]
      bar_data[pos,"open"]    = open
      bar_data[pos,"high"]    = high
      bar_data[pos,"low"]     = low
      bar_data[pos,"close"]   = close
      bar_data[pos,"volume"]  = volume
      bar_data[pos,"vwap"]     = sum(stock_data[,"stock_returns"]*stock_data[,"size"])/volume
      pos = pos+1
      
    }
  }
  return(bar_data)
}

