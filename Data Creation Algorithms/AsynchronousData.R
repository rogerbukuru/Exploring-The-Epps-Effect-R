# Author: Roger Bukuru
# Date: 16 July 2019 22:00:00
# This script generates asynchronous data after the data has been aggregated to remove repeated simulataneous trades
require(pacman)
p_load(tidyverse,knitr,openxlsx)

#Aggregate data to remove repeated simultaneous trades
aggregate_data = function(ticker_data){
  
  ticker_unique_times = as.numeric(unique(ticker_data[,"times"]))
  aggreagted_ticker_data = data.frame(
    times =as.POSIXct( ticker_unique_times, origin="1970-01-01", tz="UTC"),
    type="TRADE",
    value= numeric(length =length(ticker_unique_times)),
    size = numeric(length =length(ticker_unique_times)))
  
  stock_times = sapply(ticker_unique_times, function(stock_unique_times,all_stock_times) {
    return(which(all_stock_times==stock_unique_times))
  },ticker_data[,"times"])
  
  indexes = which(sapply(stock_times, function(stock_times) {
    return(length(stock_times) >=2)
  })==TRUE)
  
  # merged data 
  merged_data = sapply(indexes, function(index) {
    stock_prices    = ticker_data[,"value"][stock_times[index][[1]]]
    stock_volumes   = ticker_data[,"size"][stock_times[index][[1]]]
    volume_weighted_price = sum(stock_prices*stock_volumes)/sum(stock_volumes)
    final_volume = sum(stock_volumes)
    return(c(volume_weighted_price,final_volume))
  })
  aggreagted_ticker_data[indexes,"value"]   = merged_data[1,]
  aggreagted_ticker_data[indexes,"size"]    = merged_data[2,]
  
  # unmerged data
  non_aggregated_indexes = unlist(stock_times[-indexes])
  unmerged_data = sapply(non_aggregated_indexes, function(index){
    stock_price = ticker_data[,"value"][index]
    stock_volume = ticker_data[,"size"][index]
    return(c(stock_price,stock_volume))
  })
  
  aggreagted_ticker_data[-indexes,"value"]   =   unmerged_data[1,]
  aggreagted_ticker_data[-indexes,"size"]    =   unmerged_data[2,]
  
  return(aggreagted_ticker_data)
}

remove_overnight_trades  = function(ticker_data){
  
  ticker_length = nrow(ticker_data)
  trading_days  = seq.POSIXt(ticker_data[1,"times"], ticker_data[ticker_length,"times"], by="day")
  trading_day_data = lapply(trading_days, function(trading_day){
    # Clean up data to remove overnight trades and trades that occur during algorithmic calibaration
    trading_day_data  = ticker_data%>%filter(grepl(as.Date(trading_day,"UTC"),times))
    overnight_trading = as.POSIXct(paste(as.Date(trading_day,"UTC"),"09:09:59"), tz="UTC",origin="1970-01-01")
    last_bid_auctions = as.POSIXct(paste(as.Date(trading_day,"UTC"),"16:49:59"), tz="UTC",origin="1970-01-01")
    trading_day_data  = trading_day_data%>%filter(times>overnight_trading & times < last_bid_auctions)
  })
  trading_day_data = Filter(Negate(is.null), trading_day_data)
  trading_day_data = do.call("rbind",trading_day_data)
  return(trading_day_data)
}

convert_prices_to_returns = function(ticker_data){
  #ticker_data$times = as.POSIXct(ticker_data[,1], format="%Y-%m-%d %H:%M:%OS")
  ticker_length = nrow(ticker_data)
  end_day = ticker_data[ticker_length,"times"]+ as.difftime(1,units = "days")
  trading_days  = seq.POSIXt(ticker_data[1,"times"], end_day, by="day",format = "%Y-%m-%d %H:%M:%OS")
  first_day = trading_days[1]
  trading_day_data = lapply(trading_days, function(trading_day){
    
    trading_day_data  = ticker_data%>%filter(grepl(as.Date(trading_day,"UTC"),times))
    if(nrow(trading_day_data)>0){
      stock_returns = diff(log(trading_day_data[,"value"]))
      
      if(first_day==trading_day_data[1,"times"]){
        stock_returns = c(0, stock_returns)
      }else{
        stock_returns = c(NaN, stock_returns)
      }
      trading_day_data = cbind(trading_day_data,stock_returns)
      #trading_day_data = na.omit(trading_day_data) # returns of first day price and previous day closing price
      return(trading_day_data)
    }
  })
  
  trading_day_data = Filter(Negate(is.null), trading_day_data)
  trading_day_data = do.call("rbind",trading_day_data)
  return(trading_day_data)
  
}


# matched indexes with period
get_matching_indexes = function(stocks_times,period){
  
  return(which(period==stocks_times))
  
}

create_data_sample = function(tickers, start_date,frequency=1, frequency_units="weeks", prices=FALSE, volumes=FALSE){
  
  end_date = start_date+as.difftime(frequency,units =frequency_units,format = "%Y-%m-%d %H:%M:%OS")
  period = start_date:end_date
  field  = NULL
  if(prices){
    field="value"
  }else{
    field = "stock_returns"
  }
  if(volumes){
    field = "size"
  }
  
  # First Get Only Trade Date
  # Filter Trades within period
  # Aggregate any repeated simultaneuos data
  # Remove all overnight trades
  # Remove trades between algorithmic calboration
  
  aggregated_stock_data= lapply(tickers, function(ticker){
    aggregated_stock = stock_data_stream[[ticker]]%>%filter(condcode=="AT")%>%
      filter(times>= start_date & times<=end_date)%>%
      aggregate_data()%>%
      convert_prices_to_returns()
    return(data.frame(aggregated_stock))
  })

  
  names(aggregated_stock_data) = tickers
  aggregated_matched_data = lapply(aggregated_stock_data, function(agg_stock_data){
    matched_indexes = sapply(as.numeric(agg_stock_data[,"times"]),get_matching_indexes,period)
    async_data = data.frame(Date=  as.POSIXct(period, origin="1970-01-01", tz="UTC"),
                            value = vector(mode = "numeric",length(period)))
    colnames(async_data) = c("Date",field)
    async_data[matched_indexes,2]  = agg_stock_data[,field]
    async_data[-matched_indexes,2] = NaN
    return(as.data.frame(async_data[,field]))
  })
  names(aggregated_matched_data) = tickers
  
  # merge data frames 
  aggregated_async_data = data.frame(Date=  as.POSIXct(period, origin="1970-01-01", tz="UTC"))
  aggregated_merged_data = do.call("cbind",aggregated_matched_data)
  aggregated_merged_data = cbind(aggregated_async_data,aggregated_merged_data)
  colnames(aggregated_merged_data) = c("Date",tickers)
  
  na_only_rows = which(rowSums(is.na(aggregated_merged_data))==length(tickers)) # NA on all columns
  aggregated_merged_data = aggregated_merged_data[-na_only_rows,] # remove NA's 
  
  
  stocks = combine_words(unlist(tickers), sep="",and="", after="_")
  write.csv2(aggregated_merged_data,file=paste0("Cleaned Data/Asynchronous Data/",stocks,start_date,"-",end_date,"Prices",".csv"), row.names = FALSE)
  
  return(aggregated_merged_data)
  
}
