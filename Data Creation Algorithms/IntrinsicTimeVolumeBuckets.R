# Author: Roger Bukuru
# Implementation of the Derman Framework to create event time volume buckets


expand_ticker_volumes = function(tickers,stock_volumes,stock_prices){
  
  volumes_expanded  = lapply(tickers, function(ticker){ # reset volume for the next day 
    
    data_volume = stock_volumes[ticker]
    data_price = stock_prices[ticker]
    data_volume = na.omit(data_volume);
    data_price = na.omit(data_price);
    expanded_stock_data = data.frame(
      volume = rep(c(rep(1,length(data_volume[,ticker]))),c(data_volume[,ticker])),
      price = rep(c(data_price[,ticker]),c(data_volume[,ticker])),
      ticker_name = ticker)
    return(expanded_stock_data)
  })
  names(volumes_expanded) = tickers
  return(volumes_expanded)
}

#---------------------------------------------------------------------------------------------------------------


derman_volume_buckets = function(stock_data,total_buckets,volume_bucket_size,ticker){
  final_stock_data = data.frame(vwap= numeric(length = total_buckets))

  for( i in 1:total_buckets){
    bucket_size = 0 
    final_index = 0
    vwap= NaN
    final_volume= NaN
    stock_prices = stock_data[1:volume_bucket_size,"price"]
    stock_volumes = stock_data[1:volume_bucket_size,"volume"]
    vwap = sum(stock_prices*stock_volumes)/sum(stock_volumes)
    final_volume = sum(stock_volumes)
    stock_data = stock_data[-(1:volume_bucket_size),]
    final_stock_data[i,] = vwap
  }
  colnames(final_stock_data) = ticker
  return(final_stock_data)
}


#--------------------------------------------------------------------------------------------------------------

create_derman_volume_buckets = function(tickers, bucket_freq,stock_prices,stock_volumes){
  
  final_date  =  stock_prices$Date[length(stock_prices$Date)]
  days = seq.POSIXt(stock_prices$Date[1],final_date,by="day", format = "%Y-%m-%d %H:%M:%OS")
  days = as.Date(days,"UTC")
  daily_trade_volumes = lapply(days, function(day){
    return(stock_volumes%>%filter(grepl(as.Date(day,"UTC"),Date)))
  })
  
  daily_trade_prices = lapply(days, function(day){
    return(stock_prices%>%filter(grepl(as.Date(day,"UTC"),Date)))
  })
  
  
  number_of_days = length(days)
  avg_daily_trades_per_ticker = numeric(length = length(tickers))
  
  for (i in 1:length(tickers)) {
    ticker_total_volumes_per_day = sapply(daily_trade_volumes, function(day_trades){
      return(sum(na.omit(day_trades[tickers[i]])))
    })
    avg_daily_trades_per_ticker[i] = sum(ticker_total_volumes_per_day)/number_of_days
  }
  
  volume_bucket_sizes = avg_daily_trades_per_ticker/bucket_freq
  z = 1
  daily_volume_buckets = lapply(daily_trade_volumes, function(day_volume){ # expand per day
    volume_data = day_volume
    price_data = daily_trade_prices[[z]]
    volumes_expanded = expand_ticker_volumes(tickers,volume_data,price_data) # expanded volumes for the daya 
    j = 1
    bucket_sizes = numeric(length = length(tickers))
    day_stock_buckets = lapply(volumes_expanded, function(stock_data){ # now get buckets for each ticker for that day 
      total_buckets = floor(sum(stock_data[,"volume"])/volume_bucket_sizes[j])
      stock_buckets = derman_volume_buckets(stock_data,total_buckets,volume_bucket_sizes[j], tickers[j])
      bucket_sizes[j] <<- nrow(stock_buckets)
      j <<- j+1
      return(stock_buckets)
    })
    index_max = which(bucket_sizes == max(bucket_sizes))
    day_stock_buckets = lapply(day_stock_buckets, function(stock_data){
       no_rows_short = bucket_sizes[index_max] - nrow(stock_data)
       if(no_rows_short>0){ # there is a short fall
         missing_data = matrix(NaN,ncol = 1, nrow = no_rows_short)
         colnames(missing_data) = colnames(stock_data)
         stock_data = rbind(stock_data, missing_data)
       }
       return(stock_data)
    })
    final_day_stock_buckets = do.call("cbind",day_stock_buckets)
    percentage  = round((z/number_of_days)*100,2)
    print(paste(percentage,"%","complete", "day", z,"of", number_of_days))
    z <<- z+1
    return(final_day_stock_buckets)
  })
  
  volume_buckets = do.call("rbind",daily_volume_buckets)
  volume_buckets = cbind(daily_trade_volumes[[1]][1:nrow(volume_buckets),"Date"],volume_buckets)
  colnames(volume_buckets) = c("Date", tickers)
  #volume_buckets = daily_volume_buckets
  stock_returns = lapply(tickers, function(ticker){
    ticker_data = data.frame(times=volume_buckets["Date"],value=volume_buckets[ticker])
    colnames(ticker_data) = c("times", "value")
    ticker_data = ticker_data%>%convert_prices_to_returns()
    final_ticker_data = as.data.frame(ticker_data[,"stock_returns"])
    colnames(final_ticker_data) = ticker
    return(final_ticker_data)
  })
  final_volume_buckets = do.call("cbind",stock_returns)
  na_only_rows = which(rowSums(is.na(final_volume_buckets))==length(tickers)) # NA on all columns
  if(length(na_only_rows)>0){
    final_volume_buckets = final_volume_buckets[-na_only_rows,] # remove NA's
  }
  volume_ticks = 1:nrow(final_volume_buckets)
  final_volume_buckets = cbind(volume_ticks,final_volume_buckets)
  print("completed to create buckets...")
  return(final_volume_buckets)
}
