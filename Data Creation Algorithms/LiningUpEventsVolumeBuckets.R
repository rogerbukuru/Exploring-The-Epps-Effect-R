# Author: Roger Bukuru
# Implementation of the Lining Up Events to create event time volume buckets
 

expand_volume_over_price = function(stock_data,ticker_name){ 
  expanded_data = data.frame(
    volume = rep(1, stock_data[1,"volume"]), 
    price =  rep(stock_data[1,"price"], stock_data[1,"volume"]),
    ticker_name = rep(ticker_name,stock_data[1,"volume"])
  )
  return(expanded_data)
}

get_bucket_data = function(stock_data,volume_bucket_size){
  bucket_size = 0 
  final_index = 0
  vwap= NaN
  final_volume= NaN
    bucket_size = sum(stock_data[1:volume_bucket_size,"volume"])
    
    if(!is.na(bucket_size)){
      if(bucket_size==volume_bucket_size){
        stock_prices = stock_data[1:volume_bucket_size,"price"]
        stock_volumes = stock_data[1:volume_bucket_size,"volume"]
        vwap = sum(stock_prices*stock_volumes)/sum(stock_volumes)
        final_volume = sum(stock_volumes)
        stock_data = stock_data[-(1:volume_bucket_size),]
      }
    }
 
  
  data = list(vwap=vwap,final_volume=final_volume, stock_data=stock_data)
  return(data)
}

create_volume_data = function(volume_data, price_data,volume_bucket_size,tickers, day_number){
  ticker_volume_data = volume_data[1,]
  volume_sizes  = lapply(tickers, function(ticker){ # reset volume for the next day 
    ticker_info = data.frame(volume=NaN, price=NaN,ticker_name=as.character(ticker))
    return(ticker_info)
  })
  names(volume_sizes) = tickers
  
  total_rows = nrow(volume_data)
  
  for(i in 1:nrow(volume_data)){

    percentage  = round((i/total_rows)*100,2)
    print(paste(percentage,"%"," complete of day number ",day_number, "total obs", total_rows))
    current_volume = volume_data[i,2:ncol(volume_data)]
    current_price = price_data[i,2:ncol(price_data)]
    current_ticker_volume_data =  volume_data[i,]
    
    j <<- 1;
    volume_sizes = lapply(volume_sizes, function(volume_size){
      ticker = volume_size[1,"ticker_name"]
      current_row_data = data.frame(
        volume = current_volume[,j], 
        price =  current_price[,j],
        ticker_name = as.character(ticker)
      )
      
      if(!is.na(current_row_data[1,"volume"])){
        current_row_data = expand_volume_over_price(current_row_data,current_row_data[1,"ticker_name"])
      }
      final_ticker_data = rbind(volume_size,current_row_data )
      final_ticker_data = na.omit(final_ticker_data)
      bucket_data = NULL
      if(nrow(final_ticker_data)>0){
       # if(final_ticker_data[1,1]==-1){
        #  final_ticker_data = final_ticker_data[-1,]
      #  }
        bucket_data = get_bucket_data(final_ticker_data,volume_bucket_size)
        
      }
      if(!is.null(bucket_data)){
        final_ticker_data = bucket_data[["stock_data"]]
        current_ticker_volume_data[,j+1] <<- bucket_data[["vwap"]]
      }else{
        final_ticker_data = data.frame(volume=NaN, price=NaN,ticker_name=as.character(ticker))
        current_ticker_volume_data[,j+1] <<- NaN
      }
      
      j <<- j+1
      return(final_ticker_data)
    })
    names(volume_sizes) = tickers
    ticker_volume_data = rbind(ticker_volume_data,current_ticker_volume_data)
    
  } # gone all through all the required data now check if can still make buckets from what's left
  data = list(volume_sizes=volume_sizes,ticker_volume_data=ticker_volume_data)
  return(data)
}


get_remaining_buckets = function(volume_data,volume_sizes,ticker_volume_data,volume_bucket_size){
  
  numb_buckets_left = sapply(volume_sizes, function(volume_size){
    number_of_buckets = 0
    if(nrow(volume_size)>=volume_bucket_size){
      total_left_over = sum(volume_size["volume"])
      number_of_buckets = total_left_over/volume_bucket_size
    }
    return(floor(number_of_buckets))
  })
  print(paste("Number of buckets left ",numb_buckets_left))
  bucket_indexes = which(numb_buckets_left>=1) # check if there are actually any left overs
  if(length(bucket_indexes)>=1){# at least one ticker can still make buckets 
    max_number_buckets_left = numb_buckets_left[which(numb_buckets_left==max(numb_buckets_left))]
  
    for(i in 1: max_number_buckets_left){
   
      percentage  = round((i/max_number_buckets_left)*100,2)
      print(paste0(percentage,"% "," complete of creating left over buckets"))
      
      current_ticker_volume_data =  volume_data[1,]
      
      j = 1;
      volume_sizes = lapply(volume_sizes, function(volume_size){
        bucket_data = get_bucket_data(volume_size,volume_bucket_size)
        final_ticker_data = bucket_data[["stock_data"]]
        current_ticker_volume_data[,j+1] <<- bucket_data[["vwap"]]
        j <<- j+1
        return(final_ticker_data)
      })
      names(volume_sizes) = tickers
      ticker_volume_data = rbind(ticker_volume_data,current_ticker_volume_data)
      
    }
  }
  ticker_volume_data = ticker_volume_data[-1,]
  data = list(volume_sizes=volume_sizes,ticker_volume_data=ticker_volume_data)
  return(data)
}


create_volume_bucket_lining_up_events = function(tickers,bucket_freq,stock_prices,stock_volumes){
  
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
 
  avg_daily_trades_per_ticker = sapply(tickers, function(ticker){
    return(sum(na.omit(stock_volumes[,ticker])))
    })
  avg_daily_trades_per_ticker = avg_daily_trades_per_ticker/number_of_days
  print(avg_daily_trades_per_ticker)
  
  index_max = which(avg_daily_trades_per_ticker==max(avg_daily_trades_per_ticker))
  most_liquid_ticker = tickers[index_max]
  volume_bucket_size = as.numeric(floor(avg_daily_trades_per_ticker[index_max]/bucket_freq))
  #volume_bucket_size = 500
  
  print(number_of_days)
  print(most_liquid_ticker)
  print(volume_bucket_size)
  print("starting to create buckets...")
  
  z = 1

  daily_volume_buckets = lapply(daily_trade_volumes, function(day_volume){ # go day by day
      print(paste("Day number ", z))
      volume_data = day_volume
      price_data  = daily_trade_prices[[z]]
      result = create_volume_data(volume_data,price_data,volume_bucket_size,tickers,z)
      volumes_left = result$volume_sizes
      current_buckets = result$ticker_volume_data
      final_bucket_data = get_remaining_buckets(volume_data,volumes_left,current_buckets,volume_bucket_size)
      final_volume_buckets = final_bucket_data$ticker_volume_data
      remaining_buckets = final_bucket_data$volume_sizes
      print(paste("Day number ", z, " bucket creation done"))
      z <<- z +1
      day_data = list(final_volume_buckets=final_volume_buckets,remaining_buckets=remaining_buckets)
     
      return(day_data)
   })
  final_daily_data = lapply(daily_volume_buckets, function(day_data){
    return(day_data[["final_volume_buckets"]])
  })
   volume_buckets = do.call("rbind",final_daily_data)
  na_only_rows = which(rowSums(is.na(volume_buckets))==length(tickers)) # NA on all columns
  volume_buckets = volume_buckets[-na_only_rows,] # remove NA's
  volume_buckets = as.data.frame(volume_buckets)
  volume_buckets$Date = as.POSIXct(volume_buckets[,"Date"],origin="1970-01-01",tz="UTC")

  stock_returns = lapply(tickers, function(ticker){
   ticker_data = data.frame(
     times=volume_buckets[,"Date"],
     value=volume_buckets[,ticker])
   colnames(ticker_data) = c("times", "value")
   num_obs = nrow(ticker_data)
   non_na_indexes = which(!is.na(ticker_data[,"value"]))
   non_na_indexes[1] = 1
   ticker_data = na.omit(ticker_data)
   ticker_data = ticker_data%>%convert_prices_to_returns()
   final_ticker_data = data.frame(ticker=rep(NaN,num_obs))
   colnames(final_ticker_data) = ticker
   final_ticker_data[non_na_indexes,ticker] = ticker_data[,"stock_returns"]
   return(final_ticker_data)
 })
 final_volume_buckets = do.call("cbind",stock_returns)
 na_only_rows = which(rowSums(is.na(final_volume_buckets))==length(tickers)) # NA on all columns
 final_volume_buckets = final_volume_buckets[-na_only_rows,] # remove NA's
 volume_ticks = 1:nrow(final_volume_buckets)
 final_volume_buckets = cbind(volume_ticks,final_volume_buckets)
 print("completed to create buckets...")
 data = list(volume_bucket_prices=volume_buckets , volume_bucket_returns = final_volume_buckets)
 return(volume_buckets)
}