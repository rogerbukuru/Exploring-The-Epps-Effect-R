# Author: Roger Bukuru
# Date: 23 July 2019 12:21:55
rm(list = ls())
library(tidyverse)
library(openxlsx)

stock_data_stream = NULL
available_ticker_names = NULL
# Load all data into program 
#setwd(paste0(getwd(),"/","Software"))
initialise_raw_data = function() {
  
  files = Sys.glob("../Raw Data Store/Intraday/*.RData")
  start = Sys.time()
  stock_data_stream <<- lapply(files, function(ticker_data){
    load(ticker_data)
    ticker_data = intraday_data
    return(data.frame(ticker_data))
  })
  end = Sys.time()
  print(end-start)
  
  # name the list approp
  available_ticker_names <<- files %>%str_remove("../Raw Data Store/Intraday/")%>%
    str_remove(".RData")%>%
    str_remove("JSEDATA")%>%
    str_replace_all(" ","")%>%
    str_remove("SJEquity")
  names(stock_data_stream) <<- available_ticker_names
  # Data in the list can then be written to MongoDB
  print(available_ticker_names)
}

initialise_raw_data()
