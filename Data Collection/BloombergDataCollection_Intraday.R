.libPaths("F:/HonoursThesis/Software/Rpackages")
.libPaths()
install.packages(c("datetime","Rblpapi","tictoc","openxlsx"))
install.packages("doParallel")
rm(list=ls())
con <- NULL # automatic if option("blpAutoConnect") is TRUE


# Tickers to be used
tickers = NULL
start_date = NULL
end_date =NULL
init = function(){
  library(Rblpapi)
  library(datetime)
  library(tictoc)
  library(openxlsx)
  library(doParallel)
  con <<- blpConnect()
  tickers <<- c("ANH SJ Equity","BTI SJ Equity","NPN SJ Equity",
                "GLN SJ Equity","CFR SJ Equity","BIL SJ Equity",
                "AGL SJ Equity","SOL SJ Equity","FSR SJ Equity",
                "SBK SJ Equity","VOD SJ Equity","S32 SJ Equity",
                "SLM SJ Equity","MNP SJ Equity","OMU SJ Equity",
                "MTN SJ Equity","NED SJ Equity","ABG SJ Equity",
                "AMS SJ Equity","DSY SJ Equity")
  start_date <<- strptime("2018-01-01 23:59:51.123456","%Y-%m-%d %H:%M:%OS")
  op = options(digits.secs=6)
  end_date <<- Sys.time()
}



get_multiple_tickers = function(security,start_date,end_date){
  
  intraday_data = getMultipleTicks(security,startTime = start_date, endTime = end_date, 
                                   returnAs = "data.frame")
  
  return(intraday_data)
}
init()


for(i in 1:length(tickers)){
  ticker = tickers[i]
  print(ticker)
  tic()
  intraday_data = get_multiple_tickers(ticker,start_date,end_date)
  end = toc()
  time_taken = end$toc - end$tic
  write(time_taken,paste0("writeTimes",ticker,".txt"), append = TRUE)
  
  rm(list=setdiff(ls(), c("intraday_data","init","get_multiple_tickers","ticker")))
  save.image(paste0("F:/HonoursThesis/HonoursThesis/Data/Intraday/","JSEDATA",ticker,".RData"))
  print(paste0(ticker," saved"))
  init()
  
}


foreach(i=3:length(tickers), .combine)%dopar%{
  get_ticker_data()
}


get_ticker_data = function(ticker){
  
  print(ticker)
  tic()
  intraday_data = get_multiple_tickers(ticker,start_date,end_date)
  end = toc()
  time_taken = end$toc - end$tic
  write(time_taken,paste0("writeTimes",ticker,".txt"), append = TRUE)
  
  rm(list=setdiff(ls(), c("intraday_data","init","get_multiple_tickers","ticker","get_ticker_data")))
  save.image(paste0("F:/HonoursThesis/HonoursThesis/Data/Intraday/","JSEDATA",ticker,".RData"))
  print(paste0(ticker," saved"))
  init()
}









