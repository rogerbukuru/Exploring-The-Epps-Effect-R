# Author: Roger Bukuru
# Execute estimators on the given data sample


# Allows one to indicate if only a certain of the period of the data should be analysed, the period types includes the following:
# Last week of a month data sample
# Last day of a month sample
# Last houe of a month sample
# Frequency units includes day, week or hour

get_estimated_correlations = function(asset_data, frequency=1, frequency_unit="day", select_period=FALSE){
  period_data = asset_data
  if(select_period){
    period_data = extract_data_last_period(asset_data,frequency,frequency_unit) 
  }
  tickers = colnames(period_data)[-1]
  start_date = period_data[1,1]
  end_date   = period_data[nrow(period_data),1]
  hy_mm_data = HYMMDataSets(period_data, tickers,start_date,end_date)
  p = hy_mm_data$p
  t = hy_mm_data$t
  MMData = ftcorr(p,t,"ComplexExpFejer",F)
  HYData = ftcorr(p,t,"HY",F)
  result = list(MMData,HYData)
  saveRDS(result,"HYMMData.RData")
  return(result)
}


