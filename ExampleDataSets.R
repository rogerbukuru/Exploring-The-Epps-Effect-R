source("TradeDataMain.R")
source("DataEstimatedCorrelations.R")
tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 
init_env(tickers)

#-------------------------------------------------- Closing Bar Data --------------------------------------------
closing_1Min = generate_data(starting_month = starting_months[6], 
                  frequency=1,
                  frequency_unit = "weeks", 
                  asyncPrice = F,
                  syncData = T, 
                  asyncData = F,
                  bar_frequency=1,
                  bar_frequency_units = "mins")

closing_1Min = closing_1Min$bar_data

closing_10Min = generate_data(starting_month = starting_months[6], 
                              frequency=1,
                              frequency_unit = "weeks", 
                              asyncPrice = F,
                              syncData = T, 
                              asyncData = F,
                              bar_frequency=10,
                              bar_frequency_units = "mins")

closing_10Min = closing_10Min$bar_data

closing_1Hour = generate_data(starting_month = starting_months[6], 
                              frequency=1,
                              frequency_unit = "weeks", 
                              asyncPrice = F,
                              syncData = T, 
                              asyncData = F,
                              bar_frequency=1,
                              bar_frequency_units = "hours")

closing_1Hour = closing_1Hour$bar_data

#----------------------------------------------------------------------------------------------------------------
# Computing the Correlations
# 1 min Closing
get_estimated_correlations(closing_1Min, "SynchronousDataHYMM1")

# 10 min Closing
get_estimated_correlations(closing_10Min, "SynchronousDataHYMM2")

# 1 HR Closing
get_estimated_correlations(closing_1Hour, "SynchronousDataHYMM3")

#-------------------------------------------------------VWAP Bar Data--------------------------------------------

vwap_1Min = generate_data(starting_month = starting_months[6], 
                             frequency=1,
                             frequency_unit = "weeks", 
                             asyncPrice = F,
                             syncData = T, 
                             asyncData = F,
                             bar_frequency=1,
                             vwap = T,
                             bar_frequency_units = "mins")

vwap_1Min = vwap_1Min$bar_data

vwap_10Min = generate_data(starting_month = starting_months[6], 
                              frequency=1,
                              frequency_unit = "weeks", 
                              asyncPrice = F,
                              syncData = T, 
                              asyncData = F,
                              bar_frequency=10,
                              vwap = T,
                              bar_frequency_units = "mins")

vwap_10Min = vwap_10Min$bar_data

vwap_1Hour = generate_data(starting_month = starting_months[6], 
                              frequency=1,
                              frequency_unit = "weeks", 
                              asyncPrice = F,
                              syncData = T, 
                              asyncData = F,
                              bar_frequency=1,
                              vwap = T,
                              bar_frequency_units = "hours")

vwap_1Hour = vwap_1Hour$bar_data

#----------------------------------------------------------------------------------------------------------------
# Computing the Correlations
# 1 min VWAP
get_estimated_correlations(vwap_1Min, "SynchronousDataHYMMVWAP1")

# 10 min VWAP
get_estimated_correlations(vwap_10Min, "SynchronousDataHYMMVWAP2")

# 1 HR VWAP
get_estimated_correlations(vwap_1Hour, "SynchronousDataHYMMVWAP3")
