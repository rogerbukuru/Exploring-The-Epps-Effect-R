source("TradeDataMain.R")
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

#-------------------------------------------------------Derman Framework----------------------------------------

derman_48_volume_bucket = generate_data(starting_month = starting_months[6], 
                                         frequency=1,
                                         frequency_unit = "weeks", 
                                         asyncData = F,
                                         volumeBucket = T,
                                         bucket_frequency = 48)

derman_480_volume_bucket = generate_data(starting_month = starting_months[6], 
                                         frequency=1,
                                         frequency_unit = "weeks", 
                                         asyncData = F,
                                         volumeBucket = T,
                                         bucket_frequency = 480)

#------------------------------------------------------- Lining Up Framework------------------------------------

lue_8_volume_bucket = generate_data(starting_month = starting_months[6], 
                                     frequency=1,
                                     frequency_unit = "weeks", 
                                     asyncData = F,
                                     volumeBucket = T,
                                     dermanFramework = F,
                                     bucket_frequency = 8)

lue_48_volume_bucket = generate_data(starting_month = starting_months[6], 
                                      frequency=1,
                                      frequency_unit = "weeks", 
                                      asyncData = F,
                                      volumeBucket = T,
                                      dermanFramework = F,
                                      bucket_frequency = 48)

final_lue_48_volume_bucket  = lue_48_volume_bucket$volume_bucket_returns

lue_480_volume_bucket = generate_data(starting_month = starting_months[6], 
                                         frequency=1,
                                         frequency_unit = "weeks", 
                                         asyncData = F,
                                         volumeBucket = T,
                                         dermanFramework = F,
                                         bucket_frequency = 480)

final_lue_480_volume_bucket  = lue_480_volume_bucket$volume_bucket_returns


