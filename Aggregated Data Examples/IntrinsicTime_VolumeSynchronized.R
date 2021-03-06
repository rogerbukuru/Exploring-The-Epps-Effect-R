# Author: Patrick Chang

# This script walks one through creating a 480 volume bucket of our method to aggregated data from TAQ data 
# Using this aggregated data, we compute the estimators and finally plot the correlation heatmap

source("TradeDataMain.R")
source("HY_MM_DataFormatVB.R")
# Indicate the tickers to use 
# If not sure simply type available_ticker_names in you console to get a list of the tickers
tickers = c("BTI","NPN","AGL","MNP*","SOL","SBK","NED","ABG","SHP","FSR") 
init_env(tickers) # initialises the environment 

# Once the environement has been initialised the key variable that is accessible is called starting_months
# This array indicates the various months where one wishes to start across the period of the data

# 480
# ----------------------------------------- Step 1: Create the aggregated data ---------------------------------

# The started month has been selected as the 6th elemenent, 
#Type starting_months in console if want to see months available 

volume_bucket_data_480 = generate_data(starting_month = starting_months[6], 
                                     frequency=1,
                                     frequency_unit = "weeks", 
                                     asyncData = F,
                                     volumeBucket = T,
                                     bucket_frequency = 480)

final_lue_480_volume_bucket  = volume_bucket_data_480$volume_bucket_returns

# --------------------------------------- Step 2: Run correlation estimators on data --------------------------

source("DataEstimatedCorrelations.R")

correlation_estimators480 = get_estimated_correlations(final_lue_480_volume_bucket, "MMHYTimBucket480")
MMEstimator = correlation_estimators480[[1]] # Mallivan-Mancino
HYEstimator = correlation_estimators480[[2]] # Hyashi-Yoshida

stock_correlations = MMEstimator$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)
round(sd(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


## MM 480
ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(b) MM 480 Least Liquid")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0103 \\pm 0.0084"), size = 6) + 
  theme(legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        panel.border = element_rect(),
        panel.grid.major= element_blank(),
        panel.grid.minor= element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 90),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+coord_fixed()


## HY 480
stock_correlations = HYEstimator$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)
round(sd(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(d) HY 480 Least Liquid")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.1284 \\pm 0.0929"), size = 6) + 
  theme(legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.title = element_text(size = 13, face = "bold"),
        panel.border = element_rect(),
        panel.grid.major= element_blank(),
        panel.grid.minor= element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13, angle = 90),
        plot.title = element_text(face="bold", colour="black", size=15,hjust=0.5,vjust=0))+coord_fixed()
