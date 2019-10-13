## Author: Patrick Chang & Roger Bukuru
# Script file to plot the Closing prices data heatmaps

#--------------------------------------------------------------------------
# Closing

library(reshape2)
library("RColorBrewer")
library(latex2exp)
tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 
Onemin = readRDS("SynchronousDataHYMM1.RData")
MM1MinSyncClose = Onemin[[1]]
HY1MinSyncClose = Onemin[[2]]
TenMin = readRDS("SynchronousDataHYMM2.RData")
MM10MinSyncClose = TenMin[[1]]
HY10MinSyncClose = TenMin[[2]]
OneHour = readRDS("SynchronousDataHYMM3.RData")
MM1HourSyncClose = OneHour[[1]]
HY1HourSyncClose = OneHour[[2]]

#--------------------------------------------------------------------------
## Malliavin Mancino
# 1 Hour Bar
stock_correlations = MM1HourSyncClose$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(a) MM 1 HR Close Price")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.2829"), size = 6) + 
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


# 10 Minute Bar
stock_correlations = MM10MinSyncClose$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(b) MM 10 Min Close Price")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.2334"), size = 6) + 
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


# 1 Minute Bar
stock_correlations = MM1MinSyncClose$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(c) MM 1 Min Close Price")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.1384"), size = 6) + 
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


## Hayashi Yoshida
# 1 Hour Bar
stock_correlations = HY1HourSyncClose$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(e) HY 1 HR Close Price")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.2912"), size = 6) + 
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


# 10 Minute Bar
stock_correlations = HY10MinSyncClose$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(f) HY 10 Min Close Price")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.2347"), size = 6) + 
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


# 1 Minute Bar
stock_correlations = HY1MinSyncClose$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(g) HY 1 Min Close Price")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.1466"), size = 6) + 
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
