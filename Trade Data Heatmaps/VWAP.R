## Author: Patrick Chang & Roger Bukuru
# Script file to plot the VWAP data heatmaps

#--------------------------------------------------------------------------
# VWAP 

library(reshape2)
library("RColorBrewer")
tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 
load("SynchronousDataHYMMVWAP1.RData")
MM1MinSyncVWAP = MMSync
HY1MinSyncVWAP = HYSync
load("SynchronousDataHYMMVWAP2.RData")
MM10MinSyncVWAP = MMSync
HY10MinSyncVWAP = HYSync
load("SynchronousDataHYMMVWAP3.RData")
MM1HourSyncVWAP = MMSync
HY1HourSyncVWAP = HYSync

#--------------------------------------------------------------------------
## Malliavin Mancino
# 1 Hour Bar
stock_correlations = MM1HourSyncVWAP$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(a) MM 1 HR VWAP")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.1418"), size = 6) + 
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
stock_correlations = MM10MinSyncVWAP$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(b) MM 10 Min VWAP")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.097"), size = 6) + 
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


## 1 Minute Bar
stock_correlations = MM1MinSyncVWAP$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(c) MM 1 Min VWAP")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0627"), size = 6) + 
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
stock_correlations = HY1HourSyncVWAP$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(e) HY 1 HR VWAP")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.1449"), size = 6) + 
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


# 10 Min Bar
stock_correlations = HY10MinSyncVWAP$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(f) HY 10 Min VWAP")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0961"), size = 6) + 
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


# 1 Min Bar
stock_correlations = HY1MinSyncVWAP$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(g) HY 1 Min VWAP")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0784"), size = 6) + 
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



