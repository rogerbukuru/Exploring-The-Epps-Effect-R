## Author: Patrick Chang & Roger Bukuru
# Script file to plot the TAQ data heatmaps

#--------------------------------------------------------------------------

library(reshape2)
library("RColorBrewer")

#--------------------------------------------------------------------------
## 1 week asynchronous data

tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 

# MM
MM1WeekAsync = readRDS("Computed Correlation/AsyncMMWeek.RData")

stock_correlations = MM1WeekAsync$Correlation
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
  labs(title = "(d) MM TAQ")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0355 \\pm 0.0166"), size = 6) + 
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

# HY
HY1WeekAsync = readRDS("Computed Correlation/AsyncHYWeek.RData")

stock_correlations = HY1WeekAsync$Correlation
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
  labs(title = "(h) HY TAQ")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.1051 \\pm 0.0635"), size = 6) + 
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

