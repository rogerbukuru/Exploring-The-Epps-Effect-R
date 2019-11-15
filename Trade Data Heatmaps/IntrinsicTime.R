## Author: Patrick Chang & Roger Bukuru
# Script file to plot the Derman data heatmaps

#--------------------------------------------------------------------------
# Derman Bucket's

library(reshape2)
library("RColorBrewer")
library(latex2exp)
tickers = c("BTI","NPN","AGL","MNP","SOL","SBK","NED","ABG","SHP","FSR") 
volumeBucket8 = readRDS("MMHYDermanBucket8.RData")
MMBucket8 = volumeBucket8[[1]]
HYBucket8 = volumeBucket8[[2]]
volumeBucket48 = readRDS("MMHYDermanBucket48.RData")
MMBucket48 = volumeBucket48[[1]]
HYBucket48 = volumeBucket48[[2]]
volumeBucket480 = readRDS("MMHYDermanBucket480.RData")
MMBucket480 = volumeBucket480[[1]]
HYBucket480 = volumeBucket480[[2]]

#--------------------------------------------------------------------------
## Malavian Mancino 
# 8 Bucket
stock_correlations = MMBucket8$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)

ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(a) MM 8 Volume Bucket")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.2209"), size = 6) + 
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

# 48 Bucket
stock_correlations = MMBucket48$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)

ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(b) MM 48 Volume Bucket")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0879"), size = 6) + 
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


# 480 Bucket
stock_correlations = MMBucket480$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(c) MM 480 Volume Bucket")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.023"), size = 6) + 
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
# 8 Bucket
stock_correlations = HYBucket8$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)

ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(d) HY 8 Volume Bucket")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.266"), size = 6) + 
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

# 48 Bucket
stock_correlations = HYBucket48$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)

ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(e) HY 48 Volume Bucket")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0914"), size = 6) + 
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


# 480 Bucket
stock_correlations = HYBucket480$Correlation
colnames(stock_correlations) = tickers
rownames(stock_correlations) = tickers
melted_corr = melt(stock_correlations)
color <- colorRampPalette(c('red','orange',"yellow",'springgreen', 'royalblue'))(sqrt(NROW((melted_corr))))%>%rev()
round(mean(abs(stock_correlations[lower.tri(stock_correlations)])), 4)


ggplot(data = melted_corr, aes(Var1,Var2 , fill = value))+
  geom_tile()+
  scale_fill_gradientn(colours = color,limit=c(-1,max(stock_correlations)),name=TeX("$\\rho$")) +
  theme_bw()+ 
  labs(title = "(f) HY 480 Volume Bucket")+
  annotate("text", x = 7, y = 3, label = TeX("$\\bar{| \\rho_{ij} | } = 0.0338"), size = 6) + 
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

