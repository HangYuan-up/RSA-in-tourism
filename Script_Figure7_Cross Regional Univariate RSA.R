rm(list=ls())
library(pheatmap)
library(readxl)

tourismdata <- read_excel(".../Dataset_Figure7_Cross Regional Univariate RSA.xlsx",sheet = 1)

#Figure 7:The cross-regional RSA model of domestic tourism income in 2019.
domestic_tourism_income<-tourismdata[1,c(2:31)]
domestic_tourism_income<-as.data.frame(domestic_tourism_income)
domestic_tourism_income<-as.matrix(dist(t(domestic_tourism_income)))
domestic_tourism_income<-1-domestic_tourism_income/max(domestic_tourism_income)
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
pheatmap(domestic_tourism_income,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
            color = c(colorRampPalette(colors = c("#097CFF","white"))(80),
                      colorRampPalette(colors = c("white","red"))(20)),
            legend_breaks=seq(0,1,0.1),
            breaks=bk,angle_col=45)
