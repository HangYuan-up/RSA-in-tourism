rm(list=ls())
library(pheatmap)
library(readxl)

tourismdata <- read_excel(".../Dataset_Figure6_Cross Temporal Univariate RSA.xlsx",sheet = 1)

#Figure 6A:The time change pattern of domestic tourist numbers in Beijing from 2005 to 2019
beijingtourist<-tourismdata[1,c(2:16)]
beijingtourist<-as.data.frame(beijingtourist)
beijingtourist<-as.matrix(dist(t(beijingtourist)))
beijingtourist<-1-beijingtourist/max(beijingtourist)
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
a<-pheatmap(beijingtourist,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
            color = c(colorRampPalette(colors = c("#097CFF","white"))(60),
                      colorRampPalette(colors = c("white","red"))(40)),
            legend_breaks=seq(0,1,0.1),
            breaks=bk,angle_col=45)

#Figure 6B:The time change pattern of national inbound tourism income from 2005 to 2019.
national_inbound_tourism_income<-tourismdata[2,c(2:16)]
national_inbound_tourism_income<-as.data.frame(national_inbound_tourism_income)
national_inbound_tourism_income<-as.matrix(dist(t(national_inbound_tourism_income)))
national_inbound_tourism_income<-1-national_inbound_tourism_income/max(national_inbound_tourism_income)
#图2
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
a<-pheatmap(national_inbound_tourism_income,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
            color = c(colorRampPalette(colors = c("#097CFF","white"))(50),
                      colorRampPalette(colors = c("white","red"))(50)),
            legend_breaks=seq(0,1,0.1),
            breaks=bk,angle_col=45)