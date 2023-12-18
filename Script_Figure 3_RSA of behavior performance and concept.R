rm(list=ls())

library(pheatmap)
library(readxl)
#Figure 3A:Representational similarity matrix of behavior performance
ERPdata <- read_excel(".../Dataset_Figure3_RSA of behavior performance and concept.xlsx",sheet = 1)
OT_fitness<-t(as.data.frame(ERPdata[1,c(2:7)]))
OT_fitness<-as.matrix(dist(OT_fitness))
OT_fitness<-1-OT_fitness/max(OT_fitness)
bk <- c(seq(0,0.5,by=0.01),seq(0.51,1,by=0.01))
a<-pheatmap(OT_fitness,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
            color = c(colorRampPalette(colors = c("#097CFF","white", 'red'))(100)),
            legend_breaks=seq(0,1,0.2),
            breaks=bk,angle_col=45)

#Figure 3B:Representational similarity matrix of concept
rm(list=ls())
ERPdata <- read_excel(".../Dataset_Figure3_RSA of behavior performance and concept.xlsx",sheet = 2)
OT_concept<-t(as.data.frame(ERPdata[1,c(2:7)]))
OT_concept<-as.matrix(dist(OT_concept))
OT_concept<-1-OT_concept/max(OT_concept)
bk <- c(seq(0,0.5,by=0.01),seq(0.51,1,by=0.01))
a<-pheatmap(OT_concept,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
            color = c(colorRampPalette(colors = c("#097CFF","white", 'red'))(100)),
            legend_breaks=seq(0,1,0.2),
            breaks=bk,angle_col=45)