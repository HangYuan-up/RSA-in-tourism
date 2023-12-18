rm(list=ls())
library(pheatmap)
library(readxl)

tourismdata <- read_excel(".../Dataset_Figure9_RSA of conceptual models.xlsx",sheet = 1)

#Figure 9A:Coastal province distribution matrix
Coastal<-tourismdata[1,c(2:31)]
Coastal<-as.data.frame(Coastal)
Coastal<-as.matrix(dist(t(Coastal)))
Coastal<-1-Coastal/max(Coastal)
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
pheatmap(Coastal,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(50),
                   colorRampPalette(colors = c("white","red"))(50)),
         legend_breaks=seq(0,1,0.1),
         breaks=bk,angle_col=45)

#Figure 9C:Cultural tightness-looseness concept matrix
Cultural_tightness_looseness<-tourismdata[2,c(2:31)]
Cultural_tightness_looseness<-as.data.frame(Cultural_tightness_looseness)
Cultural_tightness_looseness<-as.matrix(dist(t(Cultural_tightness_looseness)))
Cultural_tightness_looseness<-1-Cultural_tightness_looseness/max(Cultural_tightness_looseness)
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
pheatmap(Cultural_tightness_looseness,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(50),
                   colorRampPalette(colors = c("white","red"))(50)),
         legend_breaks=seq(0,1,0.1),
         breaks=bk,angle_col=45)

#Figure 9B:China's "Five-Year Plan" change matrix
rm(list=ls())
tourismdata <- read_excel(".../Dataset_Figure9_RSA of conceptual models.xlsx",sheet = 2)
five_year_plan<-tourismdata[1,c(2:16)]
five_year_plan<-as.data.frame(five_year_plan)
five_year_plan<-as.matrix(dist(t(five_year_plan)))
five_year_plan<-1-five_year_plan/max(five_year_plan)
bk <- c(seq(0,0.85,by=0.01),seq(0.86,1,by=0.01))
pheatmap(five_year_plan,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(50),
                   colorRampPalette(colors = c("white","red"))(50)),
         legend_breaks=seq(0,1,0.1),
         breaks=bk,angle_col=45)
