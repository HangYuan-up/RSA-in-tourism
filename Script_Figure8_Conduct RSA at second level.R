rm(list=ls())

library(pheatmap)
library(readxl)
library(Hmisc)
#Figure 8A：RSA model with region as the first-order dimension and time as the second-order dimension.
tourismdata <- read_excel(".../Dataset_Figure8_Conduct RSA at second level.xlsx",sheet = 1)
consume<-as.data.frame(matrix(data=NA,nrow = 435,ncol=15))
for (i in 1:15){
  consume_eachyear<-tourismdata[i,c(2:31)]
  consume_eachyear<-as.data.frame(consume_eachyear)
  consume_eachyear<-as.matrix(dist(t(consume_eachyear)))
  consume_eachyear<-1-consume_eachyear/max(consume_eachyear)
  consume_eachyear<-as.matrix(consume_eachyear[lower.tri(consume_eachyear,diag =FALSE)])
  consume[,i]<-consume_eachyear
}
consume_total <- rcorr(as.matrix(consume))
pheatmap(consume_total$r,
         cluster_rows = FALSE, cluster_cols = FALSE,
         border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(75),
                   colorRampPalette(colors = c("white","white"))(5),
                   colorRampPalette(colors = c("white","red"))(20)),
         legend_breaks=seq(0.80,1,0.05),angle_col=45)

#Figure 8B：RSA model with time as the first-order dimension and region as the second-order dimension.
rm(list=ls())
tourismdata <- read_excel(".../Dataset_Figure8_Conduct RSA at second level.xlsx",sheet = 2)
consume<-as.data.frame(matrix(data=NA,nrow = 105,ncol=30))
for (i in 1:30){
  consume_eacharea<-tourismdata[i,c(2:16)]
  consume_eacharea<-as.data.frame(consume_eacharea)
  consume_eacharea<-as.matrix(dist(t(consume_eacharea)))
  consume_eacharea<-1-consume_eacharea/max(consume_eacharea)
  consume_eacharea<-as.matrix(consume_eacharea[lower.tri(consume_eacharea,diag =FALSE)])
  consume[,i]<-consume_eacharea
}
library(Hmisc)
consume_total <- rcorr(as.matrix(consume))
pheatmap(consume_total$r,
         cluster_rows = FALSE, cluster_cols = FALSE,
         border_color = "gray",
         color = c(colorRampPalette(colors = c("#097CFF","white"))(85),
                   colorRampPalette(colors = c("white","white"))(3),
                   colorRampPalette(colors = c("white","red"))(17)),
         legend_breaks=seq(0.5,1,0.1),angle_col=45)
