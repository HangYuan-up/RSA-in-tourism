rm(list=ls())
library(pheatmap)
library(readxl)

#Figure 2A:Representational similarity matrix of neural response at electrode sites level 
ERPdata <- read_excel(".../Dataset_Figure2_RSA of neural response.xlsx",sheet = 1)
OT_highfitness<-t(as.data.frame(ERPdata[1,c(2:18)]))
OT_highfitness<-as.matrix(dist(OT_highfitness))
OT_highfitness<-1-OT_highfitness/max(OT_highfitness)
bk <- c(seq(0,0.5,by=0.01),seq(0.51,1,by=0.01))
a<-pheatmap(OT_highfitness,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
            color = c(colorRampPalette(colors = c("#097CFF","white", 'red'))(100)),
            legend_breaks=seq(0,1,0.2),
            breaks=bk,angle_col=45)

#Figure 2B:Representational similarity matrix of neural response at stimulus types level
rm(list=ls())
ERPdata <- read_excel(".../Dataset_Figure2_RSA of neural response.xlsx",sheet = 2)
OT_centralfrontal<-t(as.data.frame(ERPdata[1,c(2:7)]))
OT_centralfrontal<-as.matrix(dist(OT_centralfrontal))
OT_centralfrontal<-1-OT_centralfrontal/max(OT_centralfrontal)
bk <- c(seq(0,0.5,by=0.01),seq(0.51,1,by=0.01))
a<-pheatmap(OT_centralfrontal,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
            color = c(colorRampPalette(colors = c("#097CFF","white", 'red'))(100)),
            legend_breaks=seq(0,1,0.2),
            breaks=bk,angle_col=45)

#Figure 2C: Representation similarity matrices of neural signals across time points.
rm(list=ls())
timedata <- read_excel(".../Dataset_Figure2_RSA of neural response.xlsx",sheet = 3)
timedataslice<-as.data.frame(matrix(data=NA,nrow = 601,ncol=8))
colnames(timedataslice)<-c("time(ms)","timepoint","CH","CM","CL","FH","FM","FL")
timewindow<-as.data.frame(matrix(data=NA,nrow =20,ncol=6))
for (i in 1:601){
  timedataslice[i,1]<-timedata[i,1]
  timedataslice[i,2]<-timedata[i,2]
  for (j in 1:20){  # sliding time window = 40ms(20 time points)
    timewindow[j,1]<-timedata[c(i+j-1),3]
    timewindow[j,2]<-timedata[c(i+j-1),4]
    timewindow[j,3]<-timedata[c(i+j-1),5]
    timewindow[j,4]<-timedata[c(i+j-1),6]
    timewindow[j,5]<-timedata[c(i+j-1),7]
    timewindow[j,6]<-timedata[c(i+j-1),8]
  }
  timedataslice[i,c(3:8)]<-t(as.matrix(colMeans(na.omit(timewindow))))
}

#t3:i=3
for (i in 3){
  OT_centralfrontal<-timedataslice[i,c(3:8)]
  OT_centralfrontal<-as.data.frame(OT_centralfrontal)
  OT_centralfrontal<-as.matrix(dist(t(OT_centralfrontal)))
  OT_centralfrontal<-1-OT_centralfrontal/max(OT_centralfrontal)
  bk <- c(seq(0,0.5,by=0.01),seq(0.51,1,by=0.01))
  a<-pheatmap(OT_centralfrontal,cluster_rows = FALSE, cluster_cols = FALSE,border_color = "gray",
              color = c(colorRampPalette(colors = c("#097CFF","white", 'red'))(100)),
              legend_breaks=seq(0,1,0.2),
              breaks=bk,angle_col=45)
}

