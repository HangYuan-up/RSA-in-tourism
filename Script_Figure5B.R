rm(list=ls())

library(readxl)
library(ggplot2)
library(ggalt)
library(ade4)
#Figure 5D:Time-point-wise representational similarity values (r) between neural activity RDM and behavioral RDM in the PL group

timedata <- read_excel(".../Dataset_Figure5_timeserisedata_central_frontal.xlsx")

timedataslice<-as.data.frame(matrix(data=NA,nrow = 601,ncol=14))
colnames(timedataslice)<-c("time(ms)","timepoint","CHPL","CMPL","CLPL","FHPL","FMPL","FLPL","CHOT","CMOT","CLOT","FHOT","FMOT","FLOT")
timewindow<-as.data.frame(matrix(data=NA,nrow =10,ncol=12))
for (i in 1:601){
  timedataslice[i,1]<-timedata[i,1]
  timedataslice[i,2]<-timedata[i,2]
  for (j in 1:20){   
    timewindow[j,1]<-timedata[c(i+j-1),3]
    timewindow[j,2]<-timedata[c(i+j-1),4]
    timewindow[j,3]<-timedata[c(i+j-1),5]
    timewindow[j,4]<-timedata[c(i+j-1),6]
    timewindow[j,5]<-timedata[c(i+j-1),7]
    timewindow[j,6]<-timedata[c(i+j-1),8]
    timewindow[j,7]<-timedata[c(i+j-1),9]
    timewindow[j,8]<-timedata[c(i+j-1),10]
    timewindow[j,9]<-timedata[c(i+j-1),11]
    timewindow[j,10]<-timedata[c(i+j-1),12]
    timewindow[j,11]<-timedata[c(i+j-1),13]
    timewindow[j,12]<-timedata[c(i+j-1),14]
  }
  timedataslice[i,c(3:14)]<-t(as.matrix(colMeans(na.omit(timewindow))))
}


correlation<-matrix(data=NA,nrow = 601,ncol=1)
pvalue<-matrix(data=NA,nrow = 601,ncol=1)
significance<-matrix(data=NA,nrow = 601,ncol=1)

behavior_OT<-timedata[2,c(16:21)]
behavior_OT<-as.data.frame(behavior_OT)
behavior_OT<-as.matrix(dist(t(behavior_OT)))
behavior_OT<-1-behavior_OT/max(behavior_OT)

for (i in 1:601){
  OT<-timedataslice[i,c(9:14)]
  OT<-as.data.frame(OT)
  OT<-as.matrix(dist(t(OT)))
  OT<-1-OT/max(OT)
  a<-mantel.rtest(dist(t(OT)),dist(t(behavior_OT)),nrepet = 999)
  correlation[i]=a[["obs"]]
  pvalue[i]=a[["pvalue"]]
  if (pvalue[i]<=0.05){
    significance[i]=1}
  else{significance[i]=0}
}
rsadata<-as.data.frame(cbind(timedataslice[c(1:601),1],correlation,pvalue,significance))
colnames(rsadata)<-c("time","correlation","p","significance")


dev.new()
ggplot(rsadata,aes(y=correlation,x=time))+
  geom_line(colour="blue",size=1)+
  geom_line(aes(y=pvalue),colour="white",size=0)+
  scale_y_continuous(expand = c(0,0),limits = c(-0.6,1),
                     sec.axis = sec_axis(~.,name="p"))+
  scale_x_continuous(limits = c(-200,1000),breaks=seq(-200,1000,100))+
  geom_area(data=rsadata,aes(time,pvalue),alpha=0.2,fill=4)+
  theme_test(base_size = 20)+
  theme(legend.title = element_blank(),
        legend.text = element_text(family = 'serif'), 
        legend.position = c(.2,.9),
        legend.direction = "horizontal",
        axis.text = element_text(color = 'black',family = 'serif'),
        axis.title = element_text(family = 'serif',size = 18,color = 'black'))+
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed")+
  geom_vline(aes(xintercept=0), colour="black", linetype="dashed")+
  geom_hline(aes(yintercept=0.05), colour="red", linetype="dashed")
