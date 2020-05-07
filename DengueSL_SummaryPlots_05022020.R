library(ggplot2)
library(reshape2)
library(pracma)
library(astsa)
library(sp)
library(GADMTools)
library(plotrix)
library(tsiR)
library(lubridate)
library(plm)
library(interp)
library(foreign)
library(gplots)
library(lfe)
library(zoo)
library(xts)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(graphics)

########################################################################
#########################Load Data######################################
setwd("/Users/cewagner/Documents/Princeton/Climate and Disease Projects/Dengue Sri Lanka Version 3/Data Files")
places <- read.csv(file="SriLanka_LatLon.csv", header=TRUE, sep=",")
numplacevec=dim(places)
numplace=numplacevec[1]
places$lat<-as.double(places$lat)
places$lon<-as.double(places$lon)
colnames(places)[3] <- "latitude"
colnames(places)[4] <- "longitude"
##Disease
Dengue_MonthTS <- read.csv(file="Dengue_Monthly_20102016.csv", header=TRUE, sep=",")
Dengue_MonthTS$date<-as.Date(Dengue_MonthTS$date, '%m/%d/%y')
Dengue_MonthTS20102018 <- read.csv(file="Dengue_Monthly_20102018.csv", header=TRUE, sep=",")
Dengue_MonthTS20102018$date<-as.Date(Dengue_MonthTS20102018$date, '%m/%d/%y')
##Climate
#Present Conditions
Tavg_MonthTS <- read.csv(file="Tavg_SriLanka_Monthly_20102016.csv", header=TRUE, sep=",")
Tavg_MonthTS$date<-as.Date(Tavg_MonthTS$date, '%m/%d/%y')
Tavg_PrevY_MonthTS <- read.csv(file="Tavg_SriLanka_Monthly_2009.csv", header=TRUE, sep=",")
Tavg_PrevY_MonthTS$date<-as.Date(Tavg_PrevY_MonthTS$date, '%m/%d/%y')
Precip_MonthTS <- read.csv(file="Precip_SriLanka_Monthly_20102016.csv", header=TRUE, sep=",")
Precip_MonthTS$date<-as.Date(Precip_MonthTS$date, '%m/%d/%y')
Precip_PrevY_MonthTS <- read.csv(file="Precip_SriLanka_Monthly_2009.csv", header=TRUE, sep=",")
Precip_PrevY_MonthTS$date<-as.Date(Precip_PrevY_MonthTS$date, '%m/%d/%y')
yearvec=unique(year(as.POSIXlt(Dengue_MonthTS$date, format="%d/%m/%Y")))
###################
##Map Data
maplka <- gadm_sf.loadCountries("LKA", level=1, basefile = "./")
#Rename any places whose names don't match up
places$regionname<-as.character(places$regionname)
###For Sri Lanka
places[6,1] = "Nuwara Eliya"
places[23,1] = "Moneragala"
#Demographic and tsiR data
Births_YearTS <- read.csv(file="Births_Yearly_20102016.csv", header=TRUE, sep=",")
Pop_YearTS <- read.csv(file="Population_Yearly_20092016.csv", header=TRUE, sep=",")
Pop_Year2018TS <- read.csv(file="Population_Yearly_20092018.csv", header=TRUE, sep=",")
#Take out 2009 data for Pop for these summary plots
Pop_YearTS<-Pop_YearTS[-1,]
Pop_Year2018TS<-Pop_Year2018TS[-1,]
#
IPdengue=2

#######################Compute Monthly Means 2010-2016##################################
monthmeanprecip=matrix(0,12,numplace)
monthsdprecip=matrix(0,12,numplace)
monthmeandengue=matrix(0,12,numplace)
monthsddengue=matrix(0,12,numplace)
monthmeantavg=matrix(0,12,numplace)
monthsdtavg=matrix(0,12,numplace)

tempsumprecip=0
tempsumprecipsd=matrix(0,1,length(yearvec))
tempsumdengue=0
tempsumdenguesd=matrix(0,1,length(yearvec))
tempsumtavg=0
tempsumtavgsd=matrix(0,1,length(yearvec))
for(i in 1:numplace){
  for(j in 1:12){
    for(k in 1:length(yearvec)){
      tempsumprecip=tempsumprecip+Precip_MonthTS[(k-1)*12+j,i+1]  
      tempsumprecipsd[1,k]=Precip_MonthTS[(k-1)*12+j,i+1] 
      tempsumdengue=tempsumdengue+Dengue_MonthTS[(k-1)*12+j,i+1]
      tempsumdenguesd[1,k]=Dengue_MonthTS[(k-1)*12+j,i+1] 
      tempsumtavg=tempsumtavg+Tavg_MonthTS[(k-1)*12+j,i+1]
      tempsumtavgsd[1,k]=Tavg_MonthTS[(k-1)*12+j,i+1] 
    }
    monthmeanprecip[j,i]=tempsumprecip/length(yearvec)
    monthsdprecip[j,i]=sd(tempsumprecipsd)
    monthmeandengue[j,i]=tempsumdengue/length(yearvec)
    monthsddengue[j,i]=sd(tempsumdenguesd)
    monthmeantavg[j,i]=tempsumtavg/length(yearvec)
    monthsdtavg[j,i]=sd(tempsumtavgsd)
    tempsumprecip=0
    tempsumprecipsd=matrix(0,1,length(yearvec))
    tempsumdengue=0
    tempsumdenguesd=matrix(0,1,length(yearvec))
    tempsumtavg=0
    tempsumtavgsd=matrix(0,1,length(yearvec))
  }
}
colnames(monthmeandengue)<-as.character(places[,1])
colnames(monthsddengue)<-as.character(places[,1])
colnames(monthmeanprecip)<-as.character(places[,1])
colnames(monthsdprecip)<-as.character(places[,1])
colnames(monthmeantavg)<-as.character(places[,1])
colnames(monthsdtavg)<-as.character(places[,1])

monthvec<-seq(1,12,1)
MeanMonthlyPrecip<-NULL
MeanMonthlyPrecip<-cbind(monthvec,monthmeanprecip)
MeanMonthlyPrecip<-as.data.frame(MeanMonthlyPrecip)
SDMonthlyPrecip<-NULL
SDMonthlyPrecip<-cbind(monthvec,monthsdprecip)
SDMonthlyPrecip<-as.data.frame(SDMonthlyPrecip)
MeanMonthlyDengue<-NULL
MeanMonthlyDengue<-cbind(monthvec,monthmeandengue)
MeanMonthlyDengue<-as.data.frame(MeanMonthlyDengue)
SDMonthlyDengue<-NULL
SDMonthlyDengue<-cbind(monthvec,monthsddengue)
SDMonthlyDengue<-as.data.frame(SDMonthlyDengue)
MeanMonthlyTavg<-NULL
MeanMonthlyTavg<-cbind(monthvec,monthmeantavg)
MeanMonthlyTavg<-as.data.frame(MeanMonthlyTavg)
SDMonthlyTavg<-NULL
SDMonthlyTavg<-cbind(monthvec,monthsdtavg)
SDMonthlyTavg<-as.data.frame(SDMonthlyTavg)

#############################Dengue data at yearly level#################################
Dengue_YearlyTS=matrix(0,length(yearvec),numplace)
Dengue_YearlyTS20102018=matrix(0,(length(yearvec)+2),numplace)

for(i in 1:numplace){
  monthcases <- as.xts(Dengue_MonthTS[,i+1],order.by=as.Date(Dengue_MonthTS[,1]))
  Dengue_YearlyTS[,i] <- apply.yearly(monthcases,sum)
  monthcases20102018 <- as.xts(Dengue_MonthTS20102018[,i+1],order.by=as.Date(Dengue_MonthTS20102018[,1]))
  Dengue_YearlyTS20102018[,i] <- apply.yearly(monthcases20102018,sum)
}

Dengue_YearSum=rowSums(Dengue_YearlyTS)
Dengue_YearSum20102018=rowSums(Dengue_YearlyTS20102018)

#############################Population 2010-2018 (1000)##########################################
Pop_Mean2018=matrix(0,1,numplace)
for(i in 1:numplace){
  Pop_Mean2018[,i] <- mean(Pop_Year2018TS[,i+1])/1000
}

dist_pop2018=cbind(places,t(Pop_Mean2018))
colnames(dist_pop2018)[1] <- "district"
colnames(dist_pop2018)[3] <- "latitude"
colnames(dist_pop2018)[4] <- "longitude"
colnames(dist_pop2018)[5] <- "population"

dist_pop2018$district <- as.character(dist_pop2018$district)
dist_pop2018<- rename(dist_pop2018, NAME_1 = district)
setwd("/Users/cewagner/Documents/Princeton/Climate and Disease Projects/Dengue Sri Lanka Version 3/Figures")
#tiff("PopChorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotpop2018<-choropleth(maplka,data=dist_pop2018, adm.join="NAME_1", breaks=c(0,250,500,750,1000,1250,1500,1750,2000,2250,2500), labels = c("(0,250]","(250,500]", "(500,750]", "(750,1000]", "(1000,1250]", "(1250,1500]", "(1500,1750]", "(1750,2000]","(2000,2250]","(2250,2500]"),value="population", palette=colorRampPalette(brewer.pal(10,"YlGn"))(10),  legend="")
chorplotpop2018<-chorplotpop2018  + theme(panel.grid.major = element_line(colour = 'transparent'), 
                                  panel.background = element_blank(),
                                  axis.text = element_blank(),
                                  panel.border = element_blank(),
                                  axis.ticks = element_blank(),
                                  text = element_text(size = 24))
plot(chorplotpop2018)
#dev.off()
########################National cases by year############################################
yearvec20102018=c(2010,2011,2012,2013,2014,2015,2016,2017,2018)
yearlydata=cbind(yearvec20102018,Dengue_YearSum20102018);
yearlydata<-as.data.frame(yearlydata);

par(mar=c(1,1,1,1)+2, tck=-0.03)
par(mgp=c(1.5, 0.5, 0))
par(mfrow=c(1,1))
#tiff("CasesNationally_20102018.tiff",units="in",width=7,height=6,res=300,compression='lzw')
countryyearlyplot<-ggplot(data=yearlydata, aes(x=yearvec20102018, y=Dengue_YearSum20102018/1000))+geom_line(colour='black', size=1.5) + 
  theme_bw(base_size=24) + scale_x_continuous(name ="Year", breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018)) + scale_y_continuous(name ="Dengue Cases (Thousands)",limits=c(0,200)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
plot(countryyearlyplot)
#dev.off()

#########################Choropleth region ratio 2017:mean#################################
Dengue_YearlyRatio_2017ToMean=matrix(0,1,numplace)
for(i in 1:numplace){
  Dengue_YearlyRatio_2017ToMean[1,i] <- Dengue_YearlyTS20102018[8,i]/mean(Dengue_YearlyTS[,i])
}

dist_dengueyearratio=cbind(places,t(Dengue_YearlyRatio_2017ToMean))
colnames(dist_dengueyearratio)[1] <- "district"
colnames(dist_dengueyearratio)[3] <- "latitude"
colnames(dist_dengueyearratio)[4] <- "longitude"
colnames(dist_dengueyearratio)[5] <- "ratio"

dist_dengueyearratio$district <- as.character(dist_dengueyearratio$district)
dist_dengueyearratio <- rename(dist_dengueyearratio, NAME_1 = district)
#tiff("Ratio2017_to_20102016_Choropleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotratio<-choropleth(maplka,data=dist_dengueyearratio, adm.join="NAME_1", breaks=c(2,3,4,5,6,7,8,9,10,11), labels = c("(2,3]","(3,4]", "(4,5]", "(5,6]", "(6,7]", "(7,8]", "(8,9]","(9,10]","(10,11]"),value="ratio", palette=colorRampPalette(brewer.pal(9,"YlGn"))(9),  legend="")
chorplotratio<-chorplotratio  + theme(panel.grid.major = element_line(colour = 'transparent'), 
                                      panel.background = element_blank(),
                                      axis.text = element_blank(),
                                      panel.border = element_blank(),
                                      axis.ticks = element_blank(),
                                      text = element_text(size = 24))
plot(chorplotratio)
#dev.off()
#####################Choropleth Seasonality 2010-2016 Milad Entropy Method#################################
##Compute yearly values 2010-2016
#Precip
Precip_YearlyTS=matrix(0,length(yearvec),numplace)

for(i in 1:numplace){
  monthprecip <- as.xts(Precip_MonthTS[,i+1],order.by=as.Date(Precip_MonthTS[,1]))
  Precip_YearlyTS[,i] <- apply.yearly(monthprecip,sum)
}

##Perform entropy calculation
#Precip
pm_precip=matrix(0,12,numplace)

for(i in 1:numplace){
  pm_precip[,i]=MeanMonthlyPrecip[,i+1]/mean(Precip_YearlyTS[,i])
}

Dm_precip=matrix(0,1,numplace)

for(i in 1:numplace){
  for(j in 1:12){
    Dm_precip[1,i]=Dm_precip[1,i]+pm_precip[j,i]*log(12*pm_precip[j,i])
  }
}

dist_precipent=cbind(places,t(Dm_precip))
colnames(dist_precipent)[1] <- "district"
colnames(dist_precipent)[3] <- "latitude"
colnames(dist_precipent)[4] <- "longitude"
colnames(dist_precipent)[5] <- "Dm"

dist_precipent$district <- as.character(dist_precipent$district)
dist_precipent <- rename(dist_precipent, NAME_1 = district)
#tiff("PrecipEntropyChorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotprecipent<-choropleth(maplka,data=dist_precipent, adm.join="NAME_1", value="Dm", breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), labels = c("(0-0.1]","(0.1-0.2]", "(0.2-0.3]", "(0.3-0.4]", "(0.4-0.5]", "(0.5-0.6]", "(0.6-0.7]", "(0.7-0.8]", "(0.8-0.9]", "(0.9-1.0]"), palette=colorRampPalette(brewer.pal(10,"YlGn"))(10),  legend="")
chorplotprecipent<-chorplotprecipent  + theme(panel.grid.major = element_line(colour = 'transparent'), 
                                              panel.background = element_blank(),
                                              axis.text = element_blank(),
                                              panel.border = element_blank(),
                                              axis.ticks = element_blank(),
                                              text = element_text(size = 24))
plot(chorplotprecipent)
#dev.off()
#Dengue
pm_dengue=matrix(0,12,numplace)

for(i in 1:numplace){
  pm_dengue[,i]=MeanMonthlyDengue[,i+1]/mean(Dengue_YearlyTS[,i])
}

Dm_dengue=matrix(0,1,numplace)

for(i in 1:numplace){
  for(j in 1:12){
    Dm_dengue[1,i]=Dm_dengue[1,i]+pm_dengue[j,i]*log(12*pm_dengue[j,i])
  }
}

dist_dengueent=cbind(places,t(Dm_dengue))
colnames(dist_dengueent)[1] <- "district"
colnames(dist_dengueent)[3] <- "latitude"
colnames(dist_dengueent)[4] <- "longitude"
colnames(dist_dengueent)[5] <- "Dm"

dist_dengueent$district <- as.character(dist_dengueent$district)
dist_dengueent <- rename(dist_dengueent, NAME_1 = district)
#tiff("DengueEntropyChorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotdengueent<-choropleth(maplka,data=dist_dengueent, adm.join="NAME_1", value="Dm", breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels = c("(0-0.05]","(0.05-0.1]", "(0.1-0.15]", "(0.15-0.2]", "(0.2-0.25]", "(0.25-0.3]"), palette=colorRampPalette(brewer.pal(6,"YlGn"))(6),  legend="")
chorplotdengueent<-chorplotdengueent  + theme(panel.grid.major = element_line(colour = 'transparent'), 
                                              panel.background = element_blank(),
                                              axis.text = element_blank(),
                                              panel.border = element_blank(),
                                              axis.ticks = element_blank(),
                                              text = element_text(size = 24))
plot(chorplotdengueent)
#dev.off()
########################Plots of monthly precip############################################
i=10
monthlyplot<-cbind.data.frame(seq(1,12,1),MeanMonthlyPrecip[,i+1],SDMonthlyPrecip[,i+1])

colnames(monthlyplot)[1] <- "month"
colnames(monthlyplot)[2] <- "mean"
colnames(monthlyplot)[3] <- "sd"
par(mar=c(1,1,1,1)+2, tck=-0.03)
par(mgp=c(1.5, 0.5, 0))
par(mfrow=c(1,1))

districtmonthlyplot<-ggplot(data=monthlyplot, aes(x=month, y=mean)) +  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="grey70") +geom_line(colour='black', size=1.5) +
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="",limits=c(-100,700))
p2<-ggplot(data=monthlyplot, aes(x=month, y=mean)) +  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="grey70") +geom_line(colour='black', size=1.5) +
  theme_bw(base_size=24) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="",limits=c(-100,700))
p3<-ggplot(data=monthlyplot, aes(x=month, y=mean)) +  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="grey70") +geom_line(colour='black', size=1.5) +
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="",limits=c(-100,700))

#tiff("PrecipJaffna_MaxEntropy.tiff",units="in",width=5,height=6,res=300,compression='lzw')
ggarrange(districtmonthlyplot, p2, p3, ncol = 1, nrow = 3)
#dev.off()
########################Plots of monthly dengue############################################
i=11
monthlyplot<-cbind.data.frame(seq(1,12,1),MeanMonthlyDengue[,i+1],SDMonthlyDengue[,i+1])

colnames(monthlyplot)[1] <- "month"
colnames(monthlyplot)[2] <- "mean"
colnames(monthlyplot)[3] <- "sd"
par(mar=c(1,1,1,1)+2, tck=-0.03)
par(mgp=c(1.5, 0.5, 0))
par(mfrow=c(1,1))

districtmonthlyplot<-ggplot(data=monthlyplot, aes(x=month, y=mean)) +  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="grey70") +geom_line(colour='black', size=1.5) +
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="",limits=c(-5,20))
p2<-ggplot(data=monthlyplot, aes(x=month, y=mean)) +  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="grey70") +geom_line(colour='black', size=1.5) +
  theme_bw(base_size=24) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="Month", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="Dengue Cases",limits=c(-5,20))
p3<-ggplot(data=monthlyplot, aes(x=month, y=mean)) +  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="grey70") +geom_line(colour='black', size=1.5) +
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="Month", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="",limits=c(-5,20))

ggarrange(districtmonthlyplot, p2, p3, ncol = 1, nrow = 3)

########################Monthly Mean Cases and Precip Nationally#####################################
#National SE and Mean of dengue
SDDengueNation<-matrix(0,1,12)
MeanDengueNation<-matrix(0,1,12)
for(i in 1:12){
  SDDengueNation[1,i]=sd(as.numeric(MeanMonthlyDengue[i,2:(numplace+1)]))/sqrt(numplace)
  MeanDengueNation[1,i]=mean(as.numeric(MeanMonthlyDengue[i,2:(numplace+1)]))
}
#National SE and Mean of precip
SDPrecipNation<-matrix(0,1,12)
MeanPrecipNation<-matrix(0,1,12)
for(i in 1:12){
  SDPrecipNation[1,i]=sd(as.numeric(MeanMonthlyPrecip[i,2:(numplace+1)]))/sqrt(numplace)
  MeanPrecipNation[1,i]=mean(as.numeric(MeanMonthlyPrecip[i,2:(numplace+1)]))
}
#National SE and Mean of tavg
SDTavgNation<-matrix(0,1,12)
MeanTavgNation<-matrix(0,1,12)
for(i in 1:12){
  SDTavgNation[1,i]=sd(as.numeric(MeanMonthlyTavg[i,2:(numplace+1)]))/sqrt(numplace)
  MeanTavgNation[1,i]=mean(as.numeric(MeanMonthlyTavg[i,2:(numplace+1)]))
}

NationalMeanSE<-cbind.data.frame(seq(1,12,1),t(MeanDengueNation),t(SDDengueNation),t(MeanPrecipNation),t(SDPrecipNation),t(MeanTavgNation),t(SDTavgNation))
colnames(NationalMeanSE)[1] <- "month"
colnames(NationalMeanSE)[2] <- "meandengue"
colnames(NationalMeanSE)[3] <- "sddengue"
colnames(NationalMeanSE)[4] <- "meanprecip"
colnames(NationalMeanSE)[5] <- "sdprecip"
colnames(NationalMeanSE)[6] <- "meantavg"
colnames(NationalMeanSE)[7] <- "sdtavg"

p1<- ggplot(data=NationalMeanSE, aes(x=month, y=meandengue)) + geom_ribbon(aes(ymin=meandengue-sddengue,ymax=meandengue+sddengue),fill="grey70") +geom_line(colour='black', size=1.5) +
  geom_vline(xintercept=c(1,7),col="grey80",lty=1,lwd=1.5) + geom_vline(xintercept=c(5,11),col="grey80",lty=2,lwd=1.5) + 
  theme_bw(base_size=24) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="", limits=c(0,400)) + theme(plot.margin = unit(c(0.3,0.5,0.3,0.5), "lines"))

p2<- ggplot(data=NationalMeanSE, aes(x=month, y=meanprecip)) + geom_ribbon(aes(ymin=meanprecip-sdprecip,ymax=meanprecip+sdprecip),fill="grey70") +geom_line(colour='black', size=1.5) +
  geom_vline(xintercept=c(1,7),col="grey80",lty=1,lwd=1.5) + geom_vline(xintercept=c(5,11),col="grey80",lty=2,lwd=1.5) + 
  theme_bw(base_size=24) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="", limits=c(0,400)) + theme(plot.margin = unit(c(0.3,0.5,0.3,0.5), "lines"))

p3<- ggplot(data=NationalMeanSE, aes(x=month, y=meantavg)) + geom_ribbon(aes(ymin=meantavg-sdtavg,ymax=meantavg+sdtavg),fill="grey70") +geom_line(colour='black', size=1.5) +
  geom_vline(xintercept=c(1,7),col="grey80",lty=1,lwd=1.5) + geom_vline(xintercept=c(5,11),col="grey80",lty=2,lwd=1.5) + 
  theme_bw(base_size=24) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(name ="", breaks=seq(1,12,1), labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) + 
  scale_y_continuous(name ="", limits=c(20,30)) + theme(plot.margin = unit(c(0.3,0.5,0.3,0.5), "lines"))

#tiff("DenguePrecipTempNational.tiff",units="in",width=5,height=7,res=300,compression='lzw')
ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
#dev.off()

#########################Choropleth region ratio 2016:mean 2010-2015#################################
Dengue_YearlyRatio_2016ToMean=matrix(0,1,numplace)
for(i in 1:numplace){
  Dengue_YearlyRatio_2016ToMean[1,i] <- Dengue_YearlyTS[7,i]/mean(Dengue_YearlyTS[1:6,i])
}

dist_dengueyearratio=cbind(places,t(Dengue_YearlyRatio_2016ToMean))
colnames(dist_dengueyearratio)[1] <- "district"
colnames(dist_dengueyearratio)[3] <- "latitude"
colnames(dist_dengueyearratio)[4] <- "longitude"
colnames(dist_dengueyearratio)[5] <- "ratio"

dist_dengueyearratio$district <- as.character(dist_dengueyearratio$district)
dist_dengueyearratio <- rename(dist_dengueyearratio, NAME_1 = district)
chorplotratio<-choropleth(maplka,data=dist_dengueyearratio, adm.join="NAME_1", breaks=c(0,0.5,1,1.5,2,2.5,3), labels = c("(0,0.5]","(0.5,1]", "(1,1.5]", "(1.5,2]", "(2,2.5]", "(2.5,3]"),value="ratio", palette=colorRampPalette(brewer.pal(6,"YlGn"))(6),  legend="")
chorplotratio<-chorplotratio  + theme(panel.grid.major = element_line(colour = 'transparent'), 
                                              panel.background = element_blank(),
                                              axis.text = element_blank(),
                                              panel.border = element_blank(),
                                              axis.ticks = element_blank(),
                                              text = element_text(size = 24))
#tiff("Ratio2016_to_20102015_Choropleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
plot(chorplotratio)
#dev.off()
#########################Choropleth region ratio 2015:mean 2010-2014#################################
Dengue_YearlyRatio_2015ToMean=matrix(0,1,numplace)
for(i in 1:numplace){
  Dengue_YearlyRatio_2015ToMean[1,i] <- Dengue_YearlyTS[6,i]/mean(Dengue_YearlyTS[1:5,i])
}

dist_dengueyearratio2015=cbind(places,t(Dengue_YearlyRatio_2015ToMean))
colnames(dist_dengueyearratio2015)[1] <- "district"
colnames(dist_dengueyearratio2015)[3] <- "latitude"
colnames(dist_dengueyearratio2015)[4] <- "longitude"
colnames(dist_dengueyearratio2015)[5] <- "ratio"

dist_dengueyearratio2015$district <- as.character(dist_dengueyearratio2015$district)
dist_dengueyearratio2015 <- rename(dist_dengueyearratio2015, NAME_1 = district)
chorplotratio2015<-choropleth(maplka,data=dist_dengueyearratio2015, adm.join="NAME_1", breaks=c(0,0.5,1,1.5,2,2.5,3), labels = c("(0,0.5]","(0.5,1]", "(1,1.5]", "(1.5,2]", "(2,2.5]", "(2.5,3]"),value="ratio", palette=colorRampPalette(brewer.pal(6,"YlGn"))(6),  legend="")
chorplotratio2015<-chorplotratio2015  + theme(panel.grid.major = element_line(colour = 'transparent'), 
                                      panel.background = element_blank(),
                                      axis.text = element_blank(),
                                      panel.border = element_blank(),
                                      axis.ticks = element_blank(),
                                      text = element_text(size = 24))

#tiff("Ratio2015_to_20102014_Choropleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
plot(chorplotratio2015)
#dev.off()