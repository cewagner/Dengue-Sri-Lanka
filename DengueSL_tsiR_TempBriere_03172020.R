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
library(e1071)
library(xts)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(graphics)
library(stargazer)
library(grid)
library(gridExtra)

########################################################################
########################################################################
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
####################
#Future Predictions
Tavg50_MonthTS <- read.csv(file="TavgFuture_SriLanka_Monthly_20402060.csv", header=TRUE, sep=",")
Tavg50_MonthTS$date<-as.Date(Tavg50_MonthTS$date, '%m/%d/%Y')
Tavg90_MonthTS <- read.csv(file="TavgFuture_SriLanka_Monthly_20802100.csv", header=TRUE, sep=",")
Tavg90_MonthTS$date<-as.Date(Tavg90_MonthTS$date, '%m/%d/%Y')
Precip50_MonthTS <- read.csv(file="PrecipFuture_SriLanka_Monthly_20402060.csv", header=TRUE, sep=",")
Precip50_MonthTS$date<-as.Date(Precip50_MonthTS$date, '%m/%d/%Y')
Precip90_MonthTS <- read.csv(file="PrecipFuture_SriLanka_Monthly_20802100.csv", header=TRUE, sep=",")
Precip90_MonthTS$date<-as.Date(Precip90_MonthTS$date, '%m/%d/%Y')
###################
##Map Data
map <- gadm_sf_loadCountries("LKA", level=1, basefile = "./")
#Rename any places whose names don't match up
places$regionname<-as.character(places$regionname)
###For Sri Lanka
places[6,1] = "Nuwara Eliya"
places[23,1] = "Moneragala"
#tsiR data
IPdengue=2

##Values for 2017
#Tavg
Tavg_Month2017TS <- read.csv(file="Tavg_SriLanka_Monthly_20102018.csv", header=TRUE, sep=",")
Tavg_Month2017TS <-Tavg_Month2017TS [-seq(1,84,1),]
Tavg_Month2017TS <-Tavg_Month2017TS [-seq(13,24,1),]
#Precip
Precip_Month2017TS <- read.csv(file="Precip_SriLanka_Monthly_20102018.csv", header=TRUE, sep=",")
Precip_Month2017TS <-Precip_Month2017TS [-seq(1,84,1),]
Precip_Month2017TS <-Precip_Month2017TS [-seq(13,24,1),]
#Dengue
Dengue_Month2017TS <- read.csv(file="Dengue_Monthly_20102018.csv", header=TRUE, sep=",")
Dengue_Month2017TS <-Dengue_Month2017TS [-seq(1,84,1),]
Dengue_Month2017TS <-Dengue_Month2017TS [-seq(13,24,1),]
Dengue_Week2017TS <- read.csv(file="Dengue_WeeklyInterp_20102017.csv", header=TRUE, sep=",")
Dengue_Week2017TS <- Dengue_Week2017TS[-seq(1,364,1),]
for(i in 1:numplace){
  zerovec<-NULL
  zerovec=which(Dengue_Week2017TS [,i+1]<0)
  Dengue_Week2017TS[zerovec,i+1]=0
}

###########################
##Extra plotting tools
monthplotvec=matrix(0,12,1)
for(j in 1:12){
  monthplotvec[j,1]=monthplotvec[j,1]+(j-1)*1/12
}
monthplotvec<-as.numeric(monthplotvec)
monthseqvec=seq(1,12,1)

weekplotvec=matrix(0,52,1)
for(j in 1:52){
  weekplotvec[j,1]=weekplotvec[j,1]+(j-1)*1/52
}
weekplotvec<-as.numeric(weekplotvec)
weekseqvec=seq(1,52,1)

IPplotvec=matrix(0,(52/IPdengue),1)
for(j in 1:(52/IPdengue)){
  IPplotvec[j,1]=IPplotvec[j,1]+(j-1)*1/(52/IPdengue)
}
IPplotvec<-as.numeric(IPplotvec)
IPseqvec=seq(1,52,2)

####################################
#Generate monthly averages
monthmeanprecip=matrix(0,12,numplace)
monthmeantavg=matrix(0,12,numplace)
monthmeandengue=matrix(0,12,numplace)

tempsumtavg=0
tempsumprecip=0
tempsumdengue=0
for(i in 1:numplace){
  for(j in 1:12){
    for(k in 1:length(yearvec)){
      tempsumtavg=tempsumtavg+Tavg_MonthTS[(k-1)*12+j,i+1]
      tempsumprecip=tempsumprecip+Precip_MonthTS[(k-1)*12+j,i+1]  
      tempsumdengue=tempsumdengue+Dengue_MonthTS[(k-1)*12+j,i+1]
    }
    monthmeanprecip[j,i]=tempsumprecip/length(yearvec)
    monthmeantavg[j,i]=tempsumtavg/length(yearvec)
    monthmeandengue[j,i]=tempsumdengue/length(yearvec)
    tempsumtavg=0
    tempsumprecip=0
    tempsumdengue=0
  }
}
colnames(monthmeandengue)<-as.character(places[,1])
colnames(monthmeantavg)<-as.character(places[,1])
colnames(monthmeanprecip)<-as.character(places[,1])

monthvec<-seq(1,12,1)
MeanMonthlyTavg<-NULL
MeanMonthlyTavg<-cbind(monthvec,monthmeantavg)
MeanMonthlyTavg<-as.data.frame(MeanMonthlyTavg)
MeanMonthlyPrecip<-NULL
MeanMonthlyPrecip<-cbind(monthvec,monthmeanprecip)
MeanMonthlyPrecip<-as.data.frame(MeanMonthlyPrecip)
MeanMonthlyDengue<-NULL
MeanMonthlyDengue<-cbind(monthvec,monthmeandengue)
MeanMonthlyDengue<-as.data.frame(MeanMonthlyDengue)

########################################################################
########################################################################
#Do tsiR plotting
#Demography
Births_YearTS <- read.csv(file="Births_Yearly_20102016.csv", header=TRUE, sep=",")
Pop_YearTS <- read.csv(file="Population_Yearly_20092016.csv", header=TRUE, sep=",")
#Disease
Dengue_WeekTS <- read.csv(file="Dengue_WeeklyInterp_20102016.csv", header=TRUE, sep=",")
for(i in 1:numplace){
  zerovec<-NULL
  zerovec=which(Dengue_WeekTS[,i+1]<0)
  Dengue_WeekTS[zerovec,i+1]=0
}

regressmatrix=NULL
regressmatrixforreg=NULL

tsirBetamatrix=matrix(0,52/IPdengue,numplace)
empbetaMeanmatrix=matrix(0,52/IPdengue,numplace)

rhomatrix=matrix(0,1,numplace)
R2matrix=matrix(0,1,numplace)
R2betamatrix=matrix(0,1,numplace)
placereg=matrix(0,1,numplace)
numplacereg=0

for (i in 1:numplace){
  distregressmatrix=NULL
  tsirIPdata=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  DistrictParams <- estpars(data = tsirIPdata, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes <- simulatetsir(data=tsirIPdata, IP = IPdengue, parms=DistrictParams, epidemics='break', threshold=3, method='pois', nsim=100)
  #plotres(DistrictRes)
  
  tsirBetamatrix[,i]=DistrictRes$contact$beta
  rhomatrix[i] = 1 / DistrictParams$rho[1]
  ##################################################
  monthdecvec=matrix(yearvec[1],(12*length(yearvec)),1)
  for(j in 1:(12*length(yearvec))){
    monthdecvec[j,1]=monthdecvec[j,1]+(j-1)*1/12
  }
  monthdecvec<-as.numeric(monthdecvec)
  
  t=tsirIPdata$time[1:(length(tsirIPdata$time)-1)]
  tsirPeriod=length(t)
  I=tsirIPdata$cases*DistrictRes$rho
  N=tsirIPdata$pop
  a=DistrictRes$alpha
  S=DistrictRes$sbar+DistrictRes$Z
  
  R2matrix[i]<-DistrictRes$rsquared
  
  empbeta=matrix(0,length(t),1)
  for(j in 1:length(t)){
    if(I[j+1]==0){
      empbeta[j]=0
    }
    else{
      empbeta[j]=(I[j+1]*N[j])/(I[j]^a*S[j])
    }
  }

  for(j in 1:length(empbeta)){
    if(is.infinite(empbeta[j])==TRUE || is.nan(empbeta[j])==TRUE){
      if(j==1){
        k=1
        while(k<=length(empbeta)){
          if(is.finite(empbeta[j+k])==TRUE){
            empbeta[j]=empbeta[j+k]
            break
          }
          else{
            k=k+1
          }
        }
        if(k==length(empbeta) && is.finite(empbeta[k])==FALSE){
          empbeta[j]=0
          print('Warning: No finite empirical beta values')
        }
      }
      else if(j==length(empbeta)){
        empbeta[j]=empbeta[j-1]
      }
      else{
        k=1
        while(k<=(length(empbeta)-j)){
          if(is.finite(empbeta[j+k])==TRUE && is.finite(empbeta[j-k])==TRUE){
            empbeta[j]=(empbeta[j-k]+empbeta[j+k])/2
            break}
          else {k=k+1}
        }
        if(k==(length(empbeta)-j) && is.finite(empbeta[j+k])==FALSE){
          empbeta[j]=empbeta[j-1]
        }
      }
    }
  }

  empbetamean=matrix(0,52/IPdengue,1)
  for (m1 in 1:((52/IPdengue)-1)){
    for (m2 in 1:length(yearvec)){
      empbetamean[m1]= empbetamean[m1]+empbeta[m1+(m2-1)*(52/IPdengue)]
    }
    empbetamean[m1]=empbetamean[m1]/length(yearvec)
  }
  m1=(52/IPdengue)
  for (m2 in 1:(length(yearvec)-1)){
    empbetamean[m1]= empbetamean[m1]+empbeta[m1+(m2-1)*(52/IPdengue)]
  }
  empbetamean[m1]=empbetamean[m1]/(length(yearvec)-1)
  empbetaMeanmatrix[,i]=empbetamean

  empbeta<-as.data.frame(empbeta)
  empbeta<-empbeta$V1

  tavgfunc=splinefun(x=monthdecvec,y=Tavg_MonthTS[,i+1])
  tavg=tavgfunc(t)
  precipfunc=splinefun(x=monthdecvec,y=Precip_MonthTS[,i+1])
  precip=precipfunc(t)/((52/IPdengue)/12)
  precipzeroes=which(precip<0)
  precip[precipzeroes]=0
  district<-matrix(i,length(t),1)
  district<-as.data.frame(district)
  district<-district$V1
  
  testdata<-cbind.data.frame(tsirBetamatrix[,i],empbetaMeanmatrix[,i]/mean(Pop_YearTS[1:7,i+1]))
  val<-lm(testdata[,1]~testdata[,2],data=testdata)
  R2betamatrix[i]<-summary(val)$r.squared
  
  ####################################################
  distregressmatrix=cbind(distregressmatrix,t,district,empbeta,tavg,precip)
  regressmatrix=rbind(regressmatrix,distregressmatrix)
  
  if(R2betamatrix[i]>0.5){
    numplacereg=numplacereg+1
    placereg[1,numplacereg]=i
    regressmatrixforreg=rbind(regressmatrixforreg,distregressmatrix)
  }
  
}
placereg<-placereg[1,-((numplacereg+1):numplace)]
rhomatrixreg <- rhomatrix[placereg]

############################
#CHOROPLETH Reporting Rate
places_rho=cbind(places[placereg,],(rhomatrixreg*100))

colnames(places_rho)[3] <- "latitude"
colnames(places_rho)[4] <- "longitude"
colnames(places_rho)[5] <- "rho"

places_rho <- rename(places_rho, NAME_1 = regionname)
#tiff("ReportingRateChorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotrho<-choropleth(map,data=places_rho, adm.join="NAME_1", value="rho", palette="YlGnBu", breaks = c(0,5,10,15,20,25), labels = c("0-5%","5-10%","10-15%","15-20%","20-25%"),  legend="")
chorplotrho<-chorplotrho + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotrho)
#dev.off()
############################

regressmatrix<-as.data.frame(regressmatrix)
regressmatrixforreg<-as.data.frame(regressmatrixforreg)

regressmatrix$yearcol<-paste0(floor(regressmatrix$t))
regressmatrix$monthcol<-paste0(floor((regressmatrix$t-as.numeric(regressmatrix$yearcol))*12)+1)
regressmatrix$dist_month <- as.character(paste0(regressmatrix$district,"_",regressmatrix$monthcol))
regressmatrix$dist_year <- as.character(paste0(regressmatrix$district,"_",regressmatrix$yearcol))

regressmatrixforreg$yearcol<-paste0(floor(regressmatrixforreg$t))
regressmatrixforreg$monthcol<-paste0(floor((regressmatrixforreg$t-as.numeric(regressmatrixforreg$yearcol))*12)+1)
regressmatrixforreg$dist_month <- as.character(paste0(regressmatrixforreg$district,"_",regressmatrixforreg$monthcol))
regressmatrixforreg$dist_year <- as.character(paste0(regressmatrixforreg$district,"_",regressmatrixforreg$yearcol))

####Beta from TSIR (solid lid) and empirical beta averaged over the year / population (dashed line) Plot
par(mar=c(1,1,1,1), tck=-0.02)
par(mgp=c(1.1, 0.3, 0))
par(mfrow=c(4,7))
par(ps = 9, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)

for(i in 1:numplace){
  plot(IPseqvec,tsirBetamatrix[,i],type="l", main=paste(places$regionname[i]), ylab = expression(paste(beta)), xlab="Week", col="black",lwd=2)
  lines(IPseqvec,empbetaMeanmatrix[,i]/mean(Pop_YearTS[1:7,i+1]),type="l", col="black",lwd=2, lty=2)
}
#title("TSIR Prediction of Beta by District",outer = TRUE, line=-0.5)

####Histogram of all Empirical Beta values in the regression group
par(mar=c(1,1,1,1)+2, tck=-0.01)
par(mgp=c(1.7, 0.5, 0))
par(mfrow=c(1,1))
par(ps = 12, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)
hist(log(regressmatrixforreg$empbeta+1), xlim=c(0,8), ylim=c(0,1), xlab=expression(paste("log(Empirical ",beta," +1)")), ylab="Density", main="", freq=FALSE)

##########################################################
##########################################################
regressmatrixshift<-regressmatrix
regressmatrixforregshift<-regressmatrixforreg
#Find the right lags for the regression
#Tavg
maxlagvectavg=matrix(0,numplacereg)
for(i in 1:numplacereg){
  xdistvec=which(regressmatrixforreg$district==placereg[i])
  acfval<-ccf(regressmatrixforreg$empbeta[xdistvec],regressmatrixforreg$tavg[xdistvec],lag=26)
  placeacf<-as.numeric(acfval$acf)
  placelag<-as.numeric(acfval$lag)
  posspeakspos=findpeaks(placeacf)
  posspeaksneg=findpeaks(-placeacf)
  posspeaksall=rbind(posspeakspos[,1:2],posspeaksneg[,1:2])
  posspeaksall[,2]=placelag[posspeaksall[,2]]
  neglag=which(posspeaksall[,2]<0)
  posspeaks=posspeaksall[-neglag,]
  if((length(posspeaksall[,1])-length(neglag))>1){
    sigpeaks=which(posspeaks[,1]>=qnorm(1.95/2)/sqrt(length(regressmatrixforreg$empbeta[xdistvec])))
    if(length(sigpeaks)>=1){
      maxlagvectavg[i]=min(posspeaks[sigpeaks,2]) 
    }
    else{maxlagvectavg[i]=100}
  }else if((length(posspeaksall[,1])-length(neglag))==1){
    sigpeaks=which(posspeaks[1]>=qnorm(1.95/2)/sqrt(length(regressmatrixforreg$empbeta[xdistvec])))
    if(length(sigpeaks)>=1){
      maxlagvectavg[i]=min(posspeaks[2]) 
    }
    else{maxlagvectavg[i]=100}
  }
  else{
    maxlagvectavg[i]=100
  }
}
countstavg<-as.data.frame(table(maxlagvectavg))
shiftvaltavg<-as.numeric(as.vector.factor((countstavg$maxlagvectavg[which(countstavg$Freq==max(countstavg$Freq))])))
if(length(shiftvaltavg)>1 && size(which(shiftvaltavg==100))==0){
  shiftvaltavg=floor(mean(shiftvaltavg))
}
if(length(shiftvaltavg)>1 && size(which(shiftvaltavg==100))>0){
  shiftvaltavg=0
}
if(shiftvaltavg==100){
  shiftvaltavg=0
}

for(i in 1:numplacereg){
  tavgprevy<-Tavg_PrevY_MonthTS[i+1]
  tavgprevyIP<-spline(tavgprevy,n=52/IPdengue)$y
  
  xdistvec=which(regressmatrixforreg$district==placereg[i])
  
  temp<-as.numeric(lag(zoo(regressmatrixforreg$tavg[xdistvec]),-mean(shiftvaltavg),na.pad=TRUE))
  navec<-which(is.na(temp))
  temp[navec]<-tavgprevyIP[((52/IPdengue)-length(navec)+1):(52/IPdengue)]
  regressmatrixforregshift$tavg[xdistvec]<-temp
}

for(i in 1:numplace){
  tavgprevy<-Tavg_PrevY_MonthTS[i+1]
  tavgprevyIP<-spline(tavgprevy,n=52/IPdengue)$y
  
  xdistvec=which(regressmatrix$district==placereg[i])
  
  temp<-as.numeric(lag(zoo(regressmatrix$tavg[xdistvec]),-mean(shiftvaltavg),na.pad=TRUE))
  navec<-which(is.na(temp))
  temp[navec]<-tavgprevyIP[((52/IPdengue)-length(navec)+1):(52/IPdengue)]
  regressmatrixshift$tavg[xdistvec]<-temp
}

#CHOROPLETH Tavg Lag
places_tavglag=cbind(places[placereg,],maxlagvectavg)

colnames(places_tavglag)[3] <- "latitude"
colnames(places_tavglag)[4] <- "longitude"
colnames(places_tavglag)[5] <- "tavglag"

places_tavglag <- rename(places_tavglag, NAME_1 = regionname)
#tiff("TavgLagChorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplottavg<-choropleth(map,data=places_tavglag, adm.join="NAME_1", value="tavglag", palette="YlOrRd", breaks = c(-1,2,4,6,8,12,16,20,27,101), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","No significant association"),  legend="")
chorplottavg<-chorplottavg + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplottavg)
#dev.off()
#HISTOGRAM Tavg Lag
par(mar=c(1,1,1,1)+2, tck=-0.01)
par(mgp=c(1.5, 0.5, 0))
par(mfrow=c(1,1))
par(ps = 10, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)
hist(maxlagvectavg, breaks=seq(0,100,1), xlim=c(0,100), xlab="Lag [Biweeks]", ylab="Count", main="", freq=TRUE)

#Precip
maxlagvecprecip=matrix(0,numplacereg)
for(i in 1:numplacereg){
  xdistvec=which(regressmatrixforreg$district==placereg[i])
  acfval<-ccf(regressmatrixforreg$empbeta[xdistvec],regressmatrixforreg$precip[xdistvec],lag=26)
  placeacf<-as.numeric(acfval$acf)
  placelag<-as.numeric(acfval$lag)
  posspeakspos=findpeaks(placeacf)
  posspeaksneg=findpeaks(-placeacf)
  posspeaksall=rbind(posspeakspos[,1:2],posspeaksneg[,1:2])
  posspeaksall[,2]=placelag[posspeaksall[,2]]
  neglag=which(posspeaksall[,2]<0)
  posspeaks=posspeaksall[-neglag,]
  if((length(posspeaksall[,1])-length(neglag))>1){
    sigpeaks=which(posspeaks[,1]>=qnorm(1.95/2)/sqrt(length(regressmatrixforreg$empbeta[xdistvec])))
    if(length(sigpeaks)>=1){
      maxlagvecprecip[i]=min(posspeaks[sigpeaks,2]) 
    }
    else{maxlagvecprecip[i]=100}
  }else if((length(posspeaksall[,1])-length(neglag))==1){
    sigpeaks=which(posspeaks[1]>=qnorm(1.95/2)/sqrt(length(regressmatrixforreg$empbeta[xdistvec])))
    if(length(sigpeaks)>=1){
      maxlagvecprecip[i]=min(posspeaks[2]) 
    }
    else{maxlagvecprecip[i]=100}
  }
  else{
    maxlagvecprecip[i]=100
  }
}
countsprecip<-as.data.frame(table(maxlagvecprecip))
shiftvalprecip<-as.numeric(as.vector.factor((countsprecip$maxlagvecprecip[which(countsprecip$Freq==max(countsprecip$Freq))])))
if(length(shiftvalprecip)>1 && size(which(shiftvalprecip==100))==0){
  shiftvalprecip=floor(mean(shiftvalprecip))
}
if(length(shiftvalprecip)>1 && size(which(shiftvalprecip==100))>0){
  shiftvalprecip=0
}
if(shiftvalprecip==100){
  shiftvalprecip=0
}

for(i in 1:numplacereg){
  precipprevy<-Precip_PrevY_MonthTS[i+1]
  precipprevyIP<-spline(precipprevy,n=52/IPdengue)$y/((52/IPdengue)/12)
  
  xdistvec=which(regressmatrixforreg$district==placereg[i])
  
  temp<-as.numeric(lag(zoo(regressmatrixforreg$precip[xdistvec]),-mean(shiftvalprecip),na.pad=TRUE))
  navec<-which(is.na(temp))
  temp[navec]<-precipprevyIP[((52/IPdengue)-length(navec)+1):(52/IPdengue)]
  regressmatrixforregshift$precip[xdistvec]<-temp
}

for(i in 1:numplace){
  precipprevy<-Precip_PrevY_MonthTS[i+1]
  precipprevyIP<-spline(precipprevy,n=52/IPdengue)$y/((52/IPdengue)/12)
  
  xdistvec=which(regressmatrixforreg$district==i)
  
  temp<-as.numeric(lag(zoo(regressmatrix$precip[xdistvec]),-mean(shiftvalprecip),na.pad=TRUE))
  navec<-which(is.na(temp))
  temp[navec]<-precipprevyIP[((52/IPdengue)-length(navec)+1):(52/IPdengue)]
  regressmatrixshift$precip[xdistvec]<-temp
}


#CHOROPLETH Precip Lag
places_preciplag=cbind(places[placereg,],maxlagvecprecip)
colnames(places_preciplag)[3] <- "latitude"
colnames(places_preciplag)[4] <- "longitude"
colnames(places_preciplag)[5] <- "preciplag"

places_preciplag <- rename(places_preciplag, NAME_1 = regionname)
#tiff("PrecipLagChorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotprecip<-choropleth(map,data=places_preciplag, adm.join="NAME_1", value="preciplag", palette="YlGnBu", breaks = c(-1,2,4,6,8,12,16,20,27,101), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","No significant association"),  legend="")
chorplotprecip<-chorplotprecip + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotprecip)
#dev.off()
#HISTOGRAM Precip Lag
par(mar=c(1,1,1,1)+2, tck=-0.01)
par(mgp=c(1.5, 0.5, 0))
par(mfrow=c(1,1))
par(ps = 10, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)
hist(maxlagvecprecip, breaks=seq(0,100,1), xlim=c(0,100), xlab="Lag [Biweeks]", ylab="Count", main="", freq=TRUE)

###############################################################
##Construct the regression matrices
###############################################################
regressmatrixshift<-as.data.frame(regressmatrixshift)
colnames(regressmatrixshift)[4] <- "tavgshift"
colnames(regressmatrixshift)[5] <- "precipshift"

regressmatrixforregshift<-as.data.frame(regressmatrixforregshift)
colnames(regressmatrixforregshift)[4] <- "tavgshift"
colnames(regressmatrixforregshift)[5] <- "precipshift"

###############################################################
##Do binned panel regression to find functional form
###############################################################
regressmatrixforregshiftbin<-regressmatrixforregshift
makebins <- function (vec,binvec){
  bin_out <- seq(from=0,to=0, length=(length(binvec)-1))
  for (i in 1:(length(binvec)-1)){
    j=i+1
    bin_out[i] <- sum(vec >= binvec[i] & vec < binvec[j])
  }
  return <- bin_out
}

makebin <- function (vec,start,end){
  bin_out <- sum(vec >= start & vec < end)
  return <- bin_out
}

#Bin Tavg
tavgtemp <-regressmatrixforregshiftbin$tavgshift
binvec <- c(seq(23,30,1),32)
for(i in 1:(length(binvec)-1)){
  startbin <- binvec[i]
  endbin <- binvec[i+1]
  if(binvec[i] < 0){regressmatrixforregshiftbin[[paste0("tavgshift_bin","neg",-1*startbin,"_",-1*endbin)]] <- as.numeric(lapply(tavgtemp,function(x) makebin(x,startbin,endbin))) }
  if(binvec[i] >= 0){regressmatrixforregshiftbin[[paste0("tavgshift_bin",startbin,"_",endbin)]] <- as.numeric(lapply(tavgtemp,function(x) makebin(x,startbin,endbin))) }
}

#Bin Precip
preciptemp <-regressmatrixforregshiftbin$precipshift
binvec <- c(seq(0,250,50),350,550)
for(i in 1:(length(binvec)-1)){
  startbin <- binvec[i]
  endbin <- binvec[i+1]
  if(binvec[i] < 0){regressmatrixforregshiftbin[[paste0("precipshift_bin","neg",-1*startbin,"_",-1*endbin)]] <- as.numeric(lapply(preciptemp,function(x) makebin(x,startbin,endbin))) }
  if(binvec[i] >= 0){regressmatrixforregshiftbin[[paste0("precipshift_bin",startbin,"_",endbin)]] <- as.numeric(lapply(preciptemp,function(x) makebin(x,startbin,endbin))) }
}

#Summary of Precip Effect
felmpbin<-felm(log(empbeta+1) ~ tavgshift + precipshift_bin0_50 + precipshift_bin50_100 + precipshift_bin100_150 + precipshift_bin200_250 + precipshift_bin250_350 + precipshift_bin350_550 | dist_month + dist_year | 0 | district, data = regressmatrixforregshiftbin)
sumpbin<-summary(felmpbin)
centers <- c(seq(25,225,50),300,450)
coefs <- c(sumpbin$coefficients[2:4],0,sumpbin$coefficients[5:7])
ci <- 1.96*c(sumpbin$coefficients[2:4,2],0,sumpbin$coefficients[5:7,2])

#tiff("PrecipBinRegression.tiff",units="in",width=7,height=7,res=300,compression='lzw')
par(mar=c(1,1,1,1)+3, tck=-0.01)
par(mgp=c(2.1, 0.5, 0))
par(mfrow=c(1,1))
par(ps = 12, cex = 1, cex.main = 1)

plot(centers,coefs,pch=16,col="black",ylim=c(-0.2,0.3), ylab=expression(paste("Regression coefficients on ",beta)),xlab="Precipitation [mm] ",main=paste0(""),cex.axis = 1.5, cex.lab = 2)
polygon(c(centers,rev(centers)),c(coefs+ci,rev(coefs-ci)),border=NA,col=rgb(0,0.2,1,0.4))
lines(centers,coefs,lty=3,col="black")
#dev.off()
#Summary of Tavg Effect
felmtbin<-felm(log(empbeta+1) ~ tavgshift_bin23_24 + tavgshift_bin24_25 + tavgshift_bin25_26 + tavgshift_bin27_28 + tavgshift_bin28_29 + tavgshift_bin29_30 + tavgshift_bin30_32 + precipshift| dist_month + dist_year | 0 | district, data = regressmatrixforregshiftbin)
sumtbin<-summary(felmtbin)
centers <- c(seq(23.5,29.5,1),31)
coefs <- c(sumtbin$coefficients[1:3],0,sumtbin$coefficients[4:7])
ci <- 1.96*c(sumtbin$coefficients[1:3,2],0,sumtbin$coefficients[4:7,2])

#tiff("TavgBinRegression.tiff",units="in",width=7,height=7,res=300,compression='lzw')
par(mar=c(1,1,1,1)+3, tck=-0.01)
par(mgp=c(2.1, 0.5, 0))
par(mfrow=c(1,1))
par(ps = 12, cex = 1, cex.main = 1)

plot(centers,coefs,pch=16,col="black",ylim=c(-0.1,0.6), ylab=expression(paste("Regression coefficients on ",beta)), xlab="Average Temperature [ºC]",main="",cex.axis = 1.5, cex.lab = 2)
polygon(c(centers,rev(centers)),c(coefs+ci,rev(coefs-ci)),border=NA,col=rgb(0,0.2,1,0.4))
lines(centers,coefs,lty=3,col="black")
#dev.off()

#Fit T response to Briere function as in Mordecai Plos NTD 2017
briereT<-function(Temp,Tmax,Tmin,c){(c*Temp*(Temp-Tmin)*(Tmax-Temp)^0.5)}
#Tmax is taken to be the average Tmax of all Aedes aegypti traits in Mordecai Plos NTD2017
#mean of: c(40.08,34.61,38.29,39.17,37.73,35.83,37.46,45.90)
Tmaxexp=38.63
Tavgdata<-cbind.data.frame(centers,coefs)
colnames(Tavgdata)[1] <- "centers"
colnames(Tavgdata)[2] <- "coefs"

briereCont<-nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024)
briereFIT<-nls(coefs~briereT(centers,Tmaxexp,Tmin,c),data=Tavgdata,start=list(Tmin=5,c=0.001),control=briereCont,algorithm="port",lower=c(1,1e-7),upper=c(Tmaxexp,10))

#tiff("TavgBriereFit.tiff",units="in",width=7,height=7,res=300,compression='lzw')

par(mar=c(1,1,1,1)+3, tck=-0.01)
par(mgp=c(2.1, 0.5, 0))
par(mfrow=c(1,1))
par(ps = 12, cex = 1, cex.main = 1)

parsbriereout<-as.numeric(briereFIT$m$getPars())
plot(centers,coefs,xlim=c(20,40),ylim=c(-0.1,0.6),pch=16, xlab="Average Temperature [ºC]", ylab= expression(paste("Regression coefficients on ",beta)),cex.axis = 1.5, cex.lab = 2)
arrows(centers, coefs-ci, centers, coefs+ci, length=0.05, angle=90, code=3)
curve(briereT(x,Tmaxexp,parsbriereout[1],parsbriereout[2]),xlim=c(parsbriereout[1],Tmaxexp),lwd=1.5,add=TRUE)
lines(x=seq(20,parsbriereout[1],0.1),y=matrix(0,1,length(seq(20,parsbriereout[1],0.1))),lwd=1.5)
lines(x=seq(Tmaxexp,40,0.1),y=matrix(0,1,length(seq(Tmaxexp,40,0.1))),lwd=1.5)
Tminfit=parsbriereout[1]
cfit=parsbriereout[2]
#dev.off()

###############################################################
##Add in the functional form of T
###############################################################
TavgBriereFunc <- function(x){ 
  ifelse((x < Tminfit),0,ifelse((Tminfit<=x & x<=Tmaxexp),cfit*x*(x-Tminfit)*(Tmaxexp-x)^0.5, ifelse((x>Tmaxexp),0, NA))) 
} 

regressmatrixshift<-cbind(regressmatrixshift,tavgshiftbriere=TavgBriereFunc(regressmatrixshift$tavgshift))
colnames(regressmatrixshift)[10] <- "tavgshiftbriere"

regressmatrixforregshift<-cbind(regressmatrixforregshift,tavgshiftbriere=TavgBriereFunc(regressmatrixforregshift$tavgshift))
colnames(regressmatrixforregshift)[10] <- "tavgshiftbriere"

###############################################################
##Do the panel regression
###############################################################
felmval <- felm(log(empbeta+1) ~ tavgshiftbriere + precipshift | dist_month + dist_year | 0 | district, data = regressmatrixforregshift)
felmvallin <- felm(log(empbeta+1) ~ tavgshift + precipshift | dist_month + dist_year | 0 | district, data = regressmatrixforregshift)

coeffval<-as.numeric(felmval$coefficients)
coefftavg<-coeffval[1]
coeffprecip<-coeffval[2]
lmval <- lm(log(empbeta+1) ~ tavgshiftbriere +  precipshift + factor(dist_month) + factor(dist_year), data = regressmatrixforregshift)
lmvalall <- lm(log(empbeta+1) ~ tavgshiftbriere +  precipshift + factor(dist_month) + factor(dist_year), data = regressmatrixshift)

###############################################################
##Make the table with regression results
###############################################################
stargazer(felmval, felmvallin)
###############################################################
##Fix the regression model for the table that includes all places
###############################################################
if(numplacereg<numplace){
  common<-is.element(names(lmvalall$coefficients),names(lmval$coefficients))
  commontrue<-which(common=="TRUE")
  for(i in 1:length(summary(lmvalall)$coefficients[,1])){
    if(is.element(i,commontrue)=="TRUE"){
      n<-names(lmvalall$coefficients)
      spot<-which(names(lmval$coefficients)==n[i])
      lmvalall$coefficients[i]<-lmval$coefficients[spot]
    }
  }
}

###############################################################
##Generate future climate precip and tavg with IP spacing
###############################################################
IPtavg=matrix(0,(52/IPdengue),numplace)
IPtavg50=matrix(0,(52/IPdengue),numplace)
IPtavg50shift=matrix(0,(52/IPdengue),numplace)
IPtavg90=matrix(0,(52/IPdengue),numplace)
IPtavg90shift=matrix(0,(52/IPdengue),numplace)
IPtavgb50=matrix(0,(52/IPdengue),numplace)
IPtavgb50shift=matrix(0,(52/IPdengue),numplace)
IPtavgb90=matrix(0,(52/IPdengue),numplace)
IPtavgb90shift=matrix(0,(52/IPdengue),numplace)
IPprecip=matrix(0,(52/IPdengue),numplace)
IPprecip50=matrix(0,(52/IPdengue),numplace)
IPprecip50shift=matrix(0,(52/IPdengue),numplace)
IPprecip90=matrix(0,(52/IPdengue),numplace)
IPprecip90shift=matrix(0,(52/IPdengue),numplace)
for(i in 1:numplace){
  tavgplotfunc=splinefun(x=monthplotvec,y=MeanMonthlyTavg[,i+1])
  tavgplotfunc50=splinefun(x=monthplotvec,y=Tavg50_MonthTS[,i+1])
  tavgplotfunc90=splinefun(x=monthplotvec,y=Tavg90_MonthTS[,i+1])
  
  IPtavg[,i]=t(tavgplotfunc(IPplotvec))
  IPtavg50[,i]=t(tavgplotfunc50(IPplotvec))
  IPtavg90[,i]=t(tavgplotfunc90(IPplotvec))
  
  IPtavgb50[,i]=TavgBriereFunc(IPtavg50[,i])
  IPtavgb90[,i]=TavgBriereFunc(IPtavg90[,i])
  
  IPtavg50shift[,i]<-circshift(IPtavg50[,i],mean(shiftvaltavg))
  IPtavg90shift[,i]<-circshift(IPtavg90[,i],mean(shiftvaltavg))
  
  IPtavgb50shift[,i]<-TavgBriereFunc(IPtavg50shift[,i])
  IPtavgb90shift[,i]<-TavgBriereFunc(IPtavg90shift[,i])
  
  precipplotfunc=splinefun(x=monthplotvec,y=MeanMonthlyPrecip[,i+1])
  precipplotfunc50=splinefun(x=monthplotvec,y=Precip50_MonthTS[,i+1])
  precipplotfunc90=splinefun(x=monthplotvec,y=Precip90_MonthTS[,i+1])
  
  IPprecip[,i]=t(precipplotfunc(IPplotvec)/((52/IPdengue)/12))
  precipzeroes=which(IPprecip[,i]<0)
  IPprecip[precipzeroes,i]=0
  IPprecip50[,i]=t(precipplotfunc50(IPplotvec)/((52/IPdengue)/12))
  precipzeroes=which(IPprecip50[,i]<0)
  IPprecip50[precipzeroes,i]=0
  IPprecip90[,i]=t(precipplotfunc90(IPplotvec)/((52/IPdengue)/12))
  precipzeroes=which(IPprecip90[,i]<0)
  IPprecip90[precipzeroes,i]=0
  
  IPprecip50shift[,i]<-circshift(IPprecip50[,i],mean(shiftvalprecip))
  IPprecip90shift[,i]<-circshift(IPprecip90[,i],mean(shiftvalprecip))
}
##Predict empirical betas
#Generate the new data matrices with future climate data
regressmatrixshift50<-regressmatrixshift
regressmatrixshift90<-regressmatrixshift

for(i in 1:numplace){
  tavgshift50<-t(repmat(IPtavg50shift[,i],1,length(yearvec)))
  regressmatrixshift50$tavgshift[((i-1)*(length(tavgshift50)-1)+1):(i*(length(tavgshift50)-1))]<-tavgshift50[1:(length(tavgshift50)-1)]
  tavgshift90<-t(repmat(IPtavg90shift[,i],1, length(yearvec)))
  regressmatrixshift90$tavgshift[((i-1)*(length(tavgshift90)-1)+1):(i*(length(tavgshift90)-1))]<-tavgshift90[1:(length(tavgshift90)-1)]
  
  tavgbshift50<-t(repmat(IPtavgb50shift[,i],1,length(yearvec)))
  regressmatrixshift50$tavgshiftbriere[((i-1)*(length(tavgbshift50)-1)+1):(i*(length(tavgbshift50)-1))]<-tavgbshift50[1:(length(tavgbshift50)-1)]
  tavgbshift90<-t(repmat(IPtavgb90shift[,i],1, length(yearvec)))
  regressmatrixshift90$tavgshiftbriere[((i-1)*(length(tavgbshift90)-1)+1):(i*(length(tavgbshift90)-1))]<-tavgbshift90[1:(length(tavgbshift90)-1)]

  precipshift50<-t(repmat(IPprecip50shift[,i],1,length(yearvec)))
  regressmatrixshift50$precipshift[((i-1)*(length(precipshift50)-1)+1):(i*(length(precipshift50)-1))]<-precipshift50[1:(length(precipshift50)-1)]
  precipshift90<-t(repmat(IPprecip90shift[,i],1,length(yearvec)))
  regressmatrixshift90$precipshift[((i-1)*(length(precipshift90)-1)+1):(i*(length(precipshift90)-1))]<-precipshift90[1:(length(precipshift90)-1)]
}

empbetappredraw<-exp(predict(lmvalall,regressmatrixshift))-1
empbetappredall<-matrix(0,(52/IPdengue*length(yearvec)-1),numplace)
empbetappredavg<-matrix(0,(52/IPdengue),numplace)
empbeta50predraw<-exp(predict(lmvalall,regressmatrixshift50))-1
empbeta50predall<-matrix(0,(52/IPdengue*length(yearvec)-1),numplace)
empbeta50predavg<-matrix(0,(52/IPdengue),numplace)
empbeta90predraw<-exp(predict(lmvalall,regressmatrixshift90))-1
empbeta90predall<-matrix(0,(52/IPdengue*length(yearvec)-1),numplace)
empbeta90predavg<-matrix(0,(52/IPdengue),numplace)

for(i in 1:numplace){
  empbetappredall[,i]=empbetappredraw[((i-1)*(52/IPdengue*length(yearvec)-1)+1):((i-1)*(52/IPdengue*length(yearvec)-1)+(52/IPdengue*length(yearvec)-1))]
  empbeta50predall[,i]=empbeta50predraw[((i-1)*(52/IPdengue*length(yearvec)-1)+1):((i-1)*(52/IPdengue*length(yearvec)-1)+(52/IPdengue*length(yearvec)-1))]
  empbeta90predall[,i]=empbeta90predraw[((i-1)*(52/IPdengue*length(yearvec)-1)+1):((i-1)*(52/IPdengue*length(yearvec)-1)+(52/IPdengue*length(yearvec)-1))]
}

for(i in 1:numplace){
  for (m1 in 1:((52/IPdengue)-1)){
    for (m2 in 1:length(yearvec)){
      empbetappredavg[m1,i]= empbetappredavg[m1,i]+empbetappredall[m1+(m2-1)*(52/IPdengue),i]
      empbeta50predavg[m1,i]= empbeta50predavg[m1,i]+empbeta50predall[m1+(m2-1)*(52/IPdengue),i]
      empbeta90predavg[m1,i]= empbeta90predavg[m1,i]+empbeta90predall[m1+(m2-1)*(52/IPdengue),i]
    } 
    empbetappredavg[m1,i]=empbetappredavg[m1,i]/length(yearvec)
    empbeta50predavg[m1,i]=empbeta50predavg[m1,i]/length(yearvec)
    empbeta90predavg[m1,i]=empbeta90predavg[m1,i]/length(yearvec)
  }
  m1=(52/IPdengue)
  for (m2 in 1:(length(yearvec)-1)){
    empbetappredavg[m1,i]= empbetappredavg[m1,i]+empbetappredall[m1+(m2-1)*(52/IPdengue),i]
    empbeta50predavg[m1,i]= empbeta50predavg[m1,i]+empbeta50predall[m1+(m2-1)*(52/IPdengue),i]
    empbeta90predavg[m1,i]= empbeta90predavg[m1,i]+empbeta90predall[m1+(m2-1)*(52/IPdengue),i]
  } 
  empbetappredavg[m1,i]=empbetappredavg[m1,i]/(length(yearvec)-1)
  empbeta50predavg[m1,i]=empbeta50predavg[m1,i]/(length(yearvec)-1)
  empbeta90predavg[m1,i]=empbeta90predavg[m1,i]/(length(yearvec)-1)
}

#Plot the empirical betas
par(mar=c(1,1,1,1), tck=-0.02)
par(mgp=c(1.1, 0.3, 0))
par(mfrow=c(4,7))
par(ps = 8, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)
for(i in 1:numplace){
  plot(IPseqvec,empbetappredavg[,i],type="l", main=paste(places$regionname[i]), ylim=c((min(empbetappredavg[,i])-0.2),(max(empbetappredavg[,i])+0.2)), ylab = expression(paste(beta)), xlab="Week", col="black",lwd=2)
  lines(IPseqvec,empbeta50predavg[,i],type="l", lty=1, ylab = "", xlab="", col="blue3",lwd=2)
  lines(IPseqvec,empbeta90predavg[,i],type="l", lty=1, ylab = "", xlab="", col="goldenrod2",lwd=2)
}

#Plot summary of changes in empirical beta
meanempbetap<-colMeans(empbetappredavg)
meanempbeta50<-colMeans(empbeta50predavg)
meanempbeta90<-colMeans(empbeta90predavg)

#tiff("EmpBeta_Proj_Briere.tiff",units="in",width=10,height=7,res=300,compression='lzw')
par(mar=c(10,1,0,1)+3, tck=-0.01)
par(mfrow=c(1,1))
par(mgp=c(1.5, 0.5, 0))
par(ps = 16, cex = 1, cex.main=1)
plot(seq(1,25,1),meanempbetap,pch=16, cex=1.5, main="", ylim=c(0,(max(meanempbetap)+20)), ylab = "", xlab="", xaxt='n', col="black",lwd=2, cex.lab=2, cex.axis=1)
axis(1,at=seq(1,25,1),labels=places$regionname,las=2)
points(seq(1,25,1),meanempbeta50,pch=17, cex=1.5, lty=1, ylab = "", xlab="", col="blue3")
points(seq(1,25,1),meanempbeta90,pch=18, cex=1.5, lty=1, ylab = "", xlab="", col="goldenrod2")
grid(nx = 26, ny = 1, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = FALSE)
#dev.off()

#Plot changes in mean tavg and precipitation
#Plot Tavg over the year
#tiff("Tavg_Dist_PresFuture.tiff",units="in",width=35,height=20,res=300,compression='lzw')
par(mar=c(1,0,1,0)+3.5, tck=-0.02)
par(mgp=c(1.1, 1.5, 0))
par(mfrow=c(4,7))
par(ps = 12, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)
for(i in 1:numplace){
  plot(IPseqvec,IPtavg[,i],type="l", main=paste(places$regionname[i]), ylim=c((min(IPtavg[,i])-5),(max(IPtavg[,i])+5)), ylab = "", xlab="", col="black",lwd=4,cex.axis = 2, cex.main=2.5)
  lines(IPseqvec,IPtavg50[,i],type="l", lty=1, ylab = "", xlab="", col="blue3",lwd=4)
  lines(IPseqvec,IPtavg90[,i],type="l", lty=1, ylab = "", xlab="", col="goldenrod2",lwd=4)
}
#dev.off()
#Plot Precip over the year
#tiff("Precip_Dist_PresFuture.tiff",units="in",width=35,height=20,res=300,compression='lzw')
par(mar=c(1,0,1,0)+3.5, tck=-0.02)
par(mgp=c(1.1, 1.5, 0))
par(mfrow=c(4,7))
par(ps = 12, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)
for(i in 1:numplace){
  plot(IPseqvec,IPprecip[,i],type="l", main=paste(places$regionname[i]),  ylim=c(0,350), ylab = "", xlab="", col="black",lwd=4,cex.axis = 2, cex.main=2.5)
  lines(IPseqvec,IPprecip50[,i],type="l", lty=1, ylab = "", xlab="", col="blue3",lwd=4)
  lines(IPseqvec,IPprecip90[,i],type="l", lty=1, ylab = "", xlab="", col="goldenrod2",lwd=4)
}
#dev.off()
##################################################################
scatterdata2015<-matrix(0,(52/IPdengue),4)
###Decimal month biweek vector for 2015
tsirIPdata=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,2], Births_YearTS[,2], Pop_YearTS[,2], IPdengue)
tsirIPdata2015<-tsirIPdata
tsirIPdata2015<-tsirIPdata2015[-seq(1,130,1),]
tsirIPdata2015<-tsirIPdata2015[-seq(27,52,1),]
biweek_dec <- tsirIPdata2015$time
month_date <- c(ymd("2015-01-01"),ymd("2015-02-01"),ymd("2015-03-01"),ymd("2015-04-01"),ymd("2015-05-01"),ymd("2015-06-01"),ymd("2015-07-01"),ymd("2015-08-01"),ymd("2015-09-01"),ymd("2015-10-01"),ymd("2015-11-01"),ymd("2015-12-01"))
month_dec <-decimal_date(month_date)
biweek_bin<-as.numeric(cut(biweek_dec,breaks=c(month_dec,2016),include.lowest=TRUE,right=FALSE,labels=as.character(c(1:12))))
##################################################################
#Do tSIR predicting 2015 from 2010-2014
R2matrix_2015 <- matrix(0,1,numplacereg)
LSEmatrix_2015 <- matrix(0,1,numplacereg)
PeakLagmatrix_2015 <- matrix(0,1,numplacereg)
CCFmatrix_2015 <- matrix(0,1,numplacereg)

plotmat <- list()
spot <- 1
for(i in placereg) {  
  
  tsirIPdata=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  tsirIPdata20102015<-tsirIPdata
  tsirIPdata20102015<-tsirIPdata20102015[-seq(157,182,1),]
  tsirIPdata20102014<-tsirIPdata
  tsirIPdata20102014<-tsirIPdata20102014[-seq(131,182,1),]
  tsirIPdata2015<-tsirIPdata
  tsirIPdata2015<-tsirIPdata2015[-seq(1,130,1),]
  tsirIPdata2015<-tsirIPdata2015[-seq(27,52,1),]
  
  DistrictParams20102014 <- estpars(data = tsirIPdata20102014, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes20102014 <- simulatetsir(data=tsirIPdata20102014, IP = IPdengue, parms=DistrictParams20102014, epidemics='break', threshold=3, method='pois', nsim=100)
  
  I20102014=DistrictRes20102014$res$mean*DistrictParams20102014$rho
  I150<-I20102014[length(I20102014)]
  
  S20102014=DistrictRes20102014$sbar+DistrictRes20102014$Z
  S150<-S20102014[length(S20102014)]
  
  DistrictResPred20102014 <- predicttsir(times=tsirIPdata20102014$time, births=tsirIPdata20102014$births, beta=DistrictParams20102014$contact$beta, alpha=as.numeric(DistrictParams20102014$alpha), S0=S20102014[1], I0=I20102014[1], nsim=100, stochastic=T)
  DistrictResPred2015 <- predicttsir(times=tsirIPdata2015$time, births=tsirIPdata2015$births, beta=DistrictParams20102014$contact$beta, alpha=as.numeric(DistrictParams20102014$alpha), S0=S150, I0=I150, nsim=100, stochastic=T)
  
  tsirIPdata2015<-as.data.frame(tsirIPdata2015)
  tsirIPdata2015adj <- tsirIPdata2015
  tsirIPdata2015adj$cases <- tsirIPdata2015adj$cases * mean(DistrictParams20102014$rho)
  #
  tsirIPdata20102015<-as.data.frame(tsirIPdata20102015)
  tsirIPdata20102015adj <- tsirIPdata20102015
  tsirIPdata20102015adj$cases <- tsirIPdata20102015adj$cases * mean(DistrictParams20102014$rho)
  
  IPred15=cbind(DistrictResPred2015$I$time,DistrictResPred2015$I$mean)
  ISim1014=cbind(tsirIPdata20102014$time,I20102014,DistrictResPred20102014$I$mean)
  IPred15<-as.data.frame(IPred15)
  ISim1014<-as.data.frame(ISim1014)
  
  colnames(IPred15)[colnames(IPred15)=="V1"]<-"time"
  colnames(IPred15)[colnames(IPred15)=="V2"]<-"I"
  
  colnames(ISim1014)[colnames(ISim1014)=="V1"]<-"time"
  colnames(ISim1014)[colnames(ISim1014)=="V3"]<-"IPred"
  
  #tiff("Ampara_Pred2015.tiff",units="in",width=6,height=6,res=300,compression='lzw')
  #par(mar=c(1,1,1,1)+0.2, tck=-0.03)
  #par(mgp=c(1.5, 0.5, 0))
  #par(mfrow=c(1,1))
  #par(ps = 20, cex = 1, cex.main = 1)
  #op <- par(no.readonly = TRUE)
   
  #pl<-ggplot(data=ISim1014,aes(time,I20102014))+geom_line(colour="black",lwd=1)
  #pl<-pl+geom_line(data=IPred15,aes(x=time,y=I), colour="darkred", linetype=2,lwd=1)
  #pl<-pl+geom_line(data=tsirIPdata20102015, aes(x=time, y=cases*mean(DistrictParams20102014$rho)), colour="blue3",lwd=1)
  #pl<-pl  + theme_bw(base_size=26) + scale_x_continuous(name ="Year")+ scale_y_continuous(name ="Infected Count")
  # ###For a zoom-in of Vavuniya
  # #pl<-pl + theme_bw(base_size=34) + scale_x_continuous(name="", limits=c(2014,2016), breaks=c(2014,2015,2016)) + scale_y_continuous(name="", limits=c(0,1500))
  #plot(pl)
  #dev.off()

  ###Plot all the predictions in one image
  plotmat[[spot]]<-ggplot(data=ISim1014,aes(time,I20102014))+geom_line(colour="black",lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=IPred15,aes(x=time,y=I), colour="darkred", linetype=2,lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=tsirIPdata20102015adj, aes(x=time, y=cases), colour="blue3",lwd=1)
  plotmat[[spot]]<-plotmat[[spot]] + scale_x_continuous(name="") + scale_y_continuous(name="") + ggtitle(paste(places$regionname[i]))
  
  testdata<-cbind.data.frame(tsirIPdata2015adj$cases,IPred15$I)
  val<-lm(testdata[,1]~testdata[,2],data=testdata)
  R2matrix_2015[spot]<-summary(val)$r.squared
  LSEmatrix_2015[spot]<-sum(abs((testdata[,2]-testdata[,1])/testdata[,2]))
  
  acfval<-ccf((testdata[,1]/max(testdata[,1])),(testdata[,2]/max(testdata[,2])),lag=26)
  placeacf<-as.numeric(acfval$acf)
  placelag<-as.numeric(acfval$lag)
  posspeakspos=findpeaks(placeacf)
  if(length(posspeakspos[,1])>=1){
    maxspot<-which(posspeakspos[,1]==max(posspeakspos[,1]))
    CCFmatrix_2015[spot]<-abs(placelag[posspeakspos[maxspot,2]])
  }else{
    CCFmatrix_2015[spot]<-1000
  }
  
  posspeakspos_t1=findpeaks(testdata[,1])
  posspeakspos_t2=findpeaks(testdata[,2])
  if(length(posspeakspos_t1[,1])>=1){
    tooearly_1<-which((posspeakspos_t1[,2]<=5) | (posspeakspos_t1[,2]>=21))
    if(length(tooearly_1)>0 && length(tooearly_1) < length(posspeakspos_t1[,1]) ){
      posspeakspos_t1 <- posspeakspos_t1[-tooearly_1,]
    }
    posspeakspos_t1<-matrix(posspeakspos_t1,ncol=4)
    maxspot_t1<-which(posspeakspos_t1[,1]==max(posspeakspos_t1[,1]))
    max_t1<-posspeakspos_t1[maxspot_t1,2]
  }else{
    max_t1<-Inf
  }
  if(length(posspeakspos_t2[,1])>=1){
    tooearly_2<-which((posspeakspos_t2[,2]<=5) | (posspeakspos_t2[,2]>=21))
    if(length(tooearly_2)>0 && length(tooearly_2) < length(posspeakspos_t2[,1]) ){
      posspeakspos_t2 <- posspeakspos_t2[-tooearly_2,]
    }
    posspeakspos_t2<-matrix(posspeakspos_t2,ncol=4)
    maxspot_t2<-which(posspeakspos_t2[,1]==max(posspeakspos_t2[,1]))
    max_t2<-posspeakspos_t2[maxspot_t2,2]
  }else{
    max_t2<-Inf
  }
  PeakLagmatrix_2015[spot]<-abs(max_t2-max_t1)
  
  spot<-spot+1
  
}
#tiff("District_All_Pred2015.tiff",units="in",width=6,height=6,res=300,compression='lzw')
do.call("grid.arrange", c(plotmat, ncol=4))
#dev.off()
##################################################################
#Plot stats for 2015 fits
#CHOROPLETH R2
places_R2_2015=cbind(places[placereg,],as.numeric(R2matrix_2015))

colnames(places_R2_2015)[3] <- "latitude"
colnames(places_R2_2015)[4] <- "longitude"
colnames(places_R2_2015)[5] <- "R2"

places_R2_2015 <- rename(places_R2_2015, NAME_1 = regionname)
#tiff("R22015Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotR22015<-choropleth(map,data=places_R2_2015, adm.join="NAME_1", value="R2", palette=colorRampPalette(brewer.pal(10,"YlGn"))(10), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), labels = c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8","0.8-0.9","0.9-1"),  legend="")
chorplotR22015<-chorplotR22015 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotR22015)
#dev.off()
################
#CHOROPLETH LSE
places_LSE_2015=cbind(places[placereg,],as.numeric(LSEmatrix_2015))

colnames(places_LSE_2015)[3] <- "latitude"
colnames(places_LSE_2015)[4] <- "longitude"
colnames(places_LSE_2015)[5] <- "LSE"

places_LSE_2015 <- rename(places_LSE_2015, NAME_1 = regionname)
#tiff("LSE2015Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotLSE2015<-choropleth(map,data=places_LSE_2015, adm.join="NAME_1", value="LSE",  palette="YlGnBu",  breaks = c(-1,10,30,100,300,1000), labels = c("0-10","11-30","31-100","101-300","301-1000"),  legend="")
chorplotLSE2015<-chorplotLSE2015 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotLSE2015)
#dev.off()
################
#CHOROPLETH Peak Lag
places_PL_2015=cbind(places[placereg,],as.numeric(PeakLagmatrix_2015))

colnames(places_PL_2015)[3] <- "latitude"
colnames(places_PL_2015)[4] <- "longitude"
colnames(places_PL_2015)[5] <- "PL"

places_PL_2015 <- rename(places_PL_2015, NAME_1 = regionname)
#tiff("PL2015Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotPL2015<-choropleth(map,data=places_PL_2015, adm.join="NAME_1", value="PL",  palette="YlGnBu",  breaks = c(-1,2,4,6,8,12,16,20,27,Inf), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","Undetermined Central Peaks"),  legend="")
chorplotPL2015<-chorplotPL2015 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotPL2015)
#dev.off()
################
#CHOROPLETH CCF
places_CCF_2015=cbind(places[placereg,],as.numeric(CCFmatrix_2015))

colnames(places_CCF_2015)[3] <- "latitude"
colnames(places_CCF_2015)[4] <- "longitude"
colnames(places_CCF_2015)[5] <- "CCF"

places_CCF_2015 <- rename(places_CCF_2015, NAME_1 = regionname)
#tiff("CCF2015Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotCCF2015<-choropleth(map,data=places_CCF_2015, adm.join="NAME_1", value="CCF", palette="YlGnBu",  breaks = c(-1,2,4,6,8,12,16,20,27,1001), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","No significant association"),  legend="")
chorplotCCF2015<-chorplotCCF2015 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotCCF2015)
#dev.off()

##################################################################
#Plot scatter graph for 2015 of Vavuniya and Ampara
scatterdata2015[,1]<-tsirIPdata2015$cases*mean(DistrictParams20102014$rho)
scatterdata2015[,2]<-IPred15$I
scatterdata2015[,3]<-tsirIPdata2015$cases*mean(DistrictParams20102014$rho)
scatterdata2015[,4]<-IPred15$I

xseq<-seq(floor(min(scatterdata2015)),floor(max(scatterdata2015)),1)
coeffs<-matrix(0,2,2)
for(i in 1:2){
  temp<-lm(scatterdata2015[,(2*i)]~scatterdata2015[,(2*i-1)])
  tempsum<-summary(temp)
  tempsumcoeff<-as.numeric(tempsum$coefficients)
  
  coeffs[i,1]<-tempsumcoeff[1]
  coeffs[i,2]<-tempsumcoeff[2]
}

###Initial plot with all data one colour
#tiff("VavuniyaAmpara_Scatter.tiff",units="in",width=6,height=6,res=300,compression='lzw')
par(mar=c(2,2,2,2)+3, tck=-0.02)
par(mgp=c(2.5, 1, 0))
par(mfrow=c(1,1))
par(ps = 9, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)

plot(scatterdata2015[,1],scatterdata2015[,2],type="p", pch=16, col="darkmagenta", xlim=c(floor(min(scatterdata2015)),(floor(max(scatterdata2015))+1)),ylim=c(floor(min(scatterdata2015)),(floor(max(scatterdata2015))+1)), ylab = "Predicted Incidence 2015", xlab="Observed Incidence 2015",cex.axis=1.7, cex.lab=2)
lines(xseq,(coeffs[1,1]+coeffs[1,2]*xseq),type="l", col="darkmagenta",lwd=2)
points(scatterdata2015[,3],scatterdata2015[,4],pch=17, col="goldenrod1")
lines(xseq,(coeffs[2,1]+coeffs[2,2]*xseq),type="l", col="goldenrod1",lwd=2)
lines(xseq,xseq,type="l", col="black",lwd=1,lty=2)
#dev.off()
###Post review plot with seasonal colours
#tiff("VavuniyaAmpara_Seasonal_Scatter.tiff",units="in",width=6,height=6,res=300,compression='lzw')
par(mar=c(2,2,2,2)+3, tck=-0.02)
par(mgp=c(2.5, 1, 0))
par(mfrow=c(1,1))
par(ps = 9, cex = 1, cex.main = 1)
op <- par(no.readonly = TRUE)

DJF <- which(is.element(biweek_bin,c(12,1,2))==TRUE)
MAM <-which(is.element(biweek_bin,c(3,4,5))==TRUE)
JJA <-which(is.element(biweek_bin,c(6,7,8))==TRUE)
SON <-which(is.element(biweek_bin,c(9,10,11))==TRUE)

plot(scatterdata2015[DJF,1],scatterdata2015[DJF,2],type="p", pch=16, cex=2, col="thistle1", xlim=c(floor(min(scatterdata2015)),(floor(max(scatterdata2015))+1)),ylim=c(floor(min(scatterdata2015)),(floor(max(scatterdata2015))+1)), ylab = "Predicted Incidence 2015", xlab="Observed Incidence 2015",cex.axis=1.7, cex.lab=2)
points(scatterdata2015[MAM,1],scatterdata2015[MAM,2],pch=16, cex=2, col="orchid1")
points(scatterdata2015[JJA,1],scatterdata2015[JJA,2],pch=16, cex=2, col="mediumorchid2")
points(scatterdata2015[SON,1],scatterdata2015[SON,2],pch=16, cex=2, col="darkorchid")
lines(xseq,(coeffs[1,1]+coeffs[1,2]*xseq),type="l", col="darkorchid",lwd=2)
points(scatterdata2015[DJF,3],scatterdata2015[DJF,4],pch=17, cex=2, col="wheat")
points(scatterdata2015[MAM,3],scatterdata2015[MAM,4],pch=17, cex=2, col="gold1")
points(scatterdata2015[JJA,3],scatterdata2015[JJA,4],pch=17, cex=2, col="goldenrod3")
points(scatterdata2015[SON,3],scatterdata2015[SON,4],pch=17, cex=2, col="darkgoldenrod4")
lines(xseq,(coeffs[2,1]+coeffs[2,2]*xseq),type="l", col="darkgoldenrod4",lwd=2)
lines(xseq,xseq,type="l", col="black",lwd=1,lty=2)
#dev.off()
##################################################################
#Do tSIR predicting 2016 from 2010-2015
R2matrix_2016 <- matrix(0,1,numplacereg)
LSEmatrix_2016 <- matrix(0,1,numplacereg)
PeakLagmatrix_2016 <- matrix(0,1,numplacereg)
CCFmatrix_2016 <- matrix(0,1,numplacereg)

plotmat <- list()
spot <- 1
for(i in placereg) {  

  tsirIPdata=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  tsirIPdata20102015<-tsirIPdata
  tsirIPdata20102015<-tsirIPdata20102015[-seq(157,182,1),]
  tsirIPdata2016<-tsirIPdata
  tsirIPdata2016<-tsirIPdata2016[-seq(1,156,1),]
  
  DistrictParams20102015 <- estpars(data = tsirIPdata20102015, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes20102015 <- simulatetsir(data=tsirIPdata20102015, IP = IPdengue, parms=DistrictParams20102015, epidemics='break', threshold=3, method='pois', nsim=100)

  I20102015=DistrictRes20102015$res$mean*DistrictParams20102015$rho
  I160<-I20102015[length(I20102015)]

  S20102015=DistrictRes20102015$sbar+DistrictRes20102015$Z
  S160<-S20102015[length(S20102015)]
  
  DistrictResPred20102015 <- predicttsir(times=tsirIPdata20102015$time, births=tsirIPdata20102015$births, beta=DistrictParams20102015$contact$beta, alpha=as.numeric(DistrictParams20102015$alpha), S0=S20102015[1], I0=I20102015[1], nsim=100, stochastic=T)
  DistrictResPred2016 <- predicttsir(times=tsirIPdata2016$time, births=tsirIPdata2016$births, beta=DistrictParams20102015$contact$beta, alpha=as.numeric(DistrictParams20102015$alpha), S0=S160, I0=I160, nsim=100, stochastic=T)

  #tiff("Galle_Pred2016.tiff",units="in",width=6,height=6,res=300,compression='lzw')
  par(mar=c(1,1,1,1)+0.2, tck=-0.03)
  par(mgp=c(1.5, 0.5, 0))
  par(mfrow=c(1,1))
  par(ps = 12, cex = 1, cex.main = 1)
  op <- par(no.readonly = TRUE)
  
  tsirIPdata<-as.data.frame(tsirIPdata)
  tsirIPdataadj <- tsirIPdata
  tsirIPdataadj$cases <- tsirIPdataadj$cases * mean(DistrictParams20102015$rho)
  #
  tsirIPdata2016<-as.data.frame( tsirIPdata2016)
  tsirIPdata2016adj <- tsirIPdata2016
  tsirIPdata2016adj$cases <- tsirIPdata2016adj$cases * mean(DistrictParams20102015$rho)
  
  IPred16=cbind(DistrictResPred2016$I$time,DistrictResPred2016$I$mean)
  ISim1015=cbind(tsirIPdata20102015$time,I20102015,DistrictResPred20102015$I$mean)
  IPred16<-as.data.frame(IPred16)
  ISim1015<-as.data.frame(ISim1015)
  
  colnames(IPred16)[colnames(IPred16)=="V1"]<-"time"
  colnames(IPred16)[colnames(IPred16)=="V2"]<-"I"
  
  colnames(ISim1015)[colnames(ISim1015)=="V1"]<-"time"
  colnames(ISim1015)[colnames(ISim1015)=="V3"]<-"IPred"
  
  pl<-ggplot(data=ISim1015,aes(time,I20102015))+geom_line(colour="black",lwd=1)
  #pl<-pl+geom_line(data=ISim1015,aes(x=time,y=IPred), colour="black", linetype=2,lwd=1)
  pl<-pl+geom_line(data=IPred16,aes(x=time,y=I), colour="darkred", linetype=2,lwd=1)
  pl<-pl+geom_line(data=tsirIPdata, aes(x=time, y=cases*mean(DistrictParams20102015$rho)), colour="blue3",lwd=1)
  pl<-pl  + theme_bw(base_size=26) + scale_x_continuous(name ="Year")+ scale_y_continuous(name ="Infected Count")
  #plot(pl)
  #dev.off()
  
  testdata<-cbind.data.frame(tsirIPdata2016adj$cases,IPred16$I)
  val<-lm(testdata[,1]~testdata[,2],data=testdata)
  R2matrix_2016[spot]<-summary(val)$r.squared
  LSEmatrix_2016[spot]<-sum(abs((testdata[,2]-testdata[,1])/testdata[,2]))

  acfval<-ccf((testdata[,1]/max(testdata[,1])),(testdata[,2]/max(testdata[,2])),lag=26)
  placeacf<-as.numeric(acfval$acf)
  placelag<-as.numeric(acfval$lag)
  posspeakspos=findpeaks(placeacf)
  if(length(posspeakspos[,1])>=1){
    maxspot<-which(posspeakspos[,1]==max(posspeakspos[,1]))
    CCFmatrix_2016[spot]<-abs(placelag[posspeakspos[maxspot,2]])
  }else{
    CCFmatrix_2016[spot]<-1000
  }
  
  posspeakspos_t1=findpeaks(testdata[,1])
  posspeakspos_t2=findpeaks(testdata[,2])
  if(length(posspeakspos_t1[,1])>=1){
    tooearly_1<-which((posspeakspos_t1[,2]<=5) | (posspeakspos_t1[,2]>=21))
    if(length(tooearly_1)>0 && length(tooearly_1) < length(posspeakspos_t1[,1]) ){
      posspeakspos_t1 <- posspeakspos_t1[-tooearly_1,]
    }
    posspeakspos_t1<-matrix(posspeakspos_t1,ncol=4)
    maxspot_t1<-which(posspeakspos_t1[,1]==max(posspeakspos_t1[,1]))
    max_t1<-posspeakspos_t1[maxspot_t1,2]
  }else{
    max_t1<-Inf
  }
  if(length(posspeakspos_t2[,1])>=1){
    tooearly_2<-which((posspeakspos_t2[,2]<=5) | (posspeakspos_t2[,2]>=21))
    if(length(tooearly_2)>0 && length(tooearly_2) < length(posspeakspos_t2[,1]) ){
      posspeakspos_t2 <- posspeakspos_t2[-tooearly_2,]
    }
    posspeakspos_t2<-matrix(posspeakspos_t2,ncol=4)
    maxspot_t2<-which(posspeakspos_t2[,1]==max(posspeakspos_t2[,1]))
    max_t2<-posspeakspos_t2[maxspot_t2,2]
  }else{
    max_t2<-Inf
  }
  PeakLagmatrix_2016[spot]<-abs(max_t2-max_t1)
  
  spot<-spot+1
}
##################################################################
#Plot stats for 2016 fits
#CHOROPLETH R2
places_R2_2016=cbind(places[placereg,],as.numeric(R2matrix_2016))

colnames(places_R2_2016)[3] <- "latitude"
colnames(places_R2_2016)[4] <- "longitude"
colnames(places_R2_2016)[5] <- "R2"

places_R2_2016 <- rename(places_R2_2016, NAME_1 = regionname)
#tiff("R22016Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotR22016<-choropleth(map,data=places_R2_2016, adm.join="NAME_1", value="R2", palette=colorRampPalette(brewer.pal(10,"YlGn"))(10), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), labels = c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8","0.8-0.9","0.9-1"),  legend="")
chorplotR22016<-chorplotR22016 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotR22016)
#dev.off()
################
#CHOROPLETH LSE
places_LSE_2016=cbind(places[placereg,],as.numeric(LSEmatrix_2016))

colnames(places_LSE_2016)[3] <- "latitude"
colnames(places_LSE_2016)[4] <- "longitude"
colnames(places_LSE_2016)[5] <- "LSE"

places_LSE_2016 <- rename(places_LSE_2016, NAME_1 = regionname)
#tiff("LSE2016Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotLSE2016<-choropleth(map,data=places_LSE_2016, adm.join="NAME_1", value="LSE",  palette="YlGnBu",  breaks = c(-1,10,30,100,300,1000), labels = c("0-10","11-30","31-100","101-300","301-1000"),  legend="")
chorplotLSE2016<-chorplotLSE2016 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotLSE2016)
#dev.off()
################
#CHOROPLETH Peak Lag
places_PL_2016=cbind(places[placereg,],as.numeric(PeakLagmatrix_2016))

colnames(places_PL_2016)[3] <- "latitude"
colnames(places_PL_2016)[4] <- "longitude"
colnames(places_PL_2016)[5] <- "PL"

places_PL_2016 <- rename(places_PL_2016, NAME_1 = regionname)
#tiff("PL2016Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotPL2016<-choropleth(map,data=places_PL_2016, adm.join="NAME_1", value="PL",  palette="YlGnBu",  breaks = c(-1,2,4,6,8,12,16,20,27,Inf), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","Undetermined Central Peaks"),  legend="")
chorplotPL2016<-chorplotPL2016 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotPL2016)
#dev.off()
################
#CHOROPLETH CCF
places_CCF_2016=cbind(places[placereg,],as.numeric(CCFmatrix_2016))

colnames(places_CCF_2016)[3] <- "latitude"
colnames(places_CCF_2016)[4] <- "longitude"
colnames(places_CCF_2016)[5] <- "CCF"

places_CCF_2016 <- rename(places_CCF_2016, NAME_1 = regionname)
#tiff("CCF2016Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotCCF2016<-choropleth(map,data=places_CCF_2016, adm.join="NAME_1", value="CCF", palette="YlGnBu",  breaks = c(-1,2,4,6,8,12,16,20,27,1001), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","No significant association"),  legend="")
chorplotCCF2016<-chorplotCCF2016 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotCCF2016)
#dev.off()

##################################################################
#Do tsiR predicting for 2017 
#Demography
Births_Year20102017TS <- read.csv(file="Births_Yearly_20102017.csv", header=TRUE, sep=",")
Pop_Year20102017TS <- read.csv(file="Population_Yearly_20092017.csv", header=TRUE, sep=",")
#Disease
Dengue_Week20102017TS <- read.csv(file="Dengue_WeeklyInterp_20102017.csv", header=TRUE, sep=",")
for(i in 1:numplace){
  zerovec<-NULL
  zerovec=which(Dengue_Week20102017TS[,i+1]<0)
  Dengue_Week20102017TS[zerovec,i+1]=0
}
##Values for the previous year of 2016
#Tavg
Tavg_Month2016TS <- read.csv(file="Tavg_SriLanka_Monthly_20102018.csv", header=TRUE, sep=",")
Tavg_Month2016TS <-Tavg_Month2016TS [-seq(1,72,1),]
Tavg_Month2016TS <-Tavg_Month2016TS [-seq(13,36,1),]
#Precip
Precip_Month2016TS <- read.csv(file="Precip_SriLanka_Monthly_20102018.csv", header=TRUE, sep=",")
Precip_Month2016TS <-Precip_Month2016TS [-seq(1,72,1),]
Precip_Month2016TS <-Precip_Month2016TS [-seq(13,36,1),]

##Calculate beta from model using 2017 climate info
#2016
IPtavg2016=matrix(0,(52/IPdengue),numplace)
IPtavgb2016=matrix(0,(52/IPdengue),numplace)
IPprecip2016=matrix(0,(52/IPdengue),numplace)
#2017
IPtavg2017=matrix(0,(52/IPdengue),numplace)
IPtavg2017shift=matrix(0,(52/IPdengue),numplace)
IPtavgb2017=matrix(0,(52/IPdengue),numplace)
IPtavgb2017shift=matrix(0,(52/IPdengue),numplace)
IPprecip2017=matrix(0,(52/IPdengue),numplace)
IPprecip2017shift=matrix(0,(52/IPdengue),numplace)
for(i in 1:numplace){
  
  ##2016
  #Tavg
  tavgplot2016func=splinefun(x=monthplotvec,y=Tavg_Month2016TS[,i+1])
  IPtavg2016[,i]=t(tavgplot2016func(IPplotvec))
  IPtavgb2016[,i]=TavgBriereFunc(IPtavg2016[,i])
  #Precip
  precipplot2016func=splinefun(x=monthplotvec,y=Precip_Month2016TS[,i+1])
  IPprecip2016[,i]=t(precipplot2016func(IPplotvec)/((52/IPdengue)/12))
  precipzeroes=which(IPprecip2016[,i]<0)
  IPprecip2016[precipzeroes,i]=0
  
  ##2017
  tavgplot2017func=splinefun(x=monthplotvec,y=Tavg_Month2017TS[,i+1])
  IPtavg2017[,i]=t(tavgplot2017func(IPplotvec))
  temp<-as.numeric(lag(zoo(IPtavg2017[,i]),-mean(shiftvaltavg),na.pad=TRUE))
  navec<-which(is.na(temp))
  temp[navec]<-IPtavg2016[((52/IPdengue)-length(navec)+1):(52/IPdengue),i]
  IPtavg2017shift[,i]<-temp
  
  IPtavgb2017[,i]=TavgBriereFunc(IPtavg2017[,i])
  IPtavgb2017shift[,i]<-TavgBriereFunc(IPtavg2017shift[,i])
  
  precipplot2017func=splinefun(x=monthplotvec,y=Precip_Month2017TS[,i+1])
  IPprecip2017[,i]=t(precipplot2017func(IPplotvec)/((52/IPdengue)/12))
  precipzeroes=which(IPprecip2017[,i]<0)
  IPprecip2017[precipzeroes,i]=0
  temp<-as.numeric(lag(zoo(IPprecip2017[,i]),-mean(shiftvalprecip),na.pad=TRUE))
  navec<-which(is.na(temp))
  temp[navec]<-IPprecip2016[((52/IPdengue)-length(navec)+1):(52/IPdengue),i]
  IPprecip2017shift[,i]<-temp
}

regressmatrixshift2017<-regressmatrixshift
  
for(i in 1:numplace){
  tavgshift2017<-t(repmat(IPtavg2017shift[,i],1,length(yearvec)))
  regressmatrixshift2017$tavgshift[((i-1)*(length(tavgshift2017)-1)+1):(i*(length(tavgshift2017)-1))]<-tavgshift2017[1:(length(tavgshift2017)-1)]
  
  tavgbshift2017<-t(repmat(IPtavgb2017shift[,i],1,length(yearvec)))
  regressmatrixshift2017$tavgshiftbriere[((i-1)*(length(tavgbshift2017)-1)+1):(i*(length(tavgbshift2017)-1))]<-tavgbshift2017[1:(length(tavgbshift2017)-1)]
  
  precipshift2017<-t(repmat(IPprecip2017shift[,i],1,length(yearvec)))
  regressmatrixshift2017$precipshift[((i-1)*(length(precipshift2017)-1)+1):(i*(length(precipshift2017)-1))]<-precipshift2017[1:(length(precipshift2017)-1)]
}
  
empbeta2017predraw<-exp(predict(lmvalall,regressmatrixshift2017))-1
empbeta2017predall<-matrix(0,(52/IPdengue*length(yearvec)-1),numplace)
empbeta2017predavg<-matrix(0,(52/IPdengue),numplace)
  
for(i in 1:numplace){
  empbeta2017predall[,i]=empbeta2017predraw[((i-1)*(52/IPdengue*length(yearvec)-1)+1):((i-1)*(52/IPdengue*length(yearvec)-1)+(52/IPdengue*length(yearvec)-1))]
}
  
for(i in 1:numplace){
  for (m1 in 1:((52/IPdengue)-1)){
    for (m2 in 1:length(yearvec)){
      empbeta2017predavg[m1,i]= empbeta2017predavg[m1,i]+empbeta2017predall[m1+(m2-1)*(52/IPdengue),i]
    } 
    empbeta2017predavg[m1,i]=empbeta2017predavg[m1,i]/length(yearvec)
  }
  m1=(52/IPdengue)
  for (m2 in 1:(length(yearvec)-1)){
    empbeta2017predavg[m1,i]= empbeta2017predavg[m1,i]+empbeta2017predall[m1+(m2-1)*(52/IPdengue),i]
  } 
  empbeta2017predavg[m1,i]=empbeta2017predavg[m1,i]/(length(yearvec)-1)
}
for(i in 1:numplace){
  empbeta2017predavg[,i]=empbeta2017predavg[,i]/Pop_Year20102017TS[8,(i+1)]
}

R2matrix_2017 <- matrix(0,1,numplacereg)
LSEmatrix_2017 <- matrix(0,1,numplacereg)
PeakLagmatrix_2017 <- matrix(0,1,numplacereg)
CCFmatrix_2017 <- matrix(0,1,numplacereg)

plotmat <- list()
spot <- 1

for(i in placereg) {  
#for (i in 19:19){

  tsirIPdata20102016=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  
  tsirIPdata20102017=tsiRdata(Dengue_Week20102017TS$date,Dengue_Week20102017TS[,i+1], Births_Year20102017TS[,i+1], Pop_Year20102017TS[,i+1], IPdengue)
  tsirIPdata2017<-tsirIPdata20102017
  tsirIPdata2017<-tsirIPdata2017[-seq(1,182,1),]
  
  DistrictParams20102016 <- estpars(data = tsirIPdata20102016, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes20102016 <- simulatetsir(data=tsirIPdata20102016, IP = IPdengue, parms=DistrictParams20102016, epidemics='break', threshold=3, method='pois', nsim=100)
  
  I20102016=DistrictRes20102016$res$mean*DistrictParams20102016$rho
  I170<-I20102016[length(I20102016)]
  
  S20102016=DistrictRes20102016$sbar+DistrictRes20102016$Z
  S170<-S20102016[length(S20102016)]
  
  DistrictResPred20102016 <- predicttsir(times=tsirIPdata20102016$time, births=tsirIPdata20102016$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S20102016[1], I0=I20102016[1], nsim=100, stochastic=T)

  DistrictResPred2017 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017Clim <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=empbeta2017predavg[,i], alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  
  tsirIPdata20102017<-as.data.frame(tsirIPdata20102017)
  tsirIPdata20102017adj <- tsirIPdata20102017
  tsirIPdata20102017adj$cases <- tsirIPdata20102017adj$cases * mean(DistrictParams20102016$rho)
  #
  tsirIPdata2017<-as.data.frame( tsirIPdata2017)
  tsirIPdata2017adj <- tsirIPdata2017
  tsirIPdata2017adj$cases <- tsirIPdata2017adj$cases * mean(DistrictParams20102016$rho)
  
  IPred17=cbind(DistrictResPred2017$I$time,DistrictResPred2017$I$mean,DistrictResPred2017Clim$I$mean)
  ISim1016=cbind(tsirIPdata20102016$time,I20102016,DistrictResPred20102016$I$mean)
  IPred17<-as.data.frame(IPred17)
  ISim1016<-as.data.frame(ISim1016)
  
  colnames(IPred17)[colnames(IPred17)=="V1"]<-"time"
  colnames(IPred17)[colnames(IPred17)=="V2"]<-"I"
  colnames(IPred17)[colnames(IPred17)=="V3"]<-"IClim"
  
  colnames(ISim1016)[colnames(ISim1016)=="V1"]<-"time"
  colnames(ISim1016)[colnames(ISim1016)=="V3"]<-"IPred"
  
  #tiff("Puttalam_Pred2017.tiff",units="in",width=6,height=6,res=300,compression='lzw')
  #par(mar=c(1,1,1,1)+0.2, tck=-0.03)
  #par(mgp=c(1.5, 0.5, 0))
  #par(mfrow=c(1,1))
  #par(ps = 12, cex = 1, cex.main = 1)
  #op <- par(no.readonly = TRUE)
  
  #pl<-ggplot(data=ISim1016,aes(time,I20102016))+geom_line(colour="black",lwd=1)
  #pl<-pl+geom_line(data=ISim1016,aes(x=time,y=IPred), colour="black", linetype=2,lwd=1)
  #pl<-pl+geom_line(data=IPred17,aes(x=time,y=I), colour="darkred", linetype=2,lwd=1)
  #pl<-pl+geom_line(data=IPred17,aes(x=time,y=IClim), colour="darkred",lwd=1)
  #pl<-pl+geom_line(data=tsirIPdata20102017, aes(x=time, y=cases*mean(DistrictParams20102016$rho)), colour="blue3",lwd=1)
  #pl<-pl  + theme_bw(base_size=26) + scale_x_continuous(name ="Year")+ scale_y_continuous(name ="Infected Count")
  #plot(pl)
  #dev.off()
  
  ###Plot all the predictions in one image
  plotmat[[spot]]<-ggplot(data=ISim1016,aes(time,I20102016))+geom_line(colour="black",lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=ISim1016,aes(x=time,y=IPred), colour="black", linetype=2,lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=IPred17,aes(x=time,y=I), colour="darkred", linetype=2,lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=IPred17,aes(x=time,y=IClim), colour="darkred",lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=tsirIPdata20102017adj, aes(x=time, y=cases), colour="blue3",lwd=1)
  plotmat[[spot]]<-plotmat[[spot]] + scale_x_continuous(name="") + scale_y_continuous(name="") + ggtitle(paste(places$regionname[i]))
  
  testdata<-cbind.data.frame(tsirIPdata2017adj$cases,IPred17$I)
  val<-lm(testdata[,1]~testdata[,2],data=testdata)
  R2matrix_2017[spot]<-summary(val)$r.squared
  LSEmatrix_2017[spot]<-sum(abs((testdata[,2]-testdata[,1])/testdata[,2]))

  acfval<-ccf((testdata[,1]/max(testdata[,1])),(testdata[,2]/max(testdata[,2])),lag=26)
  placeacf<-as.numeric(acfval$acf)
  placelag<-as.numeric(acfval$lag)
  posspeakspos=findpeaks(placeacf)
  if(length(posspeakspos[,1])>=1){
    maxspot<-which(posspeakspos[,1]==max(posspeakspos[,1]))
    CCFmatrix_2017[spot]<-abs(placelag[posspeakspos[maxspot,2]])
  }else{
    CCFmatrix_2017[spot]<-1000
  }
  
  posspeakspos_t1=findpeaks(testdata[,1])
  posspeakspos_t2=findpeaks(testdata[,2])
  if(length(posspeakspos_t1[,1])>=1){
    tooearly_1<-which((posspeakspos_t1[,2]<=5) | (posspeakspos_t1[,2]>=21))
    if(length(tooearly_1)>0 && length(tooearly_1) < length(posspeakspos_t1[,1]) ){
      posspeakspos_t1 <- posspeakspos_t1[-tooearly_1,]
    }
    posspeakspos_t1<-matrix(posspeakspos_t1,ncol=4)
    maxspot_t1<-which(posspeakspos_t1[,1]==max(posspeakspos_t1[,1]))
    max_t1<-posspeakspos_t1[maxspot_t1,2]
  }else{
    max_t1<-Inf
  }
  if(length(posspeakspos_t2[,1])>=1){
    tooearly_2<-which((posspeakspos_t2[,2]<=5) | (posspeakspos_t2[,2]>=21))
    if(length(tooearly_2)>0 && length(tooearly_2) < length(posspeakspos_t2[,1]) ){
      posspeakspos_t2 <- posspeakspos_t2[-tooearly_2,]
    }
    posspeakspos_t2<-matrix(posspeakspos_t2,ncol=4)
    maxspot_t2<-which(posspeakspos_t2[,1]==max(posspeakspos_t2[,1]))
    max_t2<-posspeakspos_t2[maxspot_t2,2]
  }else{
    max_t2<-Inf
  }
  PeakLagmatrix_2017[spot]<-abs(max_t2-max_t1)
  
  spot<-spot+1
}
#tiff("District_All_Pred2017_2.tiff",units="in",width=13,height=6,res=300,compression='lzw')
do.call("grid.arrange", c(plotmat, ncol=4))
#dev.off()
##################################################################
#Plot stats for 2017 fits
#CHOROPLETH R2
places_R2_2017=cbind(places[placereg,],as.numeric(R2matrix_2017))

colnames(places_R2_2017)[3] <- "latitude"
colnames(places_R2_2017)[4] <- "longitude"
colnames(places_R2_2017)[5] <- "R2"

places_R2_2017 <- rename(places_R2_2017, NAME_1 = regionname)
#tiff("R22017Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotR22017<-choropleth(map,data=places_R2_2017, adm.join="NAME_1", value="R2", palette=colorRampPalette(brewer.pal(10,"YlGn"))(10), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), labels = c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8","0.8-0.9","0.9-1"),  legend="")
chorplotR22017<-chorplotR22017 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotR22017)
#dev.off()
################
#CHOROPLETH LSE
places_LSE_2017=cbind(places[placereg,],as.numeric(LSEmatrix_2017))

colnames(places_LSE_2017)[3] <- "latitude"
colnames(places_LSE_2017)[4] <- "longitude"
colnames(places_LSE_2017)[5] <- "LSE"

places_LSE_2017 <- rename(places_LSE_2017, NAME_1 = regionname)
#tiff("LSE2017Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotLSE2017<-choropleth(map,data=places_LSE_2017, adm.join="NAME_1", value="LSE",  palette="YlGnBu",  breaks = c(-1,10,30,100,300,1000), labels = c("0-10","11-30","31-100","101-300","301-1000"),  legend="")
chorplotLSE2017<-chorplotLSE2017 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotLSE2017)
#dev.off()
################
#CHOROPLETH Peak Lag
places_PL_2017=cbind(places[placereg,],as.numeric(PeakLagmatrix_2017))

colnames(places_PL_2017)[3] <- "latitude"
colnames(places_PL_2017)[4] <- "longitude"
colnames(places_PL_2017)[5] <- "PL"

places_PL_2017 <- rename(places_PL_2017, NAME_1 = regionname)
#tiff("PL2017Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotPL2017<-choropleth(map,data=places_PL_2017, adm.join="NAME_1", value="PL",  palette="YlGnBu",  breaks = c(-1,2,4,6,8,12,16,20,27,Inf), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","Undetermined Central Peaks"),  legend="")
chorplotPL2017<-chorplotPL2017 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotPL2017)
#dev.off()
################
#CHOROPLETH CCF
places_CCF_2017=cbind(places[placereg,],as.numeric(CCFmatrix_2017))

colnames(places_CCF_2017)[3] <- "latitude"
colnames(places_CCF_2017)[4] <- "longitude"
colnames(places_CCF_2017)[5] <- "CCF"

places_CCF_2017 <- rename(places_CCF_2017, NAME_1 = regionname)
#tiff("CCF2017Chorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotCCF2017<-choropleth(map,data=places_CCF_2017, adm.join="NAME_1", value="CCF", palette="YlGnBu",  breaks = c(-1,2,4,6,8,12,16,20,27,1001), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","No significant association"),  legend="")
chorplotCCF2017<-chorplotCCF2017 + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),text = element_text(size = 18))
plot(chorplotCCF2017)
#dev.off()

# #####INCREASED SUSCEPTIBLES###########
# fracincvec<-seq(1,3,0.01)
# fracincdist<-matrix(0,1,numplace)
# ratioImaxdist<-matrix(0,1,numplace)
# SEnd2016dist<-matrix(0,1,numplace)
# for (i in 1:numplace){
#   tsirIPdata20102016=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
# 
#   tsirIPdata20102017=tsiRdata(Dengue_Week20102017TS$date,Dengue_Week20102017TS[,i+1], Births_Year20102017TS[,i+1], Pop_Year20102017TS[,i+1], IPdengue)
#   tsirIPdata2017<-tsirIPdata20102017
#   tsirIPdata2017<-tsirIPdata2017[-seq(1,182,1),]
# 
#   DistrictParams20102016 <- estpars(data = tsirIPdata20102016, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
#   DistrictRes20102016 <- simulatetsir(data=tsirIPdata20102016, IP = IPdengue, parms=DistrictParams20102016, epidemics='break', threshold=3, method='pois', nsim=100)
# 
#   I20102016=DistrictRes20102016$res$mean*DistrictParams20102016$rho
#   I170<-I20102016[length(I20102016)]
# 
#   ratioImaxtemp<-matrix(0,1,length(fracincvec))
#   for (j in 1:length(fracincvec)){
#     S20102016=DistrictRes20102016$sbar+DistrictRes20102016$Z
#     S170<-S20102016[length(S20102016)]
#     S170inc<-fracincvec[j]*S20102016[length(S20102016)]
# 
#     DistrictResPred20102016 <- predicttsir(times=tsirIPdata20102016$time, births=tsirIPdata20102016$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S20102016[1], I0=I20102016[1], nsim=100, stochastic=T)
#     DistrictResPred2017 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
#     DistrictResPred2017inc <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170inc, I0=I170, nsim=100, stochastic=T)
# 
#     par(mar=c(1,1,1,1)+0.2, tck=-0.03)
#     par(mgp=c(1.5, 0.5, 0))
#     par(mfrow=c(1,1))
#     par(ps = 7, cex = 1, cex.main = 1)
#     op <- par(no.readonly = TRUE)
# 
#     tsirIPdata20102016<-as.data.frame(tsirIPdata20102016)
#     IPred17=cbind(DistrictResPred2017$I$time,DistrictResPred2017$I$mean,DistrictResPred2017inc$I$mean)
#     ISim1016=cbind(tsirIPdata20102016$time,I20102016,DistrictResPred20102016$I$mean)
#     IPred17<-as.data.frame(IPred17)
#     ISim1016<-as.data.frame(ISim1016)
# 
#     colnames(IPred17)[colnames(IPred17)=="V1"]<-"time"
#     colnames(IPred17)[colnames(IPred17)=="V2"]<-"I"
#     colnames(IPred17)[colnames(IPred17)=="V3"]<-"Iinc"
#     colnames(ISim1016)[colnames(ISim1016)=="V1"]<-"time"
#     colnames(ISim1016)[colnames(ISim1016)=="V3"]<-"IPred"
# 
#     #pl<-ggplot(data=ISim1016,aes(time,I20102016))+geom_line(colour="black",lwd=1)
#     ##pl<-pl+geom_line(data=ISim1016,aes(x=time,y=IPred), colour="black", linetype=2,lwd=1)
#     #pl<-pl+geom_line(data=IPred17,aes(x=time,y=I), colour="darkred", linetype=1,lwd=1)
#     #pl<-pl+geom_line(data=IPred17,aes(x=time,y=Iinc), colour="darkred", linetype=2,lwd=1)
#     #pl<-pl+geom_line(data=tsirIPdata20102017, aes(x=time, y=cases*mean(DistrictParams20102016$rho)), colour="blue3",lwd=1)
#     #pl<-pl  + theme_bw(base_size=16) + scale_x_continuous(name ="Year")+ scale_y_continuous(name ="Infected Count")
#     #plot(pl)
# 
#     ratioImaxtemp[j]<-max(IPred17$Iinc)/max(tsirIPdata2017$cases*mean(DistrictParams20102016$rho))
#   }
#   spot<-which(abs(ratioImaxtemp-1)==min(abs(ratioImaxtemp-1)))
#   fracincdist[i]<-fracincvec[spot]
#   ratioImaxdist[i]<-ratioImaxtemp[spot]
#   SEnd2016dist[i]<-S170
# }
# SfracInc_dist<-as.data.frame(fracincdist)
# S20170_dist<-as.data.frame(SEnd2016dist)
# for(i in 1:numplace){
#   colnames(SfracInc_dist)[i] <- paste(places$regionname[i])
#   colnames(S20170_dist)[i] <- paste(places$regionname[i])
# }
# save(SfracInc_dist, file = "SfracInc.RData")
# save(S20170_dist, file = "S20170.RData")

##Load data file for fitted value of S at start of 2017 for immigration analysis
load("/Users/cewagner/Documents/Princeton/Climate and Disease Projects/Dengue Sri Lanka Version 3/Data Files/S20170.RData")
##Load data file for fractional increase in S
load("/Users/cewagner/Documents/Princeton/Climate and Disease Projects/Dengue Sri Lanka Version 3/Data Files/SfracInc.RData")
fracincdist<-as.numeric(SfracInc_dist)
###Choropleth of fraction increase in susceptibles
dist_sfracinc=cbind(places,fracincdist)
colnames(dist_sfracinc)[1] <- "district"
colnames(dist_sfracinc)[3] <- "latitude"
colnames(dist_sfracinc)[4] <- "longitude"
colnames(dist_sfracinc)[5] <- "sfracinc"

dist_sfracinc$district <- as.character(dist_sfracinc$district)
dist_sfracinc <- rename(dist_sfracinc, NAME_1 = district)
#tiff("SFracIncChorpleth.tiff",units="in",width=6,height=6,res=300,compression='lzw')
chorplotsfracinc<-choropleth(map,data=dist_sfracinc, adm.join="NAME_1", value="sfracinc", breaks=c(1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3), labels = c("(1-1.2]","(1.2-1.4]", "(1.4-1.6]", "(1.6-1.8]", "(1.8-2]", "(2-2.2]", "(2.2-2.4]", "(2.4-2.6]", "(2.6-2.8]", "(2.8-3]"), palette=colorRampPalette(brewer.pal(10,"YlGn"))(10),  legend="")
chorplotsfracinc<-chorplotsfracinc  + theme(panel.grid.major = element_line(colour = 'transparent'), 
                                              panel.background = element_blank(),
                                              axis.text = element_blank(),
                                              panel.border = element_blank(),
                                              axis.ticks = element_blank(),
                                              text = element_text(size = 24))
plot(chorplotsfracinc)
#dev.off()
###Plot tsir with effect of susceptible increase
for (i in 1:1){
  tsirIPdata20102016=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  
  tsirIPdata20102017=tsiRdata(Dengue_Week20102017TS$date,Dengue_Week20102017TS[,i+1], Births_Year20102017TS[,i+1], Pop_Year20102017TS[,i+1], IPdengue)
  tsirIPdata2017<-tsirIPdata20102017
  tsirIPdata2017<-tsirIPdata2017[-seq(1,182,1),]
  
  DistrictParams20102016 <- estpars(data = tsirIPdata20102016, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes20102016 <- simulatetsir(data=tsirIPdata20102016, IP = IPdengue, parms=DistrictParams20102016, epidemics='break', threshold=3, method='pois', nsim=100)
  
  I20102016=DistrictRes20102016$res$mean*DistrictParams20102016$rho
  I170<-I20102016[length(I20102016)]

  S20102016=DistrictRes20102016$sbar+DistrictRes20102016$Z
  S170<-S20102016[length(S20102016)]
  S170inc<-fracincdist[i]*S20102016[length(S20102016)]
    
  DistrictResPred20102016 <- predicttsir(times=tsirIPdata20102016$time, births=tsirIPdata20102016$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S20102016[1], I0=I20102016[1], nsim=100, stochastic=T)
  DistrictResPred2017 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017inc <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170inc, I0=I170, nsim=100, stochastic=T)
    
  #tiff("Colombo_Pred2017IncreasedS.tiff",units="in",width=6,height=6,res=300,compression='lzw')
  par(mar=c(1,1,1,1)+0.2, tck=-0.03)
  par(mgp=c(1.5, 0.5, 0))
  par(mfrow=c(1,1))
  par(ps = 12, cex = 1, cex.main = 1)
  op <- par(no.readonly = TRUE)
    
  tsirIPdata20102016<-as.data.frame(tsirIPdata20102016)
  IPred17=cbind(DistrictResPred2017$I$time,DistrictResPred2017$I$mean,DistrictResPred2017inc$I$mean)
  ISim1016=cbind(tsirIPdata20102016$time,I20102016,DistrictResPred20102016$I$mean)
  IPred17<-as.data.frame(IPred17)
  ISim1016<-as.data.frame(ISim1016)
    
  colnames(IPred17)[colnames(IPred17)=="V1"]<-"time"
  colnames(IPred17)[colnames(IPred17)=="V2"]<-"I"
  colnames(IPred17)[colnames(IPred17)=="V3"]<-"Iinc"
  colnames(ISim1016)[colnames(ISim1016)=="V1"]<-"time"
  colnames(ISim1016)[colnames(ISim1016)=="V3"]<-"IPred"
    
  pl<-ggplot(data=ISim1016,aes(time,I20102016))+geom_line(colour="black",lwd=1)
  #pl<-pl+geom_line(data=ISim1016,aes(x=time,y=IPred), colour="black", linetype=2,lwd=1)
  pl<-pl+geom_line(data=IPred17,aes(x=time,y=I), colour="darkred", linetype=2,lwd=1)
  pl<-pl+geom_line(data=IPred17,aes(x=time,y=Iinc), colour="darkred", linetype=1,lwd=1)
  pl<-pl+geom_line(data=tsirIPdata20102017, aes(x=time, y=cases*mean(DistrictParams20102016$rho)), colour="blue3",lwd=1)
  pl<-pl  + theme_bw(base_size=26) + scale_x_continuous(name ="Year")+ scale_y_continuous(name ="Infected Count")
  plot(pl)
  #dev.off()
}

###Plot tsir with effect of susceptible increase COMPARED to beta increase
for (i in 1:1){
  tsirIPdata20102016=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  
  tsirIPdata20102017=tsiRdata(Dengue_Week20102017TS$date,Dengue_Week20102017TS[,i+1], Births_Year20102017TS[,i+1], Pop_Year20102017TS[,i+1], IPdengue)
  tsirIPdata2017<-tsirIPdata20102017
  tsirIPdata2017<-tsirIPdata2017[-seq(1,182,1),]
  
  DistrictParams20102016 <- estpars(data = tsirIPdata20102016, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes20102016 <- simulatetsir(data=tsirIPdata20102016, IP = IPdengue, parms=DistrictParams20102016, epidemics='break', threshold=3, method='pois', nsim=100)
  
  I20102016=DistrictRes20102016$res$mean*DistrictParams20102016$rho
  I170<-I20102016[length(I20102016)]
  
  S20102016=DistrictRes20102016$sbar+DistrictRes20102016$Z
  S170<-S20102016[length(S20102016)]
  S170inc<-fracincdist[i]*S20102016[length(S20102016)]
  beta17inc <- fracincdist[i]* DistrictParams20102016$contact$beta
  
  DistrictResPred20102016 <- predicttsir(times=tsirIPdata20102016$time, births=tsirIPdata20102016$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S20102016[1], I0=I20102016[1], nsim=100, stochastic=T)
  DistrictResPred2017 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017_Sinc <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170inc, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017_betainc <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=beta17inc, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  
  #tiff("Colombo_Pred2017IncreasedS.tiff",units="in",width=6,height=6,res=300,compression='lzw')
  par(mar=c(1,1,1,1)+0.2, tck=-0.03)
  par(mgp=c(1.5, 0.5, 0))
  par(mfrow=c(1,1))
  par(ps = 12, cex = 1, cex.main = 1)
  op <- par(no.readonly = TRUE)
  
  tsirIPdata20102016<-as.data.frame(tsirIPdata20102016)
  IPred17=cbind(DistrictResPred2017$I$time,DistrictResPred2017$I$mean,DistrictResPred2017_Sinc$I$mean,DistrictResPred2017_betainc$I$mean)
  ISim1016=cbind(tsirIPdata20102016$time,I20102016,DistrictResPred20102016$I$mean)
  IPred17<-as.data.frame(IPred17)
  ISim1016<-as.data.frame(ISim1016)
  
  colnames(IPred17)[colnames(IPred17)=="V1"]<-"time"
  colnames(IPred17)[colnames(IPred17)=="V2"]<-"I"
  colnames(IPred17)[colnames(IPred17)=="V3"]<-"I_Sinc"
  colnames(IPred17)[colnames(IPred17)=="V4"]<-"I_betainc"
  colnames(ISim1016)[colnames(ISim1016)=="V1"]<-"time"
  colnames(ISim1016)[colnames(ISim1016)=="V3"]<-"IPred"
  
  pl<-ggplot(data=ISim1016,aes(time,I20102016))+geom_line(colour="black",lwd=1)
  #pl<-pl+geom_line(data=ISim1016,aes(x=time,y=IPred), colour="black", linetype=2,lwd=1)
  pl<-pl+geom_line(data=IPred17,aes(x=time,y=I), colour="darkred", linetype=2,lwd=1)
  pl<-pl+geom_line(data=IPred17,aes(x=time,y=I_Sinc), colour="darkred", linetype=1,lwd=1)
  pl<-pl+geom_line(data=IPred17,aes(x=time,y=I_betainc), colour="darkred", linetype=3,lwd=1)
  pl<-pl+geom_line(data=tsirIPdata20102017, aes(x=time, y=cases*mean(DistrictParams20102016$rho)), colour="blue3",lwd=1)
  pl<-pl  + theme_bw(base_size=26) + scale_x_continuous(name ="Year")+ scale_y_continuous(name ="Infected Count")
  plot(pl)
  #dev.off()
}

###Look into effect of increased climate and S
#For calculation
#For Plotting
Isum<-matrix(0,numplace,1)
Iincsum<-matrix(0,numplace,1)
IClim90sum<-matrix(0,numplace,1)
IincClim90sum<-matrix(0,numplace,1)

for (i in 1:numplace){
  tsirIPdata20102016=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  
  tsirIPdata20102017=tsiRdata(Dengue_Week20102017TS$date,Dengue_Week20102017TS[,i+1], Births_Year20102017TS[,i+1], Pop_Year20102017TS[,i+1], IPdengue)
  tsirIPdata2017<-tsirIPdata20102017
  tsirIPdata2017<-tsirIPdata2017[-seq(1,182,1),]
  
  DistrictParams20102016 <- estpars(data = tsirIPdata20102016, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes20102016 <- simulatetsir(data=tsirIPdata20102016, IP = IPdengue, parms=DistrictParams20102016, epidemics='break', threshold=3, method='pois', nsim=100)
  
  I20102016=DistrictRes20102016$res$mean*DistrictParams20102016$rho
  I170<-I20102016[length(I20102016)]
  
  S20102016=DistrictRes20102016$sbar+DistrictRes20102016$Z
  S170<-S20102016[length(S20102016)]
  S170inc<-fracincdist[i]*S20102016[length(S20102016)]
  
  empbeta90<-empbeta90predavg[,i]/Pop_Year20102017TS[8,(i+1)]
  
  DistrictResPred20102016 <- predicttsir(times=tsirIPdata20102016$time, births=tsirIPdata20102016$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S20102016[1], I0=I20102016[1], nsim=100, stochastic=T)
  DistrictResPred2017 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017inc <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170inc, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017Clim90 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=empbeta90, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017incClim90 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=empbeta90, alpha=DistrictParams20102016$alpha, S0=S170inc, I0=I170, nsim=100, stochastic=T)
  
  tsirIPdata2017<-as.data.frame(tsirIPdata2017)
  IPred17=cbind(tsirIPdata2017$cases*mean(DistrictParams20102016$rho),DistrictResPred2017$I$mean,DistrictResPred2017inc$I$mean,DistrictResPred2017Clim90$I$mean,DistrictResPred2017incClim90$I$mean)
  IPred17<-as.data.frame(IPred17)
  
  colnames(IPred17)[colnames(IPred17)=="V1"]<-"Iactual"
  colnames(IPred17)[colnames(IPred17)=="V2"]<-"I"
  colnames(IPred17)[colnames(IPred17)=="V3"]<-"Iinc"
  colnames(IPred17)[colnames(IPred17)=="V4"]<-"IClim90"
  colnames(IPred17)[colnames(IPred17)=="V5"]<-"IincClim90"
  
  Isum[i,1]<-sum(IPred17$Iactual)
  Iincsum[i,1]<-sum(IPred17$Iinc)
  IClim90sum[i,1]<-sum(IPred17$IClim90)
  IincClim90sum[i,1]<-sum(IPred17$IincClim90)
}

#tiff("TotalCases_ClimS.tiff",units="in",width=10,height=7,res=300,compression='lzw')
par(mar=c(10,1,0,1)+3, tck=-0.01)
par(mfrow=c(1,1))
par(mgp=c(1.5, 0.5, 0))
par(ps = 16, cex = 1, cex.main=1)
plot(seq(1,25,1),Isum/1000,pch=16, main="", ylim=c(0,500), ylab = "", xlab="", xaxt='n', yaxt='n', col="blue3",lwd=2, cex.lab=2, cex.axis=4)
axis(1,at=seq(1,25,1),labels=places$regionname,las=2)
axis(2,at=c(0,100,200,300,400,500),las=2)
lines(seq(1,25,1),Isum/1000,lwd=1, lty=1, ylab = "", xlab="", col="blue3")
points(seq(1,25,1),Iincsum/1000,pch=16, lty=1, ylab = "", xlab="", col="darkred")
lines(seq(1,25,1),Iincsum/1000,lwd=1, lty=3, ylab = "", xlab="", col="darkred")
points(seq(1,25,1),IClim90sum/1000,pch=16, lty=1, ylab = "", xlab="", col="goldenrod1")
lines(seq(1,25,1),IClim90sum/1000,lwd=1, lty=4, ylab = "", xlab="", col="goldenrod1")
points(seq(1,25,1),IincClim90sum/1000,pch=16, lty=1, ylab = "", xlab="", col="darkgreen")
lines(seq(1,25,1),IincClim90sum/1000,lwd=1, lty=1, ylab = "", xlab="", col="darkgreen")
grid(nx = 26, ny = 1, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = FALSE)
#dev.off()

#For Plotting
plotmat <- list()
spot <- 1
for(i in placereg) { 
#for (i in 1:1){
  tsirIPdata20102016=tsiRdata(Dengue_WeekTS$date,Dengue_WeekTS[,i+1], Births_YearTS[,i+1], Pop_YearTS[,i+1], IPdengue)
  
  tsirIPdata20102017=tsiRdata(Dengue_Week20102017TS$date,Dengue_Week20102017TS[,i+1], Births_Year20102017TS[,i+1], Pop_Year20102017TS[,i+1], IPdengue)
  tsirIPdata2017<-tsirIPdata20102017
  tsirIPdata2017<-tsirIPdata2017[-seq(1,182,1),]
  
  DistrictParams20102016 <- estpars(data = tsirIPdata20102016, IP = IPdengue, regtype = 'lm', family = 'poisson', link = 'log')
  DistrictRes20102016 <- simulatetsir(data=tsirIPdata20102016, IP = IPdengue, parms=DistrictParams20102016, epidemics='break', threshold=3, method='pois', nsim=100)
  
  I20102016=DistrictRes20102016$res$mean*DistrictParams20102016$rho
  I170<-I20102016[length(I20102016)]
  
  S20102016=DistrictRes20102016$sbar+DistrictRes20102016$Z
  S170<-S20102016[length(S20102016)]
  S170inc<-fracincdist[i]*S20102016[length(S20102016)]
  
  empbeta90<-empbeta90predavg[,i]/Pop_Year20102017TS[8,(i+1)]
  
  DistrictResPred20102016 <- predicttsir(times=tsirIPdata20102016$time, births=tsirIPdata20102016$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S20102016[1], I0=I20102016[1], nsim=100, stochastic=T)
  DistrictResPred2017 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017inc <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=DistrictParams20102016$contact$beta, alpha=DistrictParams20102016$alpha, S0=S170inc, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017Clim90 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=empbeta90, alpha=DistrictParams20102016$alpha, S0=S170, I0=I170, nsim=100, stochastic=T)
  DistrictResPred2017incClim90 <- predicttsir(times=tsirIPdata2017$time, births=tsirIPdata2017$births, beta=empbeta90, alpha=DistrictParams20102016$alpha, S0=S170inc, I0=I170, nsim=100, stochastic=T)
  
  tsirIPdata20102016<-as.data.frame(tsirIPdata20102016)
  tsirIPdata2017<-as.data.frame(tsirIPdata2017)
  IPred17=cbind(DistrictResPred2017$I$time,DistrictResPred2017$I$mean,DistrictResPred2017inc$I$mean,DistrictResPred2017Clim90$I$mean,DistrictResPred2017incClim90$I$mean)
  ISim1016=cbind(tsirIPdata20102016$time,I20102016,DistrictResPred20102016$I$mean)
  IPred17<-as.data.frame(IPred17)
  ISim1016<-as.data.frame(ISim1016)
  
  tsirIPdata2017adj <- tsirIPdata2017
  tsirIPdata2017adj$cases <- tsirIPdata2017adj$cases * mean(DistrictParams20102016$rho)
  
  colnames(IPred17)[colnames(IPred17)=="V1"]<-"time"
  colnames(IPred17)[colnames(IPred17)=="V2"]<-"I"
  colnames(IPred17)[colnames(IPred17)=="V3"]<-"Iinc"
  colnames(IPred17)[colnames(IPred17)=="V4"]<-"IClim90"
  colnames(IPred17)[colnames(IPred17)=="V5"]<-"IincClim90"
  
  colnames(ISim1016)[colnames(ISim1016)=="V1"]<-"time"
  colnames(ISim1016)[colnames(ISim1016)=="V3"]<-"IPred"
  
  #tiff("Colombo_Pred2017_SClimCombined.tiff",units="in",width=6,height=6,res=300,compression='lzw')
  #par(mar=c(1,1,1,5)+3, tck=-0.03)
  #par(mgp=c(1.5, 0.5, 0))
  #par(mfrow=c(1,1))
  #par(ps = 8, cex = 1, cex.main = 1)
  #op <- par(no.readonly = TRUE)
  
  #pl<-ggplot(data=IPred17,aes(x=time,y=I)) +geom_line(colour="black",lwd=1, linetype=2)
  #pl<-pl+geom_line(data=IPred17,aes(x=time,y=Iinc), colour="darkred", linetype=3,lwd=1)
  #pl<-pl+geom_line(data=IPred17,aes(x=time,y=IClim90), colour="goldenrod1", linetype=4,lwd=1)
  #pl<-pl+geom_line(data=IPred17,aes(x=time,y=IincClim90), colour="darkgreen", linetype=1,lwd=1)
  #pl<-pl+geom_line(data=tsirIPdata2017, aes(x=time, y=cases*mean(DistrictParams20102016$rho)), colour="blue3",lwd=1)
  #pl<-pl  + theme_bw(base_size=20) + scale_x_continuous(name ="Year", breaks=c(2017,2018),minor_breaks=c(2017.25,2017.5,2017.75))+ scale_y_continuous(name ="Infected Count") +
            #theme(plot.margin=unit(c(1,1,1,1),"cm"))
  #plot(pl)
  #dev.off()
  
  ###Plot all the predictions in one image
  plotmat[[spot]]<-ggplot(data=IPred17,aes(x=time,y=I)) +geom_line(colour="black",lwd=1, linetype=2)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=IPred17,aes(x=time,y=Iinc), colour="darkred", linetype=3,lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=IPred17,aes(x=time,y=IClim90), colour="goldenrod1", linetype=4,lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=IPred17,aes(x=time,y=IincClim90), colour="darkgreen", linetype=1,lwd=1)
  plotmat[[spot]]<-plotmat[[spot]]+geom_line(data=tsirIPdata2017adj, aes(x=time, y=cases), colour="blue3",lwd=1)
  plotmat[[spot]]<-plotmat[[spot]] + scale_x_continuous(name="",breaks=c(2017,2017.25,2017.5,2017.75,2018),labels=c("2017","","2017.5","","2018")) + scale_y_continuous(name="") + ggtitle(paste(places$regionname[i]))
  
  spot<-spot+1
}
#tiff("District_All_Future_2.tiff",units="in",width=13,height=6,res=300,compression='lzw')
do.call("grid.arrange", c(plotmat, ncol=4))
#dev.off()

