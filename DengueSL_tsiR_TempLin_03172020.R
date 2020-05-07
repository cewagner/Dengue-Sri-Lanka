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
chorplottavg<-choropleth(map,data=places_tavglag, adm.join="NAME_1", value="tavglag", palette="YlOrRd", breaks = c(-1,2,4,6,8,12,16,20,27,101), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","No significant association"),  legend="")
chorplottavg<-chorplottavg + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
plot(chorplottavg)
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
chorplotprecip<-choropleth(map,data=places_preciplag, adm.join="NAME_1", value="preciplag", palette="YlGnBu", breaks = c(-1,2,4,6,8,12,16,20,27,101), labels = c("0-4 weeks","5-8 weeks","9-12 weeks","13-16 weeks","17-24 weeks","25-32 weeks","33-40 weeks","41-52 weeks","No significant association"),  legend="")
chorplotprecip<-chorplotprecip + theme(panel.grid.major = element_line(colour = 'transparent'), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
plot(chorplotprecip)
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
##Do the panel regression
###############################################################
felmval <- felm(log(empbeta+1) ~ tavgshift + precipshift | dist_month + dist_year | 0 | district, data = regressmatrixforregshift)

coeffval<-as.numeric(felmval$coefficients)
coefftavg<-coeffval[1]
coeffprecip<-coeffval[2]
lmval <- lm(log(empbeta+1) ~ tavgshift +  precipshift + factor(dist_month) + factor(dist_year), data = regressmatrixforregshift)
lmvalall <- lm(log(empbeta+1) ~ tavgshift +  precipshift + factor(dist_month) + factor(dist_year), data = regressmatrixshift)

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
  
  IPtavg50shift[,i]<-circshift(IPtavg50[,i],mean(shiftvaltavg))
  IPtavg90shift[,i]<-circshift(IPtavg90[,i],mean(shiftvaltavg))
  
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

#tiff("EmpBeta_Proj_Linear.tiff",units="in",width=10,height=7,res=300,compression='lzw')
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
