###2013 Exploratory data to reduce data


##DATA 2013
##PETROS & COBBLE
#set working directory
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA")

#Read in 2013 quadrat data
Pop13=read.csv("2013PetrosCobbleBios.csv", header=TRUE)
Pop13DF=data.frame(Pop13)
attach(Pop13DF)

#change to Date format
class(Pop13DF$Date)
Pop13DF$Date <- as.Date(Pop13DF$Date, format='%m/%d/%y')
class(Pop13DF$Date)
#check columns
unique(Site)
unique(Date)


###Develop columns to be tested in model
Pop13DF$s1mm=rowSums(Pop13DF[,10:14], na.rm=T)    
Pop13DF$s2mm=rowSums(Pop13DF[,15:17], na.rm=T)  
Pop13DF$s3mm=rowSums(Pop13DF[,18:21], na.rm=T)    
Pop13DF$s4mm=rowSums(Pop13DF[,22:26], na.rm=T)  
Pop13DF$s5mm=rowSums(Pop13DF[,27:29], na.rm=T)  
Pop13DF$s6mm=rowSums(Pop13DF[,30:31], na.rm=T)  
Pop13DF$ReproDensity=Pop13DF[,c("Ovigerous")] 
Pop13DF$CompSpace=rowSums(Pop13DF[,c("Hemigrapsus.nudus.ALL", "Hemigrapsus.oregonensis.ALL", "Cancer.antennarius.ALL", "Pachychyles.rudis.ALL","Pachygrapsus.crassipes.ALL","Leptasterias.hexaxis.ALL","Lophopanopeus.bellus.ALL","Petrolisthes.manimaculus.ALL","Anoplarchus.purpurescens.ALL","Xiphister.atropurpurescens.ALL","Kelp.Crab.ALL","Urchin.ALL","Cancer.productis.ALL","Sculpin.ALL","Brittle.Star..M.")], na.rm=T) 
Pop13DF$PredationCrabs=rowSums(Pop13DF[,c("Hemigrapsus.nudus.ALL", "Hemigrapsus.oregonensis.ALL", "Cancer.antennarius.ALL", "Pachygrapsus.crassipes.ALL","Kelp.Crab.ALL","Cancer.productis.ALL")], na.rm=T)
Pop13DF$PredationFish=rowSums(Pop13DF[,c("Anoplarchus.purpurescens.ALL","Xiphister.atropurpurescens.ALL","Sculpin.ALL")], na.rm=T)
Pop13DF$InterCompFoodDensity=Pop13DF[,c("Petrolisthes.manimaculus.ALL")]  
Pop13DF$InterCompFoodPercent=rowSums(Pop13DF[,c("spirobilids","barnacles")], na.rm=T) 
Pop13DF$IntraCompFood=rowSums(Pop13DF[,c("s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")], na.rm=T) 
Pop13DF$BareRock=Pop13DF[,c("bare.rock")] 
Pop13DF$QualityHabitat=rowSums(Pop13DF[,c("X1m2.50cm2", "X50cm2.10cm2")], na.rm=T) 
Pop13DF$TotalLiveableHabitat=rowSums(Pop13DF[,c("X1m2.50cm2", "X50cm2.10cm2","X.10cm2..course.sand","bare.cobble...2cm." )], na.rm=T) 
Pop13DF$SexRatio=Pop13DF$Females.All/Pop13DF$Males.All

Pop13DF$s1to3mm=rowSums(Pop13DF[,c("s1mm", "s2mm", "s3mm")], na.rm=T) 
Pop13DF$GregSettlement=rowSums(Pop13DF[,c("s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")], na.rm=T) 
Pop13DF$Greater10adults=rowSums(Pop13DF[,c("X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")], na.rm=T) 

###2013 OTHER FACTORS
####LARVAE 2013
#set working directory
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA")

#Read in Light trap Larval 2013 data
Larv13=read.csv("2013LarvaeSites.csv", header=TRUE)
Larv13DF=data.frame(Larv13)
attach(Larv13DF)

class(Larv13DF$Date.collected)
Larv13DF$Date.collected<- as.Date(Larv13DF$Date.collected, format='%m/%d/%y')
class(Larv13DF$Date.collected)

#Sum rows to create combined columns for crabs and fish
Larv13DF$crabsall=rowSums(Larv13DF[,4:30,36])
Larv13DF$earlycrabs=rowSums(Larv13DF[,c(4,6,9,11,13,15,16,18)])
#included Majidae with late
Larv13DF$latecrabs=rowSums(Larv13DF[,c(5,7,8,10,12,14,17,19)])
Larv13DF$allzoea=rowSums(Larv13DF[,c("earlycrabs","latecrabs")])
Larv13DF$PLcrabs=rowSums(Larv13DF[,c(20:30,36)])
Larv13DF$fish=rowSums(Larv13DF[,c(31:35)])

#READ IN 2013 PHYSICAL
#set working directory 2013
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA/2013 Physical data")

##habitat sediment is in biological sheet
wavechla13=read.csv("2013waveschla.csv", header=TRUE)
prehabtemp13=read.csv("2013HabitatTemps.csv", header=TRUE)
prewatertemp13=read.csv("2013MooringTemps.csv", header=TRUE)
slope13=read.csv("2013Slope.csv", header=TRUE)


#check that date column is read as such
class(wavechla13$Date)
wavechla13$Date.reform <- as.Date(wavechla13$Date)
class(wavechla13$Date.reform)

class(prehabtemp13$HT.Date)
prehabtemp13$Date.reform <- as.Date(prehabtemp13$HT.Date, format='%m/%d/%y')
class(prehabtemp13$Date.reform)

class(prewatertemp13$Date)
prewatertemp13$Date.reform <- as.Date(prewatertemp13$Date, format='%m/%d/%y')
class(prewatertemp13$Date.reform)


#take out dates not using NEED TO LOOK UP 
levels(prehabtemp13$HT.Date)
habtemp13a <- prehabtemp13[prehabtemp13$Date.reform > as.Date("2013-05-30"),]
habtemp13 <- habtemp13a[habtemp13a$Date.reform < as.Date("2013-08-19"),]
View(habtemp13)

levels(prewatertemp13$Date)
watertemp13a <- prewatertemp13[prewatertemp13$Date.reform > as.Date("2013-05-30"),]
watertemp13 <- watertemp13a[watertemp13a$Date.reform < as.Date("2013-08-19"),]
View(watertemp13)

melted <- melt(Pop13DF, id.vars=c("Site"), measure.vars = c("ReproDensity","CompSpace", "PredationCrabs", "PredationFish", "InterCompFoodPercent","BareRock", "QualityHabitat", "InterCompFoodDensity", "IntraCompFood", "SexRatio", "s1to3mm", "GregSettlement", "TotalLiveableHabitat", "Greater10adults","X50cm2.10cm2"), na.rm=T)
Pop13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
Pop13

melted <- melt(Larv13DF, id.vars=c("Site"), measure.vars = c("earlycrabs", "latecrabs", "PLcrabs", "crabsall","allzoea"), na.rm=T)
larv13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
larv13

melted <- melt(wavechla13, id.vars=c("Site"), measure.vars = c("Dyno.cm", "Waveht.ft", "Chla.fl", "wavetime.sec"), na.rm=T)
wavechl13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
wavechl13

melted <- melt(prehabtemp13, id.vars=c("Site","HT.Top.or.Bottom"), measure.vars = c("HT.Temp.C"), na.rm=T)
habtemp13=ddply(melted, c("HT.Top.or.Bottom", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
habtemp13

melted <- melt(prewatertemp13, id.vars=c("Site"), measure.vars = c("Water.Temp.C"), na.rm=T)
watertemp13=ddply(melted, c("Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
watertemp13

melted <- melt(slope13, id.vars=c("Site"), measure.vars = c("Slope"), na.rm=T)
slo13=ddply(melted, c("Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
slo13


Pop13cast=dcast(Pop13, Site~variable, value.var="mean")
larv13cast=dcast(larv13,Site~variable, value.var="mean")
wavechl13cast=dcast(wavechl13,Site~variable, value.var="mean")
habtemp13cast=dcast(habtemp13,Site~HT.Top.or.Bottom, value.var="mean")
watertemp13cast=dcast(watertemp13,Site~variable, value.var="mean")
slope13cast=dcast(slo13,Site~variable, value.var="mean")


merge1=merge(Pop13cast,larv13cast,by="Site", all=T)
merge2=merge(merge1,wavechl13cast,by="Site", all=T)
merge3=merge(merge2,habtemp13cast,by="Site", all=T)
merge4=merge(merge3,watertemp13cast,by="Site", all=T)
mergePop13=merge(merge4,slope13cast,by="Site", all=T)


##READ IN SITE CHARACTERISTICS
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015")
SiteInfo=read.csv("FINAL Petro site info 2013 2014.csv", header=TRUE)
SiteInfoImp=SiteInfo[,c("Site","Year","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Lat", "Lon", "Coastline.regional.50km", "Coastline.reg.10.km", "Width.average", "Average.surfzone")]
SiteInfoImpSites=subset(SiteInfoImp, Site=="AC"|Site=="AP"|Site=="FB"|Site=="TC"|Site=="PC")
SiteInfoImpSitesYear=subset(SiteInfoImpSites, Year=="2013")
mergePopFinal13=merge(mergePop13,SiteInfoImpSitesYear,by="Site")
mergePopFinal13$s1to3mmLOG=log(mergePopFinal13$s1to3mm)
mergePopFinal13$ReproDensityLOG=log(mergePopFinal13$ReproDensity)
mergePopFinal13$TotalRepro=(mergePopFinal13$ReproDensity*4)*mergePopFinal13$Size.1
mergePopFinal13$TotalRec=(mergePopFinal13$s1to3mm*4)*mergePopFinal13$Size.1
mergePopFinal13$TotalReproLOG=log(mergePopFinal13$TotalRepro)
mergePopFinal13$TotalRecLOG=log(mergePopFinal13$TotalRec)
mergePopFinal13$SizeLOG=log(mergePopFinal13$Size.1)
mergePopFinal13$Greater10adultsLOG=log(mergePopFinal13$Greater10adults)

###SEM
###HIERARCHICAL SEM
library(piecewiseSEM)
require(piecewiseSEM)

##Attach Pop13DF with Site info
PopSite13=join(Pop13DF,SiteInfo, by=c("Site","Year"))

plot(log(Slope.min)~Wavetime,PopSite13)
summary(lm(log(Slope.min)~Wavetime,SiteInfo))

#Figure out how to include all data points from wavetimes
wavemerge=merge(wavechla13,SiteInfo, by=c("Site","Year"))
waveNONA=wavemerge[complete.cases(wavemerge[ ,c("Site","Year","Wavetime", "Slope.min")]),c("Site","Year", "Wavetime","Slope.min","Slope.Max")]
plot(log(Slope.min) ~ Wavetime, data = waveNONA)
plot(Slope.min ~ Wavetime, data = waveNONA)
#best one 
plot(Slope.min ~ Wavetime, data = SiteInfo)
Wave<-lm(log(Slope.min) ~ Wavetime, data = SiteInfo)
summary(Wave)
corr <- cor.test(x=SiteInfo$Wavetime, y=SiteInfo$Slope.min, method = 'spearman')
corr

corr <- cor.test(x=SiteInfo$Wavetime, y=SiteInfo$Waveht, method = 'spearman')
corr

setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA/2013 Physical data")
waveonly=read.csv("2013WAVESONLY.csv", header=TRUE)
wavemerge=merge(waveonly,SiteInfo, by=c("Site","Year"))
plot(log(Slope.min) ~ wavetimeonly , data = wavemerge)

plot(wavetimeonly ~ Site, data = wavemerge)

hist(wavemerge$wavetimeonly)
plot(Slope.min ~ wavetimeonly, data = wavemerge)
Wave<-lm(log(Slope.min) ~ wavetimeonly, data = wavemerge)
summary(Wave)

Wave<-lme(log(wavetimeonly) ~ Slope.min, random=~1|Site, data = wavemerge)
summary(Wave)

Wave<-lme(log(Slope.min) ~ wavetimeonly, random=~1|Site, data = wavemerge)
summary(Wave)

###FULL MODEL- BEST MODEL
PopSite13NANO <- PopSite13[complete.cases(PopSite13[ ,c("Site", "PL.crabs","Surfzone.size..m.", "BotTemp", "s1to3mm","Chl.a","X50cm2.10cm2","Ovigerous", "Slope.min", "GregSettlement", "ReproDensity", "PredationCrabs","CompSpace","PredationFish","InterCompFoodPercent","InterCompFoodDensity", "IntraCompFood", "BareRock", "Wavetime", "SameGeoNum", "QualityHabitat", "Slope.average", "Width.average", "TotalLiveableHabitat", "Average.surfzone", "Difference.from.300.Northery.heading")]),c("Site", "PL.crabs","Surfzone.size..m.", "BotTemp", "s1to3mm","Chl.a","X50cm2.10cm2","Ovigerous", "Slope.min", "GregSettlement", "ReproDensity", "PredationCrabs","CompSpace","PredationFish","InterCompFoodPercent","InterCompFoodDensity", "IntraCompFood", "BareRock", "Wavetime", "GeoType", "SameGeoNum","QualityHabitat", "Slope.average","Width.average", "TotalLiveableHabitat", "Average.surfzone","Difference.from.300.Northery.heading")]
attach(PopSite13NANO)

hist(s1to3mm)
PopSite13NANO$s1to3mmLOG=log(PopSite13NANO$s1to3mm+0.00001)
hist(PopSite13NANO$s1to3mmLOG)
PopSite13NANO$OvigerousLOG=log(PopSite13NANO$Ovigerous+0.00001)
hist(PopSite13NANO$OvigerousLOG)

OvigMNB <-lme(OvigerousLOG ~ s1to3mmLOG + QualityHabitat + BareRock + BotTemp + Chl.a , random=~1|Site, data = PopSite13NANO)
summary(OvigMNB)
plot(residuals(OvigMNB))
plot(OvigMNB)
shapiro.test(PopSite13NANO$OvigerousLOG)

hist(s1to3mm)
PopSite13NANO$s1to3mmLOG=log(PopSite13NANO$s1to3mm+0.00001)
RecMNB <-lme(s1to3mmLOG ~ Wavetime + QualityHabitat, random=~1|Site, data = PopSite13NANO)
summary(RecMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(QualityHabitat ~ Wavetime + Surfzone.size..m., random=~1|Site, data = PopSite13NANO)
summary(RockMNB)

plotmodel <- psem(OvigMNB, RecMNB,RockMNB) 
summary(plotmodel)


###JUST FOR HABITAT SIZE
###FULL MODEL- BEST MODEL
PopSite13NANO <- PopSite13[complete.cases(PopSite13[ ,c("Site", "Difference.from.300.Northery.heading", "Width.average", "")]),c("Site", "PL.crabs","Surfzone.size..m.", "BotTemp", "s1to3mm","Chl.a","X50cm2.10cm2","Ovigerous", "Slope.min", "GregSettlement", "ReproDensity", "PredationCrabs","CompSpace","PredationFish","InterCompFoodPercent","InterCompFoodDensity", "IntraCompFood", "BareRock", "Wavetime", "GeoType", "SameGeoNum","QualityHabitat", "Slope.average","Width.average", "TotalLiveableHabitat", "Average.surfzone","Difference.from.300.Northery.heading")]
attach(PopSite13NANO)


##GLMER BEST MOdEL?
OvigMNB <-lme(OvigerousLOG ~ s1to3mmLOG + QualityHabitat + BareRock + BotTemp + Chl.a , random=~1|Site, data = PopSite13NANO)
summary(OvigMNB)
plot(residuals(OvigMNB))
plot(OvigMNB)
shapiro.test(PopSite13NANO$OvigerousLOG)

HabwidthMNB <-lm(Width.average ~ Difference.from.300.Northery.heading, data = S)
summary(HabwidthMNB)
plot(residuals(OvigMNB))
plot(OvigMNB)
shapiro.test(PopSite13NANO$OvigerousLOG)

hist(s1to3mm)
PopSite13NANO$s1to3mmLOG=log(PopSite13NANO$s1to3mm+0.00001)
RecMNB <-lme(s1to3mmLOG ~ Wavetime + QualityHabitat, random=~1|Site, data = PopSite13NANO)
summary(RecMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(QualityHabitat ~ Wavetime + Surfzone.size..m., random=~1|Site, data = PopSite13NANO)
summary(RockMNB)

plotmodel <- psem(OvigMNB, RecMNB,RockMNB) 
summary(plotmodel)






##New model 1
hist(s1to3mmcombined)
recruitMNB<-glmer(s1to3mmcombined ~ scale(Slope.min, scale=F)+scale(Surfzone.size..m., scale=F)+scale(Wavetime, scale=F)+(1|Site),family=poisson(), data = PopSite13NANO)
summary(recruitMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(X50cm2.10cm2 ~ Slope.min + Surfzone.size..m. , random=~1|Site, data = PopSite13NANO)
summary(RockMNB)

hist(s35mm)
s35mmMNB <-glmer(s35mm ~ scale(s1to3mmcombined, scale=F)+scale(X50cm2.10cm2, scale=F)+(1|Site), data = PopSite13NANO,family=poisson())
summary(s35mmMNB)

AllReproMNB <-glmer(ALLREPRO ~ scale(s35mm, scale=T)+scale(X50cm2.10cm2, scale=T)+(1|Site), data = PopSite13NANO, family=poisson())
summary(AllReproMNB)

OvigMNB <-glmer(Ovigerous ~ scale(ALLREPRO, scale=T)+(1|Site), data = PopSite13NANO, family=poisson())
summary(OvigMNB)

plotmodel <- psem(recruitMNB, s35mmMNB,RockMNB,AllReproMNB,OvigMNB) 
summary(plotmodel)

##New model 2
hist(s1to3mmcombined)
PopSite13NANO$s1to3mmcombinedLOG=log(PopSite13NANO$s1to3mmcombined+0.00001)
recruitMNB<-lme(s1to3mmcombinedLOG ~ Wavetime + X50cm2.10cm2 , random=~1|Site, data = PopSite13NANO)
summary(recruitMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(X50cm2.10cm2 ~ Slope.min + Surfzone.size..m. , random=~1|Site, data = PopSite13NANO)
summary(RockMNB)

hist(Ovigerous)
PopSite13NANO$OvigerousLOG=log(PopSite13NANO$Ovigerous+0.00001)
OvigMNB <-lme(OvigerousLOG ~ s1to3mmcombinedLOG + X50cm2.10cm2, random=~1|Site, data = PopSite13NANO)
summary(OvigMNB)

plotmodel <- psem(recruitMNB, RockMNB,OvigMNB) 
summary(plotmodel)


##Original model
hist(s1to3mmcombined)
recruitMNB<-glmer.nb(s1to3mmcombined ~ scale(Slope.min, scale=F)+scale(Surfzone.size..m., scale=F)+scale(Wavetime, scale=F)+(1|Site), data = PopSite13NANO)
summary(recruitMNB)

RockMNB<-glmer.nb(X50cm2.10cm2 ~ scale(Slope.min, scale=T)+scale(Surfzone.size..m., scale=T)+scale(Wavetime, scale=F)+(1|Site), data = PopSite13NANO,control=glmerControl(optimizer="bobyqa"))
summary(RockMNB)

s35mmMNB <-glmer.nb(s35mm ~ scale(s1to3mmcombined, scale=F)+scale(X50cm2.10cm2, scale=F)+(1|Site), data = PopSite13NANO)
summary(s35mmMNB)

AllReproMNB <-glmer.nb(ALLREPRO ~ scale(s35mm, scale=T)+scale(X50cm2.10cm2, scale=T)+(1|Site), data = PopSite13NANO, control=glmerControl(optimizer="bobyqa"))
summary(AllReproMNB)

OvigMNB <-glmer.nb(Ovigerous ~ scale(ALLREPRO, scale=T)+(1|Site), data = PopSite13NANO, control=glmerControl(optimizer="bobyqa"))
summary(OvigMNB)

plotmodel <- psem(recruitMNB, s35mmMNB,RockMNB) 
summary(plotmodel,conserve=T)

##TOTAL REC and REPRO
plot(TotalRecLOG~TotalReproLOG, data=mergePopFinal13)
summary(lm(TotalRecLOG~TotalReproLOG, data=mergePopFinal13))
summary(lm(Greater10adults~TotalReproLOG, data=mergePopFinal13))

###Reproduction correlations
ggpairs(mergePopFinal13[,c("ReproDensityLOG","CompSpace","PredationCrabs", "PredationFish","InterCompFoodPercent", "BareRock", "QualityHabitat", "InterCompFoodDensity", "IntraCompFood", "Bottom","Chla.fl", "Size.1", "TotalLiveableHabitat", "Width.average")])

ggpairs(mergePopFinal13[,c("Greater10adults","CompSpace","PredationCrabs", "PredationFish","InterCompFoodPercent", "BareRock", "QualityHabitat", "InterCompFoodDensity", "IntraCompFood", "Bottom","Chla.fl", "Size.1", "TotalLiveableHabitat", "Width.average")])

plot(Width.average~Surfzone.size..m., data=mergePopFinal13)
summary(lm(Width.average~Surfzone.size..m., data=mergePopFinal13))

summary(lm(Greater10adultsLOG~Width.average+s1to3mmLOG, data=mergePopFinal13))
summary(lm(ReproDensityLOG~Greater10adults, data=mergePopFinal13))
summary(lm(s1to3mmLOG~wavetime.sec, data=mergePopFinal13))

summary(lm(wavetime.sec~Surfzone.size..m., data=mergePopFinal13))

plot(wavetime.sec~Slope.average, data=mergePopFinal13)
#summary(lm(X50cm2.10cm2~Width.average, data=mergePopFinal13))

###Recruitment correlations
ggpairs(mergePopFinal13[,c("s1to3mm","GregSettlement","PLcrabs", "Dyno.cm","Waveht.ft", "BareRock", "wavetime.sec","Water.Temp.C","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S")])

ggpairs(mergePopFinal13[,c("s1to3mmLOG","GregSettlement","PLcrabs", "Dyno.cm","Waveht.ft", "BareRock", "wavetime.sec","Water.Temp.C","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S")])

summary(lm(TotalLiveableHabitat~Dyno.cm, data=mergePopFinal13))

#physical correlates
mergePopFinal13$wavetime.secLOG=log(mergePopFinal13$wavetime.sec)
ggpairs(mergePopFinal13[,c("TotalLiveableHabitat", "QualityHabitat", "wavetime.sec","Waveht.ft", "BareRock", "Dyno.cm","Water.Temp.C", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Width.average")])

ggpairs(mergePopFinal13[,c("wavetime.sec","Waveht.ft", "BareRock", "Dyno.cm","Water.Temp.C","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Width.average")])

ggpairs(mergePopFinal13[,c("SizeLOG","Waveht.ft", "BareRock", "Dyno.cm","Water.Temp.C","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "wavetime.sec")])


M <- cor(mergePopFinal13[,c("wavetime.sec","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Width.average", "s1to3mmLOG", "ReproDensityLOG")])
corrplot(M, order = "hclust",addrect = 3)
plot(s1to3mmLOG~Total.degrees.opening, data=mergePopFinal13)

summary(lm(s1to3mmLOG~Coastline.reg.0.5.km*Width.average, data=mergePopFinal13))

plot(SizeLOG~Diff.hab.angle.and.coastline.reg, data=mergePopFinal13)
summary(lm(SizeLOG~Diff.hab.angle.and.coastline.reg, data=mergePopFinal13))

summary(lm(TotalLiveableHabitat~Dyno.cm, data=mergePopFinal13))

summary(lm(Dyno.cm~Coastline.reg.0.5.km*Total.degrees.opening, data=mergePopFinal13))
summary(lm(Dyno.cm~Heading*Surfzone.size..m., data=mergePopFinal13))


##PLOTS
plot(mergePopFinal13$Coastline.reg.0.5.km~mergePopFinal13$wavetime.sec)

##MODELS
summary(lm(s1to3mmLOG~wavetime.sec, data=mergePopFinal13))
summary(lm(wavetime.sec~Difference.from.300.Northery.heading*Total.degrees.opening, data=mergePopFinal13))

plot(ReproDensityLOG~TotalLiveableHabitat, data=mergePopFinal13)
summary(lm(ReproDensityLOG~TotalLiveableHabitat, data=mergePopFinal13))

plot(QualityHabitat~Difference.from.300.Northery.heading, data=mergePopFinal13)
summary(lm(QualityHabitat~Difference.from.300.Northery.heading, data=mergePopFinal13))

#summary(lm(ReproDensity~CompSpace, data=mergePopFinal13))



###EXPLORE


###SIZE DISTRIBUTIONS
melted=melt(Pop13DF, id.vars=c("Site"), measure.vars = c("s1to3mm","s4mm", "s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20"), na.rm=T)
Pop13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
Pop13

ggplot(data = Pop13, aes(x = variable, y = mean))+geom_point()+facet_grid(Site~.)

melted=melt(Pop13DF, id.vars=c("Site"), measure.vars = c("s1to3mm","Ovigerous"), na.rm=T)
Pop13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
Pop13

ggplot(data = Pop13, aes(x = variable, y = mean))+geom_point()+facet_grid(Site~., scale="free")


