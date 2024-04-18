###2014 Exploratory data to reduce data


##DATA 2014
##PETROS & COBBLE
#2014 PETROS AND COBBLE
#set working directory
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2014 FINAL DATA")

#Read in 2014 quadrat data
Pop14=read.csv("16Feb2017 FINAL Population data 2014.csv", header=TRUE)
Pop14DF=data.frame(Pop14)
attach(Pop14DF)

#change Date
class(Pop14DF$Date)
Pop14DF$Date <- as.Date(Pop14DF$Date, format='%m/%d/%y')
class(Pop14DF$Date)
#check columns
unique(Site)
unique(Date)


###Develop columns to be tested in model
Pop14DF$s1mm=rowSums(Pop14DF[,c(6,7,10:14)], na.rm=T)    
Pop14DF$s2mm=rowSums(Pop14DF[,c(8,15:17)], na.rm=T)  
Pop14DF$s3mm=rowSums(Pop14DF[,c(9,18:21)], na.rm=T)    
Pop14DF$s4mm=rowSums(Pop14DF[,22:26], na.rm=T)  
Pop14DF$s5mm=rowSums(Pop14DF[,27:29], na.rm=T)  
Pop14DF$s6mm=rowSums(Pop14DF[,30:31], na.rm=T)  
Pop14DF$ReproDensity=Pop14DF[,c("Ovigerous")] 
Pop14DF$QualityHabitat=rowSums(Pop14DF[,c("X1m2.50cm2", "X50cm2.10cm2")], na.rm=T) 
Pop14DF$TotalLiveableHabitat=rowSums(Pop14DF[,c("X1m2.50cm2", "X50cm2.10cm2","X.10cm2..course.sand","bare.cobble...2cm." )], na.rm=T) 
Pop14DF$SexRatio=Pop14DF$Females.All/Pop14DF$Males.All

Pop14DF$s1to3mm=rowSums(Pop14DF[,c("s1mm", "s2mm", "s3mm")], na.rm=T) 
Pop14DF$GregSettlement=rowSums(Pop14DF[,c("s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")], na.rm=T) 
Pop14DF$Greater10adults=rowSums(Pop14DF[,c("X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")], na.rm=T) 
Pop14DF$s4to6mm=rowSums(Pop14DF[,c("s4mm","s5mm","s6mm")], na.rm=T) 
Pop14DF$s7to9mm=rowSums(Pop14DF[,c("X7","X8","X9")], na.rm=T) 
Pop14DF$s10to12mm=rowSums(Pop14DF[,c("X10","X11","X12")], na.rm=T) 
Pop14DF$s13to15mm=rowSums(Pop14DF[,c("X13","X14","X15")], na.rm=T) 
Pop14DF$s16plusmm=rowSums(Pop14DF[,c("X16","X17","X18", "X19", "X20")], na.rm=T)

melted <- melt(Pop14DF, id.vars=c("Site"), measure.vars = c("ReproDensity", "QualityHabitat", "SexRatio", "s1to3mm", "GregSettlement", "TotalLiveableHabitat", "Greater10adults", "s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20", "s4mm", "s4to6mm", "s7to9mm", "s10to12mm", "s13to15mm", "s16plusmm"), na.rm=T)
Pop14=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
Pop14
Pop14cast=dcast(Pop14, Site~variable, value.var="mean")

##READ IN SITE CHARACTERISTICS
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015")
SiteInfo14=read.csv("FINAL Petro site info 2013 2014.csv", header=TRUE)
SiteInfo14Imp=SiteInfo14[,c("Site","Year","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Heading", "Heading.minus.coast", "Heading.S", "Lat", "Lon", "GeoType", "Width.average", "Average.surfzone")]
SiteInfoImp14Year=subset(SiteInfo14Imp, Year=="2014")
mergePopFinal14=merge(Pop14cast,SiteInfoImp14Year,by="Site")
mergePopFinal14$s1to3mmLOG=log(mergePopFinal14$s1to3mm)
mergePopFinal14$ReproDensityLOG=log(mergePopFinal14$ReproDensity)
mergePopFinal14$TotalRepro=(mergePopFinal14$ReproDensity*4)*mergePopFinal14$Size.1
mergePopFinal14$TotalRec=(mergePopFinal14$s1to3mm*4)*mergePopFinal14$Size.1
mergePopFinal14$TotalReproLOG=log(mergePopFinal14$TotalRepro)
mergePopFinal14$TotalRecLOG=log(mergePopFinal14$TotalRec)
mergePopFinal14$SizeLOG=log(mergePopFinal14$Size.1)
mergePopFinal14$GregSetLOG=log(mergePopFinal14$GregSettlement)
mergePopFinal14$Greater10adultsLOG=log(mergePopFinal14$Greater10adults)

##TOTAL REC and REPRO
plot(TotalRecLOG~TotalReproLOG, data=subset(mergePopFinal14, Site == "AC" | Site == "AP" |Site == "CB" | Site == "FB" |Site == "HC" | Site == "KC" |Site == "PP" | Site == "MK" |Site == "TC" | Site == "WP"|Site == "PSG"))
summary(lm(TotalRecLOG~Greater10adultsLOG, data=mergePopFinal14))

###SEM

##FULL MODEL- BEST MODEL 2014
PopSite14=join(Pop14DF,SiteInfo, by=c("Site","Year"))

PopSite14NANO <- PopSite14[complete.cases(PopSite14[ ,c("Site", "Surfzone.size..m.", "s1to3mm","X50cm2.10cm2","Ovigerous", "Slope.min", "GregSettlement", "ReproDensity", "QualityHabitat","TotalLiveableHabitat", "Width.average" )]),c("Site", "Surfzone.size..m.", "s1to3mm","X50cm2.10cm2","Ovigerous", "Slope.min", "GregSettlement", "ReproDensity", "QualityHabitat", "Slope.average", "TotalLiveableHabitat", "Width.average")]
attach(PopSite14NANO)


hist(Ovigerous)
PopSite14NANO$OvigerousLOG=log(PopSite14NANO$Ovigerous+0.00001)
PopSite14NANO$s1to3mmLOG=log(PopSite14NANO$s1to3mm+0.00001)
OvigMNB <-lme(OvigerousLOG ~ s1to3mmLOG + QualityHabitat, random=~1|Site, data = PopSite14NANO)
summary(OvigMNB)

hist(s1to3mm)
PopSite14NANO$s1to3mmLOG=log(PopSite14NANO$s1to3mm+0.00001)
RecMNB <-lme(s1to3mmLOG ~ Slope.average + QualityHabitat, random=~1|Site, data = PopSite14NANO)
summary(RecMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(QualityHabitat ~ Slope.average + Surfzone.size..m., random=~1|Site, data = PopSite14NANO)
summary(RockMNB)

plotmodel <- psem(OvigMNB, RecMNB,RockMNB) 
summary(plotmodel)




##New model 2
hist(s1to3mmcombined)
PopSite14NANO$s1to3mmcombinedLOG=log(PopSite14NANO$s1to3mmcombined+0.00001)
recruitMNB<-lme(s1to3mmcombinedLOG ~ Wavetime + X50cm2.10cm2 , random=~1|Site, data = PopSite14NANO)
summary(recruitMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(X50cm2.10cm2 ~ Slope.min + Surfzone.size..m. , random=~1|Site, data = PopSite14NANO)
summary(RockMNB)

hist(Ovigerous)
PopSite14NANO$OvigerousLOG=log(PopSite14NANO$Ovigerous+0.00001)
OvigMNB <-lme(OvigerousLOG ~ s1to3mmcombinedLOG + X50cm2.10cm2, random=~1|Site, data = PopSite14NANO)
summary(OvigMNB)

plotmodel <- psem(recruitMNB, RockMNB,OvigMNB) 
summary(plotmodel)


##Attach Pop13DF with Site info
PopSite14=join(Pop14DF,SiteInfo, by=c("Site","Year"))

PopSite14NANO <- PopSite14[complete.cases(PopSite14[ ,c("Site", "Coastline.reg.0.5.km","Surfzone.size..m.", "s1to3mm","X50cm2.10cm2","Ovigerous", "s35mm", "GregSettlement","Slope.min")]),c("Site", "Coastline.reg.0.5.km","Surfzone.size..m.", "s1to3mm","Size.2","X50cm2.10cm2","Ovigerous", "s35mm", "GregSettlement","Slope.min")]
attach(PopSite13NANO)
library(visdat)
vis_dat(PopSite13NANO)

##New model 2
hist(s1to3mmcombined)
PopSite14NANO$s1to3mmcombinedLOG=log(PopSite14NANO$s1to3mmcombined+0.00001)
recruitMNB<-lme(s1to3mmcombinedLOG ~ Wavetime + X50cm2.10cm2 , random=~1|Site, data = PopSite14NANO)
summary(recruitMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(X50cm2.10cm2 ~ Slope.min + Surfzone.size..m. , random=~1|Site, data = PopSite14NANO)
summary(RockMNB)

hist(Ovigerous)
PopSite14NANO$OvigerousLOG=log(PopSite14NANO$Ovigerous+0.00001)
OvigMNB <-lme(OvigerousLOG ~ s1to3mmcombinedLOG + X50cm2.10cm2, random=~1|Site, data = PopSite14NANO)
summary(OvigMNB)

plotmodel <- psem(recruitMNB, RockMNB,OvigMNB) 
summary(plotmodel)


#summary(lm(ReproDensityLOG~Greater10adults, data=mergePopFinal14))
plot(Width.average~Surfzone.size..m., data=mergePopFinal14)
summary(lm(Width.average~Surfzone.size..m., data=mergePopFinal14))

summary(lm(Greater10adultsLOG~Width.average+s1to3mmLOG, data=mergePopFinal14))
summary(lm(ReproDensityLOG~Greater10adults, data=mergePopFinal14))
summary(lm(s1to3mmLOG~Slope.average+Surfzone.size..m., data=mergePopFinal14))

###Reproduction correlations
ggpairs(mergePopFinal14[,c("ReproDensity", "QualityHabitat", "Size.1", "Size.2", "TotalLiveableHabitat")])

ggpairs(mergePopFinal14[,c("ReproDensityLOG", "QualityHabitat", "Size.1", "Size.2", "TotalLiveableHabitat")])

plot(s1to3mmLOG~Size.2*Slope.min, data=mergePopFinal14)

summary(lm(QualityHabitat~Difference.from.300.Northery.heading, data=mergePopFinal14))

summary(lm(QualityHabitat~Slope.min, data=mergePopFinal14))
summary(lm(Greater10adults~QualityHabitat, data=mergePopFinal14))
summary(lm(ReproDensityLOG~Greater10adults, data=mergePopFinal14))

summary(lm(Surfzone.size..m.~Slope.min, data=mergePopFinal14))

plot(Surfzone.size..m.~Slope.average, data=mergePopFinal14)
ggplot(data = mergePopFinal14, aes(x = Slope.average, y = Surfzone.size..m., color=Site))+geom_point()
ggplot(data = mergePopFinal14, aes(x = s1to3mmLOG, y = Slope.average, color=Site))+geom_point()

summary(lm(s1to3mmLOG~Slope.average+Surfzone.size..m., data=mergePopFinal14))
summary(lm(s1to3mmLOG~Slope.average, data=mergePopFinal14))
summary(lm(Slope.average~Surfzone.size..m., data=mergePopFinal14))
summary(lm(Slope.average~Width.average, data=mergePopFinal14))

###Recruitment correlations
ggpairs(mergePopFinal14[,c("s1to3mm","GregSettlement","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Width.average")])

ggpairs(mergePopFinal14[,c("s1to3mmLOG","GregSettlement","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Width.average", "Average.surfzone")])



summary(lm(s1to3mmLOG~Coastline.reg.0.5.km*Width.average, data=mergePopFinal14))
summary(lm(s1to3mmLOG~Slope.min, data=mergePopFinal14))

###Physical
mergePopFinal14NoMB=subset(mergePopFinal14, Site == "AC" | Site == "AP" |Site == "CB" | Site == "FB" |Site == "HC" | Site == "KC" |Site == "PP" | Site == "MK" |Site == "TC" | Site == "WP"|Site == "PSG")
ggpairs(mergePopFinal14[,c("SizeLOG", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S")])

ggpairs(mergePopFinal14[,c("QualityHabitat", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Average.surfzone")])

ggpairs(mergePopFinal14[,c("Greater10adults", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "TotalLiveableHabitat", "QualityHabitat")])

mergePopFinal14NoMB=subset(mergePopFinal14, Site == "AC" | Site == "AP" |Site == "CB" | Site == "FB" |Site == "HC" | Site == "KC" |Site == "PP" | Site == "MK" |Site == "TC" | Site == "WP"|Site == "PSG")
ggpairs(mergePopFinal14NoMB[,c("SizeLOG", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S")])

ggpairs(mergePopFinal14NoMB[,c("QualityHabitat", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S")])

ggpairs(mergePopFinal14NoMB[,c("Greater10adults", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "TotalLiveableHabitat", "QualityHabitat")])

datanew=subset(mergePopFinal14, Site == "AC" | Site == "AP" |Site == "HC" | Site == "KC" |Site == "WP")
plot(QualityHabitat~Difference.from.300.Northery.heading, data=datanew)
summary(lm(QualityHabitat~Difference.from.300.Northery.heading, data=datanew))
plot(ReproDensityLOG~QualityHabitat, data=datanew)
summary(lm(ReproDensityLOG~QualityHabitat, data=datanew))
plot(s1to3mm~Total.degrees.opening, data=datanew)

datanew2=subset(mergePopFinal14, Site == "CB" | Site == "MK" |Site == "PP" | Site == "PSG" |Site == "TC")
plot(QualityHabitat~Difference.from.300.Northery.heading, data=datanew2)
summary(lm(QualityHabitat~Difference.from.300.Northery.heading+Coastline.reg.0.5.km, data=datanew2))
plot(ReproDensity~QualityHabitat, data=datanew2)
summary(lm(ReproDensity~QualityHabitat, data=datanew2))


ggpairs(datanew[,c("Greater10adults", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "TotalLiveableHabitat")])

datanew2=subset(mergePopFinal14, Site == "KC" | Site == "MB" |Site == "MK" | Site == "WP")
ggpairs(datanew2[,c("Greater10adults", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "TotalLiveableHabitat", "QualityHabitat")])



summary(lm(TotalLiveableHabitat~Coastline.reg.0.5.km*Total.degrees.opening, data=mergePopFinal14))
summary(lm(TotalLiveableHabitat~Heading*Total.degrees.opening, data=mergePopFinal14))
plot(TotalLiveableHabitat~Coastline.reg.0.5.km*Total.degrees.opening, data=mergePopFinal14)

plot(SizeLOG~Diff.hab.angle.and.coastline.reg, data=mergePopFinal14)
summary(lm(SizeLOG~Diff.hab.angle.and.coastline.reg, data=mergePopFinal14))

plot(SizeLOG~Heading, data=mergePopFinal14)
plot(SizeLOG~Coastline.hab.angle, data=mergePopFinal14)
plot(Coastline.hab.angle~Diff.hab.angle.and.coastline.reg, data=mergePopFinal14)
summary(lm(SizeLOG~Coastline.hab.angle, data=mergePopFinal14))
summary(lm(Coastline.hab.angle~Diff.hab.angle.and.coastline.reg, data=mergePopFinal14))

network_plot(correlate(mergePopFinal14[,c("Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Width.average", "ReproDensityLOG", "s1to3mmLOG")]), min_cor=0.6)

###SIZE DISTRIBUTION
melted=melt(Pop14DF, id.vars=c("Site"), measure.vars = c("s1to3mm","s4mm", "s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20"), na.rm=T)
Pop14=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
Pop14
ggplot(data = Pop14, aes(x = variable, y = mean))+geom_point()+facet_grid(Site~., scale="free")

melted=melt(Pop14DF, id.vars=c("Site"), measure.vars = c("s1to3mm","s4to6mm","s7to9mm","s10to12mm","s13to15mm","s16plusmm"), na.rm=T)
Pop14=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
Pop14
ggplot(data = Pop14, aes(x = variable, y = mean))+geom_point()+facet_grid(Site~., scale="free")

melted=melt(Pop14DF, id.vars=c("Site"), measure.vars = c("s1to3mm","Ovigerous"), na.rm=T)
Pop14=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
Pop14

ggplot(data = Pop14, aes(x = variable, y = mean))+geom_point()+facet_grid(Site~., scale="free")

mergePopFinal14$ReproRec=mergePopFinal14$ReproDensity/mergePopFinal14$s1to3mm
ggplot(data = mergePopFinal14, aes(x = Site, y = ReproRec))+geom_point()

ggplot(data = mergePopFinal14, aes(x = Site, y = Width.average))+geom_point()
ggplot(data = mergePopFinal14, aes(x = Site, y = Slope.average))+geom_point()
ggplot(data = mergePopFinal14, aes(x = Site, y = Surfzone.size..m.))+geom_point()
ggplot(data = mergePopFinal14, aes(x = Site, y = Heading))+geom_point()
ggplot(data = mergePopFinal14, aes(x = Site, y = Total.degrees.opening))+geom_point()
ggplot(data = mergePopFinal14, aes(x = Site, y = Heading.minus.coast))+geom_point()

#####DELETE AT END
###EXPLORING 1to3mm relationships
SiteInfo=read.csv("FINAL Petro site info 2013 2014.csv", header=TRUE)
SiteInfoImpYearex=subset(SiteInfo, Year=="2014")
mergePopFinal14EX=merge(Pop14cast,SiteInfoImpYearex,by="Site")
mergePopFinal14EXNOPPPSG=subset(mergePopFinal14EX, Site == "AC" | Site == "AP" |Site == "CB" | Site == "FB" |Site == "HC" | Site == "KC" |Site == "MB" | Site == "MK" |Site == "TC" | Site == "WP")

mergePopFinal14EXNOPPPSG$s1to3mmLOG=log(mergePopFinal14EXNOPPPSG$s1to3mm)
summary(lm(s1to3mmLOG~Slope.min, data=mergePopFinal14EXNOPPPSG))


#all recruits
mergePopFinal14EX$s1to3mmLOG=log(mergePopFinal14EX$s1to3mm)

ggpairs(mergePopFinal14EX[,c("s1to3mm","GregSettlement","Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S")])

Pop14sizes=Pop14DF[,c("Site","s1to3mm","s3to5mm","s4to6mm","s7to9mm","s10to12mm","s13to15mm","s16plusmm")]
 Pop14DFNONA <- Pop14DF[complete.cases(Pop14DF[ ,c("Site","s1to3mm","s4to6mm","s7to9mm","s10to12mm","s13to15mm","s16plusmm")]),]
 Pop14DFNO=na.omit(Pop14sizes)
newPop14sizes<- Pop14sizes[ !is.na(Pop14sizes) ]
as.data.frame(newPop14sizes)
 #Pop14DFNOKC=subset(Pop14DFNONA, Site == "AC" | Site == "AP" |Site == "CB" | Site == "FB" |Site == "HC" | Site == "PSG" |Site == "MB" | Site == "MK" |Site == "TC" | Site == "WP" | Site == "PP")

siteNMDS=metaMDS(Pop14DFNONA[,c("s1to3mm","s4to6mm","s7to9mm","s10to12mm","s13to15mm","s16plusmm")], trymax=1999, k=2, dist="bray")
siteNMDS
stressplot(siteNMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions

#plot of NMDS
NMDS1 = siteNMDS$points[,1]
NMDS2 = siteNMDS$points[,2]
NMDS = data.frame(NMDS1 = NMDS1, NMDS2 = NMDS2)

yearNMDS <- ggplot(NMDS, aes(x=NMDS1, y=NMDS2, col=Pop14DFNONANA$Site)) + geom_point(size=2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.2),legend.title=element_blank(),legend.text=element_text(size=12),panel.border = element_rect(colour = "black", fill=NA, size=1))+annotate("text", label = "Stress: 0.15", x = 1, y = -1.35, size = 3.5, colour = "black")
yearNMDS

M <- cor(mergePopFinal14[,c("Surfzone.size..m.", "Slope.average","Slope.min", "Diff.hab.angle.and.coastline.reg", "Size.1","Size.2", "Coastline.reg.0.5.km","Difference.from.300.Northery.heading","Total.degrees.opening","Coastline.hab.angle", "Surfzone.size..m.", "Heading", "Heading.minus.coast", "Heading.S", "Width.average", "s1to3mmLOG", "ReproDensityLOG")])
corrplot(M, order = "hclust",addrect = 3)
plot(GregSetLOG~log(Width.average), data=mergePopFinal14)
summary(lm(GregSetLOG~Width.average, data=mergePopFinal14))

