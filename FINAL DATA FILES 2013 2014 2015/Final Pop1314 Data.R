###7 FEB 2018
###FINAL CODE FOR POPULATION


#clear anything that may interfere with running code
rm(list=ls())

#install packages
require(vioplot)
require(vegan)
require(dplyr)
require(maps)
require(ggplot2)
require(ggmap)
require(pscl)
require(MASS)
require(boot)
require(reshape)
require(gridExtra)
require(plotrix)
require(stargazer)
require(NPCirc)
require(reshape2)
require(Hmisc)
require(corrplot)
library(plyr)
library(reshape2)
library(car)
library(lme4)
library(stargazer)
library(ggfortify)
library(psych)
library(corrr)
library(fitdistrplus)
library(logspline)
library(glmm)
library(devtools)
library(lmerTest)
library(mgcv)
#install.packages("glmmADMB", repos="http://glmmadmb.r-forge.r-project.org/repos", type="source") 
#library(glmmADMB)

##IMPORTANT DATA FRAMES
#POP13DF
#POP14DF
#POP1314

##DATA 2013/2014
##READ IN SITE CHARACTERISTICS
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015")
SiteInfo=read.csv("FINAL Petro site info 2013 2014.csv", header=TRUE)

##PETROS & COBBLE 2013
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

#Consolidated Pop Info
Pop13DF$s1mm=rowSums(Pop13DF[,10:14], na.rm=T)    
Pop13DF$s2mm=rowSums(Pop13DF[,15:17], na.rm=T)  
Pop13DF$s3mm=rowSums(Pop13DF[,18:21], na.rm=T)    
Pop13DF$s4mm=rowSums(Pop13DF[,22:26], na.rm=T)  
Pop13DF$s5mm=rowSums(Pop13DF[,27:29], na.rm=T)  
Pop13DF$s6mm=rowSums(Pop13DF[,30:31], na.rm=T)  
Pop13DF$s1to3mm=rowSums(Pop13DF[,c("s1mm", "s2mm", "s3mm")], na.rm=T) 
Pop13DF$s1to2mm=rowSums(Pop13DF[,c("s1mm", "s2mm")], na.rm=T) 
#<3mm
Pop13DF$recruits3mm=rowSums(Pop13DF[,10:18],na.rm=T) 
#3 and <5mm
Pop13DF$s35mm=rowSums(Pop13DF[,19:27],na.rm=T)
#5,6,7
Pop13DF$s57mm=rowSums(Pop13DF[,28:32],na.rm=T)
#8,9,10
Pop13DF$s810mm=rowSums(Pop13DF[,33:35],na.rm=T)
Pop13DF$s1113mm=rowSums(Pop13DF[,36:38],na.rm=T)
Pop13DF$s1416mm=rowSums(Pop13DF[,39:41],na.rm=T)
Pop13DF$s1720mm=rowSums(Pop13DF[,42:44],na.rm=T)
Pop13DF$ALL=rowSums(Pop13DF[,c("recruits3mm","s35mm","s57mm", "s810mm", "s1113mm", "s1416mm", "s1720mm")],na.rm=T)
Pop13DF$s1mmcombined=rowSums(Pop13DF[,c("s1mm", "Clear")], na.rm=T)  
Pop13DF$s2mmcombined=rowSums(Pop13DF[,c("s2mm", "Pigmented.1")], na.rm=T) 
Pop13DF$s3mmcombined=rowSums(Pop13DF[,c("s3mm", "Pigmented.2")], na.rm=T) 
Pop13DF$s4mmcombined=rowSums(Pop13DF[,c("s4mm", "Pigmented.3")], na.rm=T) 
Pop13DF$s1to3mmcombined=rowSums(Pop13DF[,c("s1mmcombined", "s2mmcombined", "s3mmcombined")], na.rm=T) 
Pop13DF$MaleFemales=rowSums(Pop13DF[,c("Females.All", "Males.All")], na.rm=T) 
Pop13DF$FemaleProp=Pop13DF$Females.All/Pop13DF$MaleFemales
Pop13DF$MaleProp=Pop13DF$Males.All/Pop13DF$MaleFemales
Pop13DF$Propss1to3mmcombined=Pop13DF$s1to3mmcombined/Pop13DF$ALL
#<3mm
Pop13DF$Proprecruits3mm=Pop13DF$recruits3mm/Pop13DF$ALL
#3 and <5mm
Pop13DF$Props35mm=Pop13DF$s35mm/Pop13DF$ALL
#5,6,7
Pop13DF$Props57mm=Pop13DF$s57mm/Pop13DF$ALL
#8,9,10
Pop13DF$Props810mm=Pop13DF$s810mm/Pop13DF$ALL
Pop13DF$Props1113mm=Pop13DF$s1113mm/Pop13DF$ALL
Pop13DF$Props1416mm=Pop13DF$s1416mm/Pop13DF$ALL
Pop13DF$Props1720mm=Pop13DF$s1720mm/Pop13DF$ALL
Pop13DF$LiveableRocks=rowSums(Pop13DF[,c("X.1m2", "X1m2.50cm2", "X50cm2.10cm2", "bare.cobble...2cm.","X.10cm2..course.sand")])
Pop13DF$IdealLiveableRocks=rowSums(Pop13DF[,c("X.1m2", "X1m2.50cm2", "X50cm2.10cm2")])
Pop13DF$NonliveableRocks=rowSums(Pop13DF[,c("rocky.bench", "cemented.rock.1m2.10cm2")])
Pop13DF$Sand=Pop13DF[,c("bare.sand..fine.very.course.sand.")]
Pop13DF$CobbleCourseSand=rowSums(Pop13DF[,c("bare.cobble...2cm.", "X.10cm2..course.sand")])
Pop13DF$OvigFemale=Pop13DF$Ovigerous/Pop13DF$Females.All


#Underrock
Pop13DF$CrabsUnderRock=rowSums(Pop13DF[,c("Hemigrapsus.nudus.ALL", "Hemigrapsus.oregonensis.ALL", "Cancer.antennarius.ALL", "Pachychyles.rudis.ALL", "Pachygrapsus.crassipes.ALL","Lophopanopeus.bellus.ALL","Petrolisthes.manimaculus.ALL")])
Pop13DF$FishUnderRock=rowSums(Pop13DF[,c("Gobiesox.maeandricus.ALL", "Anoplarchus.purpurescens.ALL", "Xiphister.atropurpurescens.ALL")])

##ATTACH SITE INFO
Pop13SiteInfo=merge(Pop13DF,SiteInfo, by=c("Site","Year"))

#subset data set 2013 so only includes up to % flipped so can be merged with 2014
Pop13SiteInfosub=Pop13SiteInfo[,c(1:62,156:193,196:234)]

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


#Consolidated Pop Info
Pop14DF$s1mm=rowSums(Pop14DF[,10:14], na.rm=T)    
Pop14DF$s2mm=rowSums(Pop14DF[,15:17], na.rm=T)  
Pop14DF$s3mm=rowSums(Pop14DF[,18:21], na.rm=T)    
Pop14DF$s4mm=rowSums(Pop14DF[,22:26], na.rm=T)  
Pop14DF$s5mm=rowSums(Pop14DF[,27:29], na.rm=T)  
Pop14DF$s6mm=rowSums(Pop14DF[,30:31], na.rm=T)  
Pop14DF$s1to3mm=rowSums(Pop14DF[,c("s1mm", "s2mm", "s3mm")], na.rm=T) 
Pop14DF$s1to2mm=rowSums(Pop14DF[,c("s1mm", "s2mm")], na.rm=T) 
#<3mm
Pop14DF$recruits3mm=rowSums(Pop14DF[,10:18],na.rm=T) 
#3 and <5mm
Pop14DF$s35mm=rowSums(Pop14DF[,19:27],na.rm=T)
#5,6,7
Pop14DF$s57mm=rowSums(Pop14DF[,28:32],na.rm=T)
#8,9,10
Pop14DF$s810mm=rowSums(Pop14DF[,33:35],na.rm=T)
Pop14DF$s1113mm=rowSums(Pop14DF[,36:38],na.rm=T)
Pop14DF$s1416mm=rowSums(Pop14DF[,39:41],na.rm=T)
Pop14DF$s1720mm=rowSums(Pop14DF[,42:44],na.rm=T)
Pop14DF$ALL=rowSums(Pop14DF[,c("recruits3mm","s35mm","s57mm", "s810mm", "s1113mm", "s1416mm", "s1720mm")],na.rm=T)
Pop14DF$s1mmcombined=rowSums(Pop14DF[,c("s1mm", "Clear")], na.rm=T)  
Pop14DF$s2mmcombined=rowSums(Pop14DF[,c("s2mm", "Pigmented.1")], na.rm=T) 
Pop14DF$s3mmcombined=rowSums(Pop14DF[,c("s3mm", "Pigmented.2")], na.rm=T) 
Pop14DF$s4mmcombined=rowSums(Pop14DF[,c("s4mm", "Pigmented.3")], na.rm=T) 
Pop14DF$s1to3mmcombined=rowSums(Pop14DF[,c("s1mmcombined", "s2mmcombined", "s3mmcombined")], na.rm=T) 
Pop14DF$MaleFemales=rowSums(Pop14DF[,c("Females.All", "Males.All")], na.rm=T) 
Pop14DF$FemaleProp=Pop14DF$Females.All/Pop14DF$MaleFemales
Pop14DF$MaleProp=Pop14DF$Males.All/Pop14DF$MaleFemales
Pop14DF$Propss1to3mmcombined=Pop14DF$s1to3mmcombined/Pop14DF$ALL
#<3mm
Pop14DF$Proprecruits3mm=Pop14DF$recruits3mm/Pop14DF$ALL
#3 and <5mm
Pop14DF$Props35mm=Pop14DF$s35mm/Pop14DF$ALL
#5,6,7
Pop14DF$Props57mm=Pop14DF$s57mm/Pop14DF$ALL
#8,9,10
Pop14DF$Props810mm=Pop14DF$s810mm/Pop14DF$ALL
Pop14DF$Props1113mm=Pop14DF$s1113mm/Pop14DF$ALL
Pop14DF$Props1416mm=Pop14DF$s1416mm/Pop14DF$ALL
Pop14DF$Props1720mm=Pop14DF$s1720mm/Pop14DF$ALL
Pop14DF$LiveableRocks=rowSums(Pop14DF[,c("X.1m2", "X1m2.50cm2", "X50cm2.10cm2", "bare.cobble...2cm.","X.10cm2..course.sand")])
Pop14DF$IdealLiveableRocks=rowSums(Pop14DF[,c("X.1m2", "X1m2.50cm2", "X50cm2.10cm2")])
Pop14DF$NonliveableRocks=rowSums(Pop14DF[,c("rocky.bench", "cemented.rock.1m2.10cm2")])
Pop14DF$Sand=Pop14DF[,c("bare.sand..fine.very.course.sand.")]
Pop14DF$CobbleCourseSand=rowSums(Pop14DF[,c("bare.cobble...2cm.", "X.10cm2..course.sand")])
Pop14DF$OvigFemale=Pop14DF$Ovigerous/Pop14DF$Females.All

##ATTACH SITE INFO
Pop14SiteInfo=merge(Pop14DF,SiteInfo, by=c("Site","Year"))

#combine 2013 2014 
Pop1314SiteInfo=rbind(Pop13SiteInfosub,Pop14SiteInfo)


###----------------------------------------------------------------
###----------------------------------------------------------------
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
Larv13DF$crabsall=rowSums(Larv13DF[,5:31,37])
Larv13DF$earlycrabs=rowSums(Larv13DF[,c(5,7,10,12,14,16,17,19)])
#included Majidae with late
Larv13DF$latecrabs=rowSums(Larv13DF[,c(6,8,9,11,13,15,18,20)])
Larv13DF$allzoea=rowSums(Larv13DF[,c("earlycrabs","latecrabs")])
Larv13DF$PLcrabs=rowSums(Larv13DF[,c(21:31,37)])
Larv13DF$fish=rowSums(Larv13DF[,c(32:36)])

#set working directory 2013
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA/2013 Physical data")

#2013 PHYSICAL
##habitat sediment is in biological sheet
wavechla13=read.csv("2013waveschla.csv", header=TRUE)
prehabtemp13=read.csv("2013HabitatTemps.csv", header=TRUE)
prewatertemp13=read.csv("2013MooringTemps.csv", header=TRUE)
pretidal13=read.csv("2013Tidal.csv", header=TRUE)
undersed13=read.csv("2013UnderRockSediment.csv", header=TRUE)
premeterodata13=read.csv("2013NDBCData.csv", header=TRUE)

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

class(pretidal13$Date)
pretidal13$Date.reform <- as.Date(pretidal13$Date)
class(pretidal13$Date.reform)

class(premeterodata13$Date)
premeterodata13$Date.reform<- as.Date(premeterodata13$Date, format='%m/%d/%y')
class(premeterodata13$Date.reform)

#take out dates not using NEED TO LOOK UP 
levels(prehabtemp13$HT.Date)
habtemp13a <- prehabtemp13[prehabtemp13$Date.reform > as.Date("2013-05-30"),]
habtemp13 <- habtemp13a[habtemp13a$Date.reform < as.Date("2013-08-19"),]
#View(habtemp13)

levels(prewatertemp13$Date)
watertemp13a <- prewatertemp13[prewatertemp13$Date.reform > as.Date("2013-05-30"),]
watertemp13 <- watertemp13a[watertemp13a$Date.reform < as.Date("2013-08-19"),]
#View(watertemp13)

levels(pretidal13$Date)
pretidal13a <- pretidal13[pretidal13$Date.reform > as.Date("2013-05-11"),]
tidal13 <- pretidal13a[pretidal13a$Date.reform < as.Date("2013-08-25"),]
#View(tidal13)

levels(premeterodata13$Date)
meterodata13a <- premeterodata13[premeterodata13$Date.reform > as.Date("2013-05-11"),]
meterodata13 <- meterodata13a[meterodata13a$Date.reform < as.Date("2013-08-25"),]
#View(meterodata13)


###AVERAGE SPATIAL 2013
#For 2013 ONLY
melted <- melt(Pop13SiteInfo, id.vars=c("Site"), measure.vars = c(6:195,199:207,209:219), na.rm=T)
all2013SiteInfo=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)), na.rm=T)
all2013SiteInfo

melted <- melt(Larv13DF, id.vars=c("Site"), measure.vars = c(5:38,41:45), na.rm=T)
larvae13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)), na.rm=T)
larvae13

melted <- melt(wavechla13, id.vars=c("Site"), measure.vars = c("Dyno.cm", "Waveht.ft", "Chla.fl", "wavetime.sec"), na.rm=T)
wavechl13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)), na.rm=T)
wavechl13

melted <- melt(prehabtemp13, id.vars=c("Site","HT.Top.or.Bottom"), measure.vars = c("HT.Temp.C"), na.rm=T)
habtemp13=ddply(melted, c("HT.Top.or.Bottom", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)), na.rm=T)
habtemp13

melted <- melt(prewatertemp13, id.vars=c("Site"), measure.vars = c("Water.Temp.C"), na.rm=T)
watertemp13=ddply(melted, c("Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)), na.rm=T)
watertemp13

all2013sitecast13=cast(all2013SiteInfo, Site~variable, value="mean", na.rm=T)
larvae13cast=cast(larvae13,Site~variable, value="mean", na.rm=T)
wavechl13cast=cast(wavechl13,Site~variable, value="mean", na.rm=T)
habtemp13cast=cast(habtemp13,Site~HT.Top.or.Bottom, value="mean", na.rm=T)
watertemp13cast=cast(watertemp13,Site~variable, value="mean", na.rm=T)

merge1=merge(all2013sitecast13,larvae13cast,by="Site", na.rm=T)
merge2=merge(merge1,larvae13cast,by="Site", na.rm=T)
merge3=merge(merge2,wavechl13cast,by="Site", na.rm=T)
merge4=merge(merge3,habtemp13cast,by="Site", na.rm=T)
merge13=merge(merge4,watertemp13cast,by="Site", all.x=T,na.rm=T)

network_plot(correlate(merge13[,c(153:155,163:167,189:206,221:223,241:245,285:290)]), min_cor=0.9)
corrplot(cor(merge13[,c(153:155,163:167,189:206,221:223,241:245,285:290)]), type="upper")
ggplot(data = merge13, aes(x = Site, y =PLcrabs.x, color=Site))+geom_point()
ggplot(data = merge13, aes(x = Site, y =recruits3mm, color=Site))+geom_point()
ggplot(data = merge14, aes(x = Site, y =PLcrabsall.x, color=Site))+geom_point()
ggplot(data = merge14, aes(x = Site, y =s1to3mmcombined, color=Site))+geom_point()

###AVERAGE TMEPORAL 2013
#For 2013 ONLY
melted <- melt(Pop13SiteInfo, id.vars=c("Site","Week"), measure.vars = c(6:195,199:207,209:219), na.rm=T)
all2013SiteInfoT=ddply(melted, c("variable", "Site", "Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)), na.rm=T)
all2013SiteInfoT

melted <- melt(Larv13DF, id.vars=c("Site","Week"), measure.vars = c(5:39,41:46), na.rm=T)
larvae13T=ddply(melted, c("variable", "Site", "Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)),na.rm=T)
larvae13T

melted <- melt(wavechla13, id.vars=c("Site","Week"), measure.vars = c("Dyno.cm", "Waveht.ft", "Chla.fl", "wavetime.sec"), na.rm=T)
wavechl13T=ddply(melted, c("variable", "Site", "Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)),na.rm=T)
wavechl13T

#melted <- melt(prewatertemp13, id.vars=c("Site"), measure.vars = c("Water.Temp.C"), na.rm=T)
#watertemp13T=ddply(melted, c("Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
#watertemp13T

all2013sitecast13T=cast(all2013SiteInfoT, Site+Week~variable, value="mean", na.rm=T)
larvae13castT=cast(larvae13T,Site+Week~variable, value="mean", na.rm=T)
wavechl13castT=cast(wavechl13T,Site+Week~variable, value="mean", na.rm=T)
#watertemp13cast=cast(watertemp13,Site~variable, value="mean")

merge1T=merge(all2013sitecast13T,larvae13castT, by=c("Site","Week"))
merge13T=merge(merge1T,wavechl13castT, by=c("Site","Week"))

merge13TNA=na.omit(merge13T[,c(243:252,185,42,148:156)])
network_plot(correlate(merge13TNA), min_cor=0.3)
corrplot(cor(merge13TNA), type="upper")

ggplot(data = merge13T, aes(x = Waveht.ft , y =s1mm))+geom_point()+facet_grid(Site~.)
ggplot(data = merge13T, aes(x = wavetime.sec , y =s1mm))+geom_point()+facet_grid(Site~.,scale="free")
ggplot(data = merge13T, aes(x = log(crabsall) , y =s1mm, color=Site))+geom_point()

#set working directory 2014
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2014 FINAL DATA")

#2014 LARVAE
##habitat sediment is in biological sheet
larvtrap14=read.csv("2014Trap.csv", header=TRUE)
larvtuffy14=read.csv("2014Tuffy.csv", header=TRUE)

larvtrap14$PLcrabsall=rowSums(larvtrap14[,c(4:8)])
larvtrap14$fishall=rowSums(larvtrap14[,c(9:12)])

larvtuffy14$PLcrabsall=rowSums(larvtuffy14[,c(4:7)])
larvtuffy14$fishall=larvtuffy14$Tuffy.Sculpin

##2014 Site
Pop14SiteInfosub=subset(Pop14SiteInfo, Site=="AC"| Site=="AP" | Site=="FB" | Site=="TC")
melted <- melt(Pop14SiteInfosub, id.vars=c("Site"), measure.vars = c(6:100,104:112,114:124), na.rm=T)
all2014SiteInfo=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
all2014SiteInfo

melted <- melt(larvtrap14, id.vars=c("Site"), measure.vars = c(4:14), na.rm=T)
larvae14=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
larvae14

melted <- melt(larvtuffy14, id.vars=c("Site"), measure.vars = c(4:8,10:11), na.rm=T)
larvaetuffy14=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
larvaetuffy14

all2014sitecast14=cast(all2014SiteInfo, Site~variable, value="mean", na.rm=T)
larvae14cast=cast(larvae14,Site~variable, value="mean", na.rm=T)
larvaetuffy14cast=cast(larvaetuffy14,Site~variable, value="mean", na.rm=T)

merge114=merge(all2014sitecast14,larvae14cast, by=c("Site"))
merge14=merge(merge114,larvaetuffy14cast, by=c("Site"))


network_plot(correlate(merge214[,c(108:109)]), min_cor=0.3)
corrplot(cor(merge214TNA), type="upper")

##2014 Time
Pop14SiteInfosub=subset(Pop14SiteInfo, Site=="AC"| Site=="AP" | Site=="FB" | Site=="TC")
melted <- melt(Pop14SiteInfosub, id.vars=c("Site", "Week"), measure.vars = c(6:100,104:112,114:124), na.rm=T)
all2014SiteInfoT=ddply(melted, c("variable", "Site","Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
all2014SiteInfoT

melted <- melt(larvtrap14, id.vars=c("Site","Week"), measure.vars = c(4:14), na.rm=T)
larvae14T=ddply(melted, c("variable", "Site","Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
larvae14T

melted <- melt(larvtuffy14, id.vars=c("Site","Week"), measure.vars = c(4:8,10:11), na.rm=T)
larvaetuffy14T=ddply(melted, c("variable", "Site","Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
larvaetuffy14T

all2014sitecast14T=cast(all2014SiteInfoT, Site+Week~variable, value="mean", na.rm=T)
larvae14castT=cast(larvae14T,Site+Week~variable, value="mean", na.rm=T)
larvaetuffy14castT=cast(larvaetuffy14T,Site+Week~variable, value="mean", na.rm=T)

merge114T=merge(all2014sitecast14T,larvae14castT, by=c("Site","Week"), na.rm=T)
merge214T=merge(merge114T,larvaetuffy14castT, by=c("Site","Week"), na.rm=T, all=T)

merge214TNA=na.omit(merge214T[,c(48:50,58:62,109:110,116:117)])
network_plot(correlate(merge214TNA), min_cor=0.3)
corrplot(cor(merge214TNA), type="upper")

T13a=ggplot(data = merge13T, aes(x = Week, y =log(PLcrabs), color=Site))+geom_point()
T13b=ggplot(data = merge13T, aes(x = Week, y =log(recruits3mm), color=Site))+geom_point()
grid.arrange(T13a,T13b)
ggplot(data = merge214T, aes(x = Site, y =PLcrabsall.x, color=Site))+geom_point()
ggplot(data = merge214T, aes(x = Site, y =s1to3mmcombined, color=Site))+geom_point()


###----------------------------------------------------------------
###----------------------------------------------------------------
###----------------------------------------------------------------
###----------------------------------------------------------------
###CORRELATIONS FOR 2013 and 2014
##BOTH YEARS
merge1314a <- melt(Pop1314SiteInfo, id.vars=c("Site", "Year"), measure.vars = c(6:100,104:112,114:128,139), na.rm=T)
merge1314b=ddply(merge1314a, c("variable", "Site","Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
merge1314b

merge1314=cast(merge1314b, Site+Year~variable, value="mean", na.rm=T)
merge1314=as.data.frame(merge1314)

####CORRELATIONS
###2014 4 Sites
merge4sites2014=subset(Pop1314SiteInfo, Year=="2014")
merge4sites2014a=subset(merge4sites2014, Site=="AC"| Site=="AP" | Site=="FB" | Site=="TC")
merge20144sites <- melt(merge4sites2014a, id.vars=c("Site"), measure.vars = c(6:100,104:112,114:129), na.rm=T)
merge4sites2014=ddply(merge20144sites, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
merge4sites2014

foursites2014=cast(merge4sites2014, Site~variable, value="mean")
foursites2014=as.data.frame(foursites2014)

network_plot(correlate(foursites2014[,c(26:29,32:37,49:56,61,66:96)]), min_cor=0.9)
corrplot(cor(foursites2014[,c(26:29,32:37,49:56,61,66:96)]), type="upper", order="hclust")

ggplot(data = foursites2014, aes(x = Diff.hab.coast.angle.and.coast , y =Ovigerous.size.2014 , color=Site))+geom_point()


z=cor(foursites2014[,c(26:29,32:37,49:56,61,66:96)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .4)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.4)
write.csv(zdf1, file = "Foursites2014.csv")

####2014 ALL SITES WEEK 0
mergeALLsites2014=subset(Pop1314SiteInfo, Year=="2014")
mergeALLsites2014a=subset(mergeALLsites2014, Week=="Week 0")
merge2014ALLsites <- melt(mergeALLsites2014a, id.vars=c("Site"), measure.vars = c(6:100,104:112,114:129), na.rm=T)
mergeALLsites2014=ddply(merge2014ALLsites, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
mergeALLsites2014

allsites2014=cast(mergeALLsites2014, Site~variable, value="mean")
allsites2014=as.data.frame(allsites2014)

network_plot(correlate(allsites2014[,c(25,28,33,30,53:55,57,61,69,71,74,75:94)]), min_cor=0.6)
corrplot(cor(allsites2014[,c(94,95:99,75:93,28)]), type="upper")
corrplot(cor(allsites2014[,c(45,57,25:33,74:99)]), method = "number")

pairs(allsites2014[,c(45,57,25:33,74:99)])
pairs(allsites2013[,c(91,116,43:49)])

z=cor(allsites2014[,c(94,28:33,75:93,95:99)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .3)
zdfa<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.3)

sub1=subset(allsites2014, Coastline.regional.50km < 326)
z=cor(sub1[,c(28:33,75:99)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .3)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.3)

sub2=subset(allsites2014, Coastline.regional.50km > 326)
z=cor(sub2[,c(22:25,45:57,61:71,28:33,74:99)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .3)
zdf2<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.3)
View(zdf2)

z=cor(Pop13SiteInfo[,c(52:195)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .1)
zdf13<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.05)

network_plot(correlate(sub1[,c(25,28,33,30,53:55,57,61,69,71,74,75:94)]), min_cor=0.8)
network_plot(correlate(sub2[,c(25,28,33,30,53:55,57,61,69,71,74,75:94)]), min_cor=0.8)

ggplot(data = subset(allsites2014), aes(x = Coastline.regional.50km , y =, color=Site))+geom_point()
ggplot(data = subset(allsites2013), aes(x = Coastline.regional.50km , y =, color=Site))+geom_point()

ggplot(data = subset(allsites2014, Coastline.regional.50km < 326), aes(x = Props810mm , y =Propss1to3mmcombined, color=Site))+geom_point()
ggplot(data = subset(allsites2014, Coastline.regional.50km > 326), aes(x = Props810mm , y =Propss1to3mmcombined, color=Site))+geom_point()

ggplot(data = subset(allsites2013, Coastline.regional.50km < 326), aes(x = Props810mm , y =Propss1to3mmcombined, color=Site))+geom_point()
ggplot(data = subset(allsites2013, Coastline.regional.50km > 326), aes(x = Props810mm , y =Propss1to3mmcombined, color=Site))+geom_point()


suballsites2014=subset(allsites2014,Ovigerous < 3)
network_plot(correlate(suballsites2014[,c(25,28,33,30,53:55,57,61,69,71,74,75:94)]), min_cor=0.6)
corrplot(cor(suballsites2014[,c(25,28,33,30,53:55,57,61,69,71,74,75:94)]), type="upper", order="hclust")

z=cor(allsites2014[,c(22:25,28:33,45:52,57,58:71,73:92)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .3)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.3)
write.csv(zdf1, file = "ALLsites2014.csv")


####2013 ALL 5 SITES 
mergeALLsites2013=subset(Pop1314SiteInfo, Year=="2013")
merge2013ALLsites <- melt(mergeALLsites2013, id.vars=c("Site"), measure.vars = c(6:100,104:112,114:129), na.rm=T)
mergeALLsites2013=ddply(merge2013ALLsites, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
mergeALLsites2013

allsites2013=cast(mergeALLsites2013, Site~variable, value="mean")
allsites2013=as.data.frame(allsites2013)

network_plot(correlate(allsites2013[,c(38:41,43:49,62:69,54:56,75:88,90:109)]), min_cor=0.9)
corrplot(cor(allsites2013[,c(44,86:88,92:114)]), type="upper", order="hclust")
pairs(allsites2013[,c(62,54:56,94:111)])

z=cor(allsites2013[,c(44,92:116)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .4)
zdf2013<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.4)

ggplot(data = allsites2013, aes(x = Coastline.regional.50km , y =s1to3mmcombined , color=Site))+geom_point()
ggplot(data = allsites2014, aes(x = Coastline.regional.50km , y =s1to3mmcombined , color=Heading))+geom_point()

z=cor(allsites2013[,c(38:41,43:49,62:69,54:56,75:88,90:109)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .4)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.4)
write.csv(zdf1, file = "ALLsites2013.csv")


###FINAL GRAPHS###
####Site --> recruits (Site,RECRUITS)
allsites2014$Site2 <- factor(allsites2014$Site, levels = allsites2014$Site[order(allsites2014$s1to3mmcombined)])
ggplot(data = subset(allsites2014), aes(x = Site2 , y =s1to3mmcombined, color=Site))+geom_point()

allsites2013$Site2 <- factor(allsites2013$Site, levels = allsites2013$Site[order(allsites2013$s1to3mmcombined)])
ggplot(data = subset(allsites2013), aes(x = Site2 , y =s1to3mmcombined, color=Site))+geom_point()

melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Site"), measure.vars = c("s1to3mmcombined"), na.rm=T)
rec=ddply(melted, c("Year", "Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
rec 

recplot=ggplot(data = rec, aes(x = Site, y = mean))+geom_bar(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=rec, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+labs(x=(""), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(variable~Year)
recplot

##SITE AND OVIGEROUS
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Site"), measure.vars = c("Ovigerous", "OvigFemale"), na.rm=T)
ovig=ddply(melted, c("Year", "Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ovig 

ovigplot=ggplot(data = ovig, aes(x = Site, y = mean))+geom_bar(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=ovig, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+labs(x=(""), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(variable~Year, scale="free")
ovigplot


####Wave exposure --> recruits (COASTLINE REGIONAL,RECRUITS)
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Site", "Coastline.regional.50km"), measure.vars = c("s1to3mmcombined"), na.rm=T)
reccoast=ddply(melted, c("Year", "Site", "Coastline.regional.50km", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
reccoast 

reccoastplot=ggplot(data = reccoast, aes(x = Coastline.regional.50km, y = mean, color=Site))+geom_point(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=reccoast, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+labs(x=("habitat orientation (degrees)"), y=("density of recruits"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
reccoastplot

ggplot(data = subset(allsites2014), aes(x = Coastline.regional.50km , y =s1to3mmcombined, color=Site))+geom_point()
ggplot(data = subset(allsites2013), aes(x = Coastline.regional.50km , y =s1to3mmcombined, color=Site))+geom_point()

mixed.lmer <- lmer(s1to3mmcombined ~ Coastline.regional.50km + (1|Year), data = merge1314, REML=F)
mixed.null <- lmer(s1to3mmcombined ~  (1|Year), data = merge1314, REML=F)
anova(mixed.lmer,mixed.null)
summary(mixed.lmer)

####Wave exposure --> Habitat size (COASTLINE REGIONAL,SIZE.2)- DONT NEED YEAR
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Site", "Coastline.regional.50km"), measure.vars = c("Size.2"), na.rm=T)
sizecoast=ddply(melted, c("Year", "Site", "Coastline.regional.50km", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sizecoast 

sizecoastplot=ggplot(data = sizecoast, aes(x = Coastline.regional.50km, y = mean, color=Site))+geom_point(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=sizecoast, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+labs(x=("habitat orientation (degrees)"), y=("habitat size (m2)"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
sizecoastplot


ggplot(data = subset(allsites2014), aes(x = Coastline.regional.50km , y =Size.2, color=Site))+geom_point()
ggplot(data = subset(allsites2013), aes(x = Coastline.regional.50km , y =Size.2, color=Site))+geom_point()

mixed.lmer <- lm(Size.2 ~ Coastline.regional.50km, data = subset(merge1314, Year=="2014"))
summary(mixed.lmer)
#mixed.null <- lm(Size.2 ~ , data = subset(merge1314, Year=="2014"))
anova(mixed.lmer)

mixed.lmer <- lmer(Ovigerous ~ Coastline.regional.50km + (1|Year), data = Pop1314SiteInfo, REML=F)
mixed.null <- lmer(s1to3mmcombined ~  (1|Year), data = Pop1314SiteInfo, REML=F)
anova(mixed.lmer,mixed.null)
summary(mixed.lmer)

ggplot(data = subset(merge1314), aes(x = Coastline.regional.50km , y =X50cm2.10cm2, color=Site))+geom_point()+facet_grid(Year~.)



####Wave exposure regional --> Cobble size (Coastline 50km, X50cmto10cm)
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Site", "Coastline.regional.50km"), measure.vars = c("X50cm2.10cm2"), na.rm=T)
sedcoast=ddply(melted, c("Year", "Site", "Coastline.regional.50km", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sedcoast 

sedcoastplot=ggplot(data = sedcoast, aes(x = Coastline.regional.50km, y = mean, color=Site))+geom_point(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=sedcoast, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+labs(x=("habitat orientation (degrees)"), y=("percentage of quality sediment"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
sedcoastplot

ggplot(data = subset(allsites2014), aes(x = Coastline.regional.50km , y=X50cm2.10cm2, color=Site))+geom_point()
ggplot(data = subset(allsites2013), aes(x = Coastline.regional.50km, y = X50cm2.10cm2, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Coastline.regional.50km , y = X50cm2.10cm2, color=Site))+geom_point()

ggplot(data = subset(Pop1314SiteInfo), aes(x = Coastline.regional.50km , y = X50cm2.10cm2, color=Same.geo, group=Site))+geom_boxplot()+facet_grid(Year~.)

ggplot(data = subset(Pop1314SiteInfo), aes(x = Size.2 , y = X50cm2.10cm2, color=Same.geo))+geom_point()+facet_grid(Year~.)

mixed.lmer <- lmer(X50cm2.10cm2 ~ Coastline.regional.50km + (1|Year), data = merge1314, REML=F)
mixed.null <- lmer(X50cm2.10cm2 ~  (1|Year), data = merge1314, REML=F)
anova(mixed.lmer,mixed.null)
summary(mixed.lmer)


gam1 <- mgcv::gam(asin(sqrt(X50cm2.10cm2/100)) ~ s(Coastline.regional.50km), data = subset(merge1314, Year=="2014"))
summary(gam1)
plot(gam1)



hist(asin(sqrt(Pop1314SiteInfo$X50cm2.10cm2/100)))

####Size --> Sediment (Size.2,5010cm)
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Site", "Size.2"), measure.vars = c("X50cm2.10cm2"), na.rm=T)
sizesed=ddply(melted, c("Year", "Site", "Size.2", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sizesed 

sizesedplot=ggplot(data = sizesed, aes(x = Size.2, y = mean, color=Site))+geom_point(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=sizesed, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+labs(x=("habitat size (m2)"), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
sizesedplot

mixed.lmer <- lmer(X50cm2.10cm2~ Size.2 + (1|Year), data = Pop1314SiteInfo, REML=F)
mixed.null <- lmer(X50cm2.10cm2 ~  (1|Year), data = Pop1314SiteInfo, REML=F)
anova(mixed.lmer,mixed.null)

####Size --> Proportion of Ovigerous (Size.2,OvigFemale)
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Site", "Size.2"), measure.vars = c("OvigFemale"), na.rm=T)
propovigsize=ddply(melted, c("Year", "Site", "Size.2", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
propovigsize 

propovigsizeplot=ggplot(data = sizesed, aes(x = Size.2, y = mean, color=Site))+geom_point(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=propovigsize, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+labs(x=("habitat size (m2)"), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
propovigsizeplot

Pop1314SiteInfoNA=na.omit(Pop1314SiteInfo[,c("Year","Size.2", "X50cm2.10cm2", "Site", "OvigFemale")])

mixed.lmer <- lmer(OvigFemale~ Size.2 + (1|Year), data = Pop1314SiteInfoNA, REML=F)
mixed.null <- lmer(OvigFemale ~  (1|Year), data = Pop1314SiteInfoNA, REML=F)
anova(mixed.lmer,mixed.null)

####Sediment --> Prop of Ovigerous (X50cm10cm,OvigFemale)
glimmer<-glmer(data=Pop1314SiteInfo[-which(Pop1314SiteInfo$Ovigerous>Pop1314SiteInfo$Females.All),], cbind(Ovigerous,Females.All-Ovigerous)~scale(X50cm2.10cm2) + (1|Year) + (1|Site), family="binomial")
summary(glimmer)

ggplot(data = Pop1314SiteInfo, aes(x = X50cm2.10cm2, y = OvigFemale))+geom_point()+facet_wrap(~Site)

limmersize<-lmer(data=merge1314, Ovigerous.size~scale(X50cm2.10cm2) + (1|Year))
summary(limmersize)

limmer<-lmer(data=Pop1314SiteInfo[-which(Pop1314SiteInfo$Ovigerous>Pop1314SiteInfo$Females.All),], OvigFemale~scale(X50cm2.10cm2) + (1|Year))
summary(limmer)

####Sediment --> Ovigerous (X50cm10cm,Ovigerous)
sedovigplot=ggplot(data = Pop1314SiteInfo, aes(x = X50cm2.10cm2, y = Ovigerous, color=Site))+geom_point()+labs(x=("percentage of quality sediment"), y=("Reproductive female density"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
sedovigplot

Pop1314SiteInfoNA=na.omit(Pop1314SiteInfo[,c("Year","Ovigerous", "X50cm2.10cm2", "Site")])

mixed.lmer <- lmer(Ovigerous~ X50cm2.10cm2 + (1|Year)+ (1|Site), data = Pop1314SiteInfoNA, REML=F)
mixed.null <- lmer(Ovigerous ~  (1|Year) + (1|Site), data = Pop1314SiteInfoNA, REML=F)
anova(mixed.lmer,mixed.null)

###Maybe include thermal for 5 sites to show that FB is also very hot which may explain lower number?

####Sediment --> Ovigerous size (X50cm10cm,Ovigerous) 
melted <- melt(Pop1314SiteInfo, id.vars=c("Site","Year", "Ovigerous.size"), measure.vars = c("X50cm2.10cm2"), na.rm=T)
sedsizeovig=ddply(melted, c("Site", "Ovigerous.size", "variable", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sedsizeovig 

sedsizeovigplot=ggplot(data = sedsizeovig, aes(y = mean, x = Ovigerous.size, color=Site))+geom_point(stat = "identity", size=2, position=position_dodge(width=1))+labs(y=("percentage of quality sediment"), x=("Reproductive female size (mm)"))+geom_errorbar(data=sedsizeovig, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+coord_flip()+facet_grid(Year~.,scale="free")
sedsizeovigplot

Pop1314SiteInfoNA=na.omit(Pop1314SiteInfo[,c("Year","Ovigerous.size", "X50cm2.10cm2", "Site")])

mixed.lmer <- lmer(Ovigerous.size~ X50cm2.10cm2 + (1|Year), data = Pop1314SiteInfoNA, REML=F)
mixed.null <- lmer(Ovigerous.size ~  (1|Year), data = Pop1314SiteInfoNA, REML=F)
anova(mixed.lmer,mixed.null)

##Ovigerous by embryos
corrplot(cor(merge1314[,c(122,97:121,46,49:54)]), type="upper")
ggplot(data = merge1314, aes(x = Ovigerous, y = Embryos.per.m2, color=Site))+geom_point()+labs(x=(""), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~.)

###Proportion of Ovigerous by embryos
ggplot(data = merge1314, aes(x = OvigFemale, y = Embryos.per.m2, color=Site))+geom_point()+labs(x=(""), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~.)

##Embryo density v. recruitment
ggplot(data = merge1314, aes(x = Embryos.per.m2, y = s1to3mmcombined, color=Site))+geom_point()+labs(x=(""), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~.)

mixed.lmer <- lmer(s1to3mmcombined~ Embryos.per.m2 + (1|Year), data = Pop1314SiteInfo, REML=F)
mixed.null <- lmer(s1to3mmcombined ~  (1|Year), data = Pop1314SiteInfo, REML=F)
anova(mixed.lmer,mixed.null)


###Total reproductive output to total recruitment
Pop1314SiteInfo$s1to3mmcombinedm2=Pop1314SiteInfo$s1to3mmcombined*4
Pop1314SiteInfo$Totalrecruitment=Pop1314SiteInfo$s1to3mmcombinedm2*Pop1314SiteInfo$Size.2
Pop1314SiteInfo$Totalreproduction=Pop1314SiteInfo$Embryos.per.m2*Pop1314SiteInfo$Size.2

merge1314a <- melt(Pop1314SiteInfo, id.vars=c("Site", "Year"), measure.vars = c(6:100,104:112,114:128,139:142), na.rm=T)
merge1314b=ddply(merge1314a, c("variable", "Site","Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
merge1314b

merge1314B=cast(merge1314b, Site+Year~variable, value="mean", na.rm=T)
merge1314B=as.data.frame(merge1314B)

ggplot(data = merge1314B, aes(x = log(Totalreproduction), y = log(Totalrecruitment), color=Site))+geom_point()+labs(x=("Total reproduction (log)"), y=("Total recruitment (log)"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~.)

mixed.lmer <- lmer(Totalrecruitment~ Totalreproduction + (1|Year), data = Pop1314SiteInfo, REML=F)
mixed.null <- lmer(Totalrecruitment ~  (1|Year), data = Pop1314SiteInfo, REML=F)
anova(mixed.lmer,mixed.null)


##INTERACTION BETWEEN REGIONAL AND HEADING N and RECRUITS
#ggplot(data = subset(allsites2014, Coastline.regional.50km < 326), aes(x = Difference.from.300.Northery.heading , y =s1to3mmcombined, color=Site))+geom_point()
#ggplot(data = subset(allsites2014, Coastline.regional.50km > 326), aes(x = Difference.from.300.Northery.heading , y =s1to3mmcombined, color=Site))+geom_point()

#ggplot(data = subset(allsites2013, Coastline.regional.50km < 326), aes(x = Difference.from.300.Northery.heading , y =s1to3mmcombined, color=Site))+geom_point()
#ggplot(data = subset(allsites2013, Coastline.regional.50km > 326), aes(x = Difference.from.300.Northery.heading , y =s1to3mmcombined, color=Site))+geom_point()

#mixed.lmer <- lmer(s1to3mmcombined ~ Coastline.regional.50km*Difference.from.300.Northery.heading + (1|Year), data = Pop1314SiteInfo, REML=F)
#mixed.null <- lmer(s1to3mmcombined ~  (1|Year), data = Pop1314SiteInfo, REML=F)
#anova(mixed.lmer,mixed.null)


####Ovigerous density --> recruit density (recruits,Ovigerous) ????
#recovigplot=ggplot(data = Pop1314SiteInfo, aes(y = s1to3mmcombined, x = Ovigerous, color=Site))+geom_point()+labs(y=("Recruit density"), x=("Reproductive female density"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+coord_flip()+facet_grid(Year~.,scale="free")
#recovigplot

#Pop1314SiteInfoNA=na.omit(Pop1314SiteInfo[,c("Year","Site", "Ovigerous", "s1to3mmcombined")])

#mixed.lmer <- lmer(s1to3mmcombined~ Ovigerous + (1|Year)+ (1|Site), data = Pop1314SiteInfoNA, REML=F)
#mixed.null <- lmer(s1to3mmcombined ~  (1|Year)+ (1|Site), data = Pop1314SiteInfoNA, REML=F)
#anova(mixed.lmer,mixed.null)


####Proportion of Ovigerous --> Ovigerous.Size (OvigFemale,Ovigerous size)
#melted <- melt(Pop1314SiteInfo, id.vars=c("Site","Year", "Ovigerous.size"), measure.vars = c("OvigFemale"), na.rm=T)
#propovigsize=ddply(melted, c("Site", "Ovigerous.size", "variable", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
#propovigsize 

#propovigsizeplot=ggplot(data = propovigsize, aes(y = mean, x = Ovigerous.size, color=Site))+geom_point(stat = "identity", size=2, position=position_dodge(width=1))+labs(y=("Proportion of reproductive females"), x=("Reproductive female size (mm)"))+geom_errorbar(data=propovigsize, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+coord_flip()+facet_grid(Year~.,scale="free")
#propovigsizeplot

#Pop1314SiteInfoNA=na.omit(Pop1314SiteInfo[,c("Year","Ovigerous.size", "OvigFemale", "Site")])

#mixed.lmer <- lmer(Ovigerous.size~ OvigFemale + (1|Year), data = Pop1314SiteInfoNA, REML=F)
#mixed.null <- lmer(Ovigerous.size ~  (1|Year), data = Pop1314SiteInfoNA, REML=F)
#anova(mixed.lmer,mixed.null)




###STACKED BARPLOT
##STACKED BARPLOT OF SEDIMENT
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Regional.grouping"), measure.vars = c(52:57), na.rm=T)
sediment1314=ddply(melted, c("Year", "variable", "Regional.grouping"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sed=ggplot(data = sediment1314, aes(x = Regional.grouping, y = mean, fill=variable))+geom_bar(stat = "identity", width=0.5)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(.~Year)+scale_fill_manual(values = c("chocolate", "orangered3", "darkblue", "darkgoldenrod1", "burlywood", "black"))
sed

##STACKED BARPLOT OF SIZE CLASSES
melted <- melt(Pop1314SiteInfo, id.vars=c("Year", "Regional.grouping"), measure.vars = c(88:94), na.rm=T)
size1314=ddply(melted, c("Regional.grouping", "Year", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
size=ggplot(data = size1314, aes(x = Regional.grouping, y = mean, fill=variable))+geom_bar(stat = "identity", width=0.5)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(.~Year, scale="free")#+scale_fill_manual(values = c("darkred", "orangered3", "chocolate", "darkgoldenrod1", "grey80", "burlywood", "darkblue", "black"))
size 

grid.arrange(sed,size)


###OTHERS
###Size classes and NonLiveable
nonlive57plot=ggplot(data = Pop1314SiteInfo, aes(x = NonliveableRocks, y = s57mm, color=Site))+geom_point()+labs(x=("percentage of nonliveable sediment"), y=("5 to 7mm density"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
nonlive57plot

Pop1314SiteInfoNA=na.omit(Pop1314SiteInfo[,c("Year","Site", "s57mm", "NonliveableRocks")])
mixed.lmer <- lmer(s57mm~ NonliveableRocks + (1|Year)+ (1|Site), data = Pop1314SiteInfoNA, REML=F)
mixed.null <- lmer(s57mm ~  (1|Year)+ (1|Site), data = Pop1314SiteInfoNA, REML=F)
anova(mixed.lmer,mixed.null)

nonlive810plot=ggplot(data = Pop1314SiteInfo, aes(x = NonliveableRocks, y = s810mm, color=Site))+geom_point()+labs(x=("percentage of nonliveable sediment"), y=("8 to 10mm density"))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))+facet_grid(Year~., scale="free")
nonlive810plot

Pop1314SiteInfoNA=na.omit(Pop1314SiteInfo[,c("Year","Site", "s810mm", "NonliveableRocks")])
mixed.lmer <- lmer(s810mm~ NonliveableRocks + (1|Year)+ (1|Site), data = Pop1314SiteInfoNA, REML=F)
mixed.null <- lmer(s810mm ~  (1|Year)+ (1|Site), data = Pop1314SiteInfoNA, REML=F)
anova(mixed.lmer,mixed.null)

##CRABS UNDER ROCK AND 5010cm
ggplot(data = Pop13SiteInfo, aes(x = X50cm2.10cm2, y = CrabsUnderRock, color=Site))+geom_point()+labs(x=(""), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))


Pop13SiteInfoNA=na.omit(Pop13SiteInfo[,c("Site", "X50cm2.10cm2", "CrabsUnderRock")])
mixed.lmer <- lmer(CrabsUnderRock~ X50cm2.10cm2 + (1|Site), data = Pop13SiteInfoNA, REML=F)
mixed.null <- lmer(CrabsUnderRock ~ + (1|Site), data = Pop13SiteInfoNA, REML=F)
anova(mixed.lmer,mixed.null)

#H.nudus and 5010cm
melted <- melt(Pop13SiteInfo, id.vars=c("Site"), measure.vars = c("Hemigrapsus.nudus..S.", "Hemigrapsus.nudus..M.", "Hemigrapsus.nudus..L."), na.rm=T)
Hnudus=ddply(melted, c("Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))

ggplot(data = Hnudus, aes(x = Site, y = mean, color=variable))+geom_point()+labs(x=(""), y=(""))+theme(plot.title=element_text(size=18), axis.text=element_text(size=12), axis.title.y = element_text(size=15), axis.title.x= element_text(size=15),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())+theme(legend.position="right", legend.title=element_text(size=18) , legend.text=element_text(size=15))


Pop13SiteInfoNA=na.omit(Pop13SiteInfo[,c("Site", "X50cm2.10cm2", "CrabsUnderRock")])
mixed.lmer <- lmer(CrabsUnderRock~ X50cm2.10cm2 + (1|Site), data = Pop13SiteInfoNA, REML=F)
mixed.null <- lmer(CrabsUnderRock ~ + (1|Site), data = Pop13SiteInfoNA, REML=F)
anova(mixed.lmer,mixed.null)


