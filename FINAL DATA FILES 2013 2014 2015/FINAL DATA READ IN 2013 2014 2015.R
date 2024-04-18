###2013, 2014, 2015
### RUN THIS BEFORE DOING ANYTING!

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
#install.packages("glmmADMB", repos="http://glmmadmb.r-forge.r-project.org/repos", type="source") 
#library(glmmADMB)

##DATA 2013/2014
##READ IN SITE CHARACTERISTICS
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015")
SiteInfo=read.csv("FINAL Petro site info 2013 2014.csv", header=TRUE)
network_plot(correlate(SiteInfo[,c(5:6,8:13,15:22,24:25)]), min_cor=0.7)

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

#Consolidated pigs and mm (eliminate for now because may be confusing results)
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


#Underrock
Pop13DF$CrabsUnderRock=rowSums(Pop13DF[,c("Hemigrapsus.nudus.ALL", "Hemigrapsus.oregonensis.ALL", "Cancer.antennarius.ALL", "Pachychyles.rudis.ALL", "Pachygrapsus.crassipes.ALL","Lophopanopeus.bellus.ALL","Petrolisthes.manimaculus.ALL")])
Pop13DF$FishUnderRock=rowSums(Pop13DF[,c("Gobiesox.maeandricus.ALL", "Anoplarchus.purpurescens.ALL", "Xiphister.atropurpurescens.ALL")])

#subset data set 2013 so only includes up to % flipped so can be merged with 2014
Pop13DFsub=Pop13DF[,c(1:62,156:192)]

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

#Consolidated pigs and mm (eliminate for now because may be confusing results)
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


#combine 2013 2014 
Pop1314=rbind(Pop13DFsub,Pop14DF)




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
View(habtemp13)

levels(prewatertemp13$Date)
watertemp13a <- prewatertemp13[prewatertemp13$Date.reform > as.Date("2013-05-30"),]
watertemp13 <- watertemp13a[watertemp13a$Date.reform < as.Date("2013-08-19"),]
View(watertemp13)

levels(pretidal13$Date)
pretidal13a <- pretidal13[pretidal13$Date.reform > as.Date("2013-05-11"),]
tidal13 <- pretidal13a[pretidal13a$Date.reform < as.Date("2013-08-25"),]
View(tidal13)

levels(premeterodata13$Date)
meterodata13a <- premeterodata13[premeterodata13$Date.reform > as.Date("2013-05-11"),]
meterodata13 <- meterodata13a[meterodata13a$Date.reform < as.Date("2013-08-25"),]
View(meterodata13)



#Read in wave data
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2014 NDBC")
#21014 Waves
wavedirCM=read.csv("Cape Mendo waves 2014 46213.csv", header=TRUE)
wavedirHMB=read.csv("Half Moon Bay waves 2014 46012.csv", header=TRUE)
wavedirMB=read.csv("Monterey Bay wave dir 2014 46042.csv", header=TRUE)

setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA/2013 Physical data")
#2013 Waves
wavedirCM2013=read.csv("CM waves 2013.csv", header=TRUE)
wavedirHMB2013=read.csv("Half Moon Bay waves 2013.csv", header=TRUE)
###END



###TEMPORAL
###AVERAGE QUADRAT LEVEL DATA TO INTEGRATE WITH SITE LEVEL- TEMPORAL
#For 2013 ONLY
melted <- melt(Pop13DF, id.vars=c("Site"), measure.vars = c(10:180), na.rm=T)
all2013quad=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
all2013quad

melted <- melt(Larv13DF, id.vars=c("Site"), measure.vars = c(4:38,41:46), na.rm=T)
larvae13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
larvae13

melted <- melt(slope13, id.vars=c("Site"), measure.vars = c("Slope"), na.rm=T)
slo13=ddply(melted, c("Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
slo13

melted <- melt(wavechla13, id.vars=c("Site"), measure.vars = c("Dyno.cm", "Waveht.ft", "Chla.fl", "wavetime.sec"), na.rm=T)
wavechl13=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
wavechl13
wavechlm=cast(wavechl13, Site~variable, value="mean")
pairs(wavechlm[,c(2:5)])

melted <- melt(prehabtemp13, id.vars=c("Site","HT.Top.or.Bottom"), measure.vars = c("HT.Temp.C"), na.rm=T)
habtemp13=ddply(melted, c("HT.Top.or.Bottom", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
habtemp13

melted <- melt(prewatertemp13, id.vars=c("Site"), measure.vars = c("Water.Temp.C"), na.rm=T)
watertemp13=ddply(melted, c("Site", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
watertemp13

all2013quadcast13=cast(all2013quad, Site~variable, value="mean")
larvae13cast=cast(larvae13,Site~variable, value="mean")
slope13cast=cast(slo13,Site~variable, value="mean")
wavechl13cast=cast(wavechl13,Site~variable, value="mean")
habtemp13cast=cast(habtemp13,Site~HT.Top.or.Bottom, value="mean")
watertemp13cast=cast(watertemp13,Site~variable, value="mean")

merge1=merge(all2013quadcast13,larvae13cast,by="Site")
merge2=merge(merge1,larvae13cast,by="Site")
merge3=merge(merge2,slope13cast,by="Site")
merge4=merge(merge3,wavechl13cast,by="Site")
merge5=merge(merge4,habtemp13cast,by="Site")
merge6=merge(merge5,watertemp13cast,by="Site", all.x=T)

#merge 2013 with habitat data 2013
mergefinal2013=merge(merge6,SiteInfo)



###TEMPORAL WAVES
##2013 MB
wavedirMB$date <- as.Date(with(wavedirMB, paste(X.YY, MM, DD,sep="-")), "%Y-%m-%d")
wavedirMB$date

wavedirMBsub=subset(wavedirMB, date > as.Date("2014-05-01") & date < as.Date("2014-08-23"))

wavedir=ggplot(data = wavedirMBsub, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")

winddir=ggplot(data = wavedirMBsub, aes(x = date, y = WDIR))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")

windspd=ggplot(data = wavedirMBsub, aes(x = date, y = WSP))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")

grid.arrange(winddir,windspd)

###WAVE DIR FROM CM and HMB
wavedirCM$date <- as.Date(with(wavedirCM, paste(X.YY, MM, DD,sep="-")), "%Y-%m-%d")
wavedirCM$date

ggplot(data = wavedirCM, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 month")

wavedirCMsubPP=subset(wavedirCM, date > as.Date("2014-04-30") & date < as.Date("2014-05-13"))
ggplot(data = wavedirCMsubPP, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
table(cut(wavedirCMsubPP$MWD,seq(min(wavedirCMsubPP$MWD),max(wavedirCMsubPP$MWD),203),include.lowest = TRUE))
table(cut(wavedirCMsubPP$MWD,seq(225,356,131),include.lowest = TRUE))
table(cut(wavedirCMsubPP$MWD,seq(314,316,2),include.lowest = TRUE))

wavedirCMsubPSG=subset(wavedirCM, date > as.Date("2014-05-01") & date < as.Date("2014-05-14"))
ggplot(data = wavedirCMsubPSG, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
table(cut(wavedirCMsubPSG$MWD,seq(min(wavedirCMsubPSG$MWD),max(wavedirCMsubPSG$MWD),203),include.lowest = TRUE))
table(cut(wavedirCMsubPSG$MWD,seq(128,271,143),include.lowest = TRUE))
table(cut(wavedirCMsubPSG$MWD,seq(166,168,2),include.lowest = TRUE))

wavedirCMsubCB=subset(wavedirCM, date > as.Date("2014-05-02") & date < as.Date("2014-05-15"))
ggplot(data = wavedirCMsubCB, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
min(wavedirCMsubCB$MWD)
max(wavedirCMsubCB$MWD)
table(cut(wavedirCMsubCB$MWD,seq(min(wavedirCMsubCB$MWD),max(wavedirCMsubPSG$MWD),203),include.lowest = TRUE))
table(cut(wavedirCMsubCB$MWD,seq(316,385,69),include.lowest = TRUE))


wavedirCMsubMK=subset(wavedirCM, date > as.Date("2014-05-06") & date < as.Date("2014-05-19"))
ggplot(data = wavedirCMsubMK, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
min(wavedirCMsubMK$MWD)
max(wavedirCMsubMK$MWD)
table(cut(wavedirCMsubMK$MWD,seq(min(wavedirCMsubMK$MWD),max(wavedirCMsubPSG$MWD),203),include.lowest = TRUE))
table(cut(wavedirCMsubMK$MWD,seq(282,363,81),include.lowest = TRUE))


wavedirCMsubHC=subset(wavedirCM, date > as.Date("2014-05-07") & date < as.Date("2014-05-20"))
ggplot(data = wavedirCMsubHC, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
min(wavedirCMsubHC$MWD)
max(wavedirCMsubHC$MWD)
table(cut(wavedirCMsubHC$MWD,seq(min(wavedirCMsubHC$MWD),max(wavedirCMsubPSG$MWD),203),include.lowest = TRUE))
table(cut(wavedirCMsubHC$MWD,seq(282,363,81),include.lowest = TRUE))
table(cut(wavedirCMsubHC$MWD,seq(212,214,2),include.lowest = TRUE))

wavedirCMsubFB=subset(wavedirCM, date > as.Date("2014-05-08") & date < as.Date("2014-05-21"))
ggplot(data = wavedirCMsubFB, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
min(wavedirCMsubFB$MWD)
max(wavedirCMsubFB$MWD)
table(cut(wavedirCMsubFB$MWD,seq(min(wavedirCMsubFB$MWD),max(wavedirCMsubFB$MWD),168),include.lowest = TRUE))
table(cut(wavedirCMsubFB$MWD,seq(267,334,67),include.lowest = TRUE))

wavedirCMsubMB=subset(wavedirCM, date > as.Date("2014-05-16") & date < as.Date("2014-05-29"))
ggplot(data = wavedirCMsubMB, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
min(wavedirCMsubMB$MWD)
max(wavedirCMsubMB$MWD)
table(cut(wavedirCMsubMB$MWD,seq(min(wavedirCMsubMB$MWD),max(wavedirCMsubMB$MWD),168),include.lowest = TRUE))
table(cut(wavedirCMsubMB$MWD,seq(267,334,67),include.lowest = TRUE))


####

wavedirHMB$date <- as.Date(with(wavedirHMB, paste(X.YY, MM, DD,sep="-")), "%Y-%m-%d")
wavedirHMB$date
wavedirHMBsub=subset(wavedirHMB, MM < 7 & MM > 4)
HMB=ggplot(data = wavedirHMBsub, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")


wavedirMB$date <- as.Date(with(wavedirMB, paste(X.YY, MM, DD,sep="-")), "%Y-%m-%d")
wavedirMB$date
wavedirMBsub=subset(wavedirMB, MM < 7 & MM > 4)
MB=ggplot(data = wavedirMBsub, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")

grid.arrange(MB,HMB,CM)

##2013Waves
wavedirCM2013$date <- as.Date(with(wavedirCM2013, paste(X.YY, MM, DD,sep="-")), "%Y-%m-%d")
wavedirCM2013$date 
wavedir2013sub <- subset(wavedirCM2013, date > as.Date("2013-05-12") & date < as.Date("2013-08-24"))
WD2013=ggplot(data = wavedir2013sub, aes(x = date, y = MWD))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
WD2013

WH2013=ggplot(data = wavedir2013sub, aes(x = date, y = WVHT))+geom_point()+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")
WH2013

table(cut(wavedir2013sub$MWD,seq(min(wavedir2013sub$MWD),max(wavedir2013sub$MWD),212),include.lowest = TRUE))

table(cut(wavedirCMsub$MWD,seq(186,253,67),include.lowest = TRUE))

melted <- melt(Pop13DF, id.vars=c("Site", "Date"), measure.vars = c(173), na.rm=T)
recruitstime=ddply(melted, c("variable", "Site", "Date"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
recruitstime
rec2013=ggplot(data = recruitstime, aes(x = Date, y = mean, fill=Site))+geom_bar(stat="identity")+ scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")#+facet_grid(Site~., scale="free")
rec2013

grid.arrange(WD2013,WH2013, rec2013)
