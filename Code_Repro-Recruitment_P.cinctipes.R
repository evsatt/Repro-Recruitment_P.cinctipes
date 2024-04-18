####FINAL CODE for Habitat characteristics couple reproduction & recruitment: the importance of holistic, habitat-centric management

## Download and installation of required packages for SEM 
install.packages("ape")
install.packages("devtools")
install.packages("ggplot2")
install.packages("lavaan")
install.packages("lme4")
install.packages("lmerTest")
install.packages("ncf")
install.packages("nlme")
install.packages("plyr")
install.packages("sp")
install.packages("raster")
install.packages("lavaan.survey")
install.packages("semTools")
install.packages("semPlot")
install.packages("car")
install.packages("MASS")
install.packages("ggplot2")
install.packages("AICcmodavg")
install.packages("tidyverse")
library(devtools)
install_github("jslefche/piecewiseSEM@2.0")
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
require(GGally)
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
require(piecewiseSEM)


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
Pop13DF$s1mm=rowSums(Pop13DF[,c(6,7,10:14)], na.rm=T)    
Pop13DF$s2mm=rowSums(Pop13DF[,c(8,15:17)], na.rm=T)  
Pop13DF$s3mm=rowSums(Pop13DF[,c(9,18:21)], na.rm=T)    
Pop13DF$s4mm=rowSums(Pop13DF[,22:26], na.rm=T)  
Pop13DF$s5mm=rowSums(Pop13DF[,27:29], na.rm=T)  
Pop13DF$s6mm=rowSums(Pop13DF[,30:31], na.rm=T)  
Pop13DF$ReproDensity=Pop13DF[,c("Ovigerous")] 
Pop13DF$QualityHabitat=rowSums(Pop13DF[,c("X1m2.50cm2", "X50cm2.10cm2")], na.rm=T) 
Pop13DF$TotalLiveableHabitat=rowSums(Pop13DF[,c("X1m2.50cm2", "X50cm2.10cm2","X.10cm2..course.sand","bare.cobble...2cm." )], na.rm=T) 
Pop13DF$SexRatio=Pop13DF$Females.All/Pop13DF$Males.All

Pop13DF$s1to3mm=rowSums(Pop13DF[,c("s1mm", "s2mm", "s3mm")], na.rm=T)
Pop13DF$GregSettlement=rowSums(Pop13DF[,c("s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")], na.rm=T) 
Pop13DF$Greater10adults=rowSums(Pop13DF[,c("X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")], na.rm=T) 
Pop13DF$s4to6mm=rowSums(Pop13DF[,c("s4mm","s5mm","s6mm")], na.rm=T)
Pop13DF$s7to9mm=rowSums(Pop13DF[,c("X7","X8","X9")], na.rm=T)
Pop13DF$s10to12mm=rowSums(Pop13DF[,c("X10","X11","X12")], na.rm=T) 
Pop13DF$s13to15mm=rowSums(Pop13DF[,c("X13","X14","X15")], na.rm=T) 
Pop13DF$s16plusmm=rowSums(Pop13DF[,c("X16","X17","X18", "X19", "X20")], na.rm=T) 
Pop13DF$ALL=rowSums(Pop13DF[,c("s1to3mm","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm")], na.rm=T)

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
Pop14DF$ALL=rowSums(Pop14DF[,c("s1to3mm","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm")], na.rm=T)

Pop13COMB=Pop13DF[,c("Site", "Year", "ReproDensity", "QualityHabitat", "TotalLiveableHabitat","s1to3mm","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm","s1mm","s2mm","s3mm","s4mm","s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X.1m2", "X1m2.50cm2", "X50cm2.10cm2", "X.10cm2..course.sand", "cemented.rock.1m2.10cm2", "bare.cobble...2cm.", "bare.sand..fine.very.course.sand.", "rocky.bench", "Gravel", "shell.hash", "tidepool", "wrack", "ALL" )]
Pop14COMB=Pop14DF[,c("Site", "Year", "ReproDensity", "QualityHabitat", "TotalLiveableHabitat","s1to3mm","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm","s1mm","s2mm","s3mm","s4mm","s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X.1m2", "X1m2.50cm2", "X50cm2.10cm2", "X.10cm2..course.sand", "cemented.rock.1m2.10cm2", "bare.cobble...2cm.", "bare.sand..fine.very.course.sand.", "rocky.bench", "Gravel", "shell.hash", "tidepool", "wrack", "ALL")]
Pop1314=rbind(Pop13COMB, Pop14COMB)
#View(Pop1314)

melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(3:44), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314
P1314cast=dcast(P1314, Site+Year~variable, value.var="mean")

setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015")
SiteInfo=read.csv("FINAL Petro site info 2013 2014.csv", header=TRUE)

#PopSite1314 is all data
PopSite1314=merge(Pop1314,SiteInfo, by=c("Site","Year"), all=T)
PopSite1314AVG=merge(P1314cast,SiteInfo, by=c("Site","Year"))

#PopSite1314AVG is averages by site data
PopSite1314AVG$TotalRepros=(PopSite1314AVG$ReproDensity*4)*PopSite1314AVG$Size.1
PopSite1314AVG$TotalEmbryos=(PopSite1314AVG$TotalRepros*PopSite1314AVG$Embryos.per.ovig)
PopSite1314AVG$TotalRec=(PopSite1314AVG$s1to3mm*4)*PopSite1314AVG$Size.1
PopSite1314AVG$AllReproAge=(PopSite1314AVG$s7to9mm+PopSite1314AVG$s10to12mm+PopSite1314AVG$s13to15mm+PopSite1314AVG$s16plusmm)
PopSite1314AVG$Avggrainsize=((PopSite1314AVG$X1m2.50cm2*75)+(PopSite1314AVG$X50cm2.10cm2*30)+(PopSite1314AVG$X.10cm2..course.sand*4)+(PopSite1314AVG$bare.cobble...2cm.*2)+(PopSite1314AVG$bare.sand..fine.very.course.sand.*1))/100
PopSite1314AVG$Allgreater4=(PopSite1314AVG$s4to6mm+PopSite1314AVG$s7to9mm+PopSite1314AVG$s10to12mm+PopSite1314AVG$s13to15mm+PopSite1314AVG$s16plusmm)
PopSite1314AVG$AngleIncid=PopSite1314AVG$Coastline.reg.0.5.km-294
PopSite1314AVG$Difffrom294AngIncid=PopSite1314AVG$Heading.N-294
PopSite1314AVG$Difffrom300AngIncid=PopSite1314AVG$Heading.N-300
PopSite1314AVG$Difffrom300Coast=PopSite1314AVG$X1kmcoastlineangle-300
PopSite1314AVG$Difffrom300Heading=PopSite1314AVG$Heading-300
PopSite1314AVG$ReprominusRecs=PopSite1314AVG$ReproDensity-PopSite1314AVG$s1to3mm
PopSite1314AVG$X1coastminus300=PopSite1314AVG$X1kmcoastlineangle-300
PopSite1314AVG$X1coastminus90=PopSite1314AVG$X1kmcoastlineangle-90
PopSite1314AVG$Grain2=PopSite1314AVG$Avggrainsize*PopSite1314AVG$Avggrainsize
PopSite1314AVG$TotEmLog=log(PopSite1314AVG$TotalEmbryos)
PopSite1314AVG$TotRecLog=log(PopSite1314AVG$TotalRec)
PopSite1314AVG$YearFac=as.factor(PopSite1314AVG$Year)

P13=subset(PopSite1314AVG, Year=="2013")
P14=subset(PopSite1314AVG, Year=="2014")




###MAP OF STUDY AREA
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA")
SI=read.csv("Site info.csv")

states <- map_data("state")
ca <- subset(states, region %in% c("california", "oregon"))

ggplot(data = ca) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "black") + 
  coord_fixed(1.3)

#MAP OF WHOLE AREA
sbbox <- make_bbox(lon = SI$Lon, lat = SI$Lat, f = 1.02276)
sbbox

sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")

ggmap(sq_map) + geom_point(data = SI, mapping = aes(x = Lon, y = Lat, color=Type, size=Size.1)) + geom_text(data = SI, aes(x = Lon, y = Lat, label=Site), size = 2.5, vjust = 0.5, hjust = -0.6, color="cornsilk") + scale_colour_manual(values=c("coral1", "cadetblue2")) + theme(legend.position="none") + ylab("") + xlab("")



##EXPLORE
ggpairs(P14[,c(87, 98, 34:39)])
ggpairs(P14[,c(98, 48:54,61:65,67)])
nums <- unlist(lapply(PopSite1314AVG, is.numeric))  
PopNums=PopSite1314AVG[ , nums]
z = cor(PopNums)
z[abs(z)<0.5]=0
zdf=as.data.frame(as.table(z))

ggplot(data = PopSite1314AVG, aes(x=X1kmcoastlineangle, y=Heading, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x=Heading, y=log(Size.1), color=Site))+geom_point()+ facet_grid(Year~.)


Rel<-lm(Surfzone.size..m.~ Diff300Protneg + Avggrainsize , data = P14)
summary(Rel)

ggplot(data = PopSite1314AVG, aes(x=Slope.average, y=Avggrainsize, color=Site))+geom_point()+ facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x=Difffrom294AngIncid, y=s1to3mm, color=Site))+geom_point()+ facet_grid(Year~.)

###TOTAL REPRODUCTIVE OUTPUT BY RECRUITMENT
library(nlme)
fm2 <- lme(log(TotalEmbryos) ~ log(TotalRec), data = PopSite1314AVG, random = ~ 1|Year)
summary(fm2)
ranef(fm2)


###FIGURE 1- relationship between reproduction and recruitment
library(effects)
library(lme4)
library(ggplot2)

m2013 <- lm(TotRecLog~ TotEmLog, data=subset(PopSite1314AVG, Year=="2013"))
summary(m2013)
m2013 <- lm(TotalRec~ TotalEmbryos, data=subset(PopSite1314AVG, Year=="2013"))
summary(m2013)

m2014 <- lm(TotRecLog~ TotEmLog, data=subset(PopSite1314AVG, Year=="2014"))
summary(m2014)
m2014 <- lm(TotalRec~ TotalEmbryos, data=subset(PopSite1314AVG, Year=="2014"))
summary(m2014)

m1 <- lmer(TotRecLog~ TotEmLog + (1|Year),PopSite1314AVG)
summary(m1)

final1 <- ggplot(data=PopSite1314AVG, mapping=aes(x=TotEmLog, y=TotRecLog, colour=YearFac)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +   
  ylab("log(Total Recruits)") + 
  xlab("log(Total Reproduction)") +
  labs(color="Year")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+geom_text(aes(label=Site),hjust=1, vjust=1)

final1

cor.test(log(PopSite1314AVG$TotalEmbryos), log(PopSite1314AVG$TotalRec), method="pearson")
sub2013=subset(PopSite1314AVG, Year==2013)
cor.test(log(sub2013$TotalEmbryos), log(sub2013$TotalRec), method="pearson")

sub2014=subset(PopSite1314AVG, Year==2014)
cor.test(log(sub2014$TotalEmbryos), log(sub2014$TotalRec), method="pearson")

seewolog <- ggplot(data=PopSite1314AVG, mapping=aes(x=TotalEmbryos, y=TotalRec, colour=YearFac)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +   
  ylab("Total Recruits") + 
  xlab("Total Reproduction") +
  labs(color="Year")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+geom_text(aes(label=Site),hjust=1, vjust=1) + geom_abline()

seewolog


####FINAL STRUCTURAL EQUATION MODEL- FINAL!
#COBBLE
Sed <-lme(Avggrainsize ~ X1coastminus90, random=~1|Year, data = PopSite1314AVG)
summary(Sed)

#COBBLE
Cob <-lme(X50cm2.10cm2 ~ Avggrainsize, random=~1|Year, data = PopSite1314AVG)
summary(Cob)

Cobplot <- ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = X50cm2.10cm2, color=as.factor(PopSite1314AVG$Year)))+geom_point(position = position_jitter(w = 3, h = 0))+ylab("Habitat quality (density of medium cobble)") +xlab("Average sediment size (mm)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
Cobplot


#Habitat Size
HabSize <-lme(log(Size.1) ~ X1coastminus90, random=~1|Year, data = PopSite1314AVG)
summary(HabSize)

Habplot <- ggplot(data = PopSite1314AVG, aes(x = X1coastminus90, y = log(Size.1), color=as.factor(PopSite1314AVG$Year)))+geom_point(position = position_jitter(w = 3, h = 0))+ylab(bquote('log(total habitat size('~m^2*'))')) +xlab("coastline orientation (degrees)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
Habplot

ggplot(data = PopSite1314AVG, aes(x = X1coastminus90, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)


##Density of recruits
Recruit <-lme(s1to3mm ~ X1coastminus90, random=~1|Year, data = PopSite1314AVG)
summary(Recruit)


DenRecplot <- ggplot(data = PopSite1314AVG, aes(x = X1coastminus90, y = s1to3mm, color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('density of recruits ('~number~ per~ m^2*')')) +xlab("coastline orientation (degrees)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
DenRecplot

ggplot(data = PopSite1314AVG, aes(x = X1coastminus90, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)


##Total recruits
TotRecruit <-lme(log(TotalRec) ~ s1to3mm + log(Size.1), random=~1|Year, data = PopSite1314AVG)
summary(TotRecruit)


TotRecplot1 <- ggplot(data = PopSite1314AVG, aes(y = log(TotalRec), x = s1to3mm, color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('log(total recruits)')) +xlab(bquote('density of recruits ('~number~ per~ m^2*')'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
TotRecplot1

TotRecplot2 <- ggplot(data = PopSite1314AVG, aes(y = log(TotalRec), x =  log(Size.1), color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('log(total recruits)')) +xlab(bquote('log(total habitat size('~m^2*'))'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
TotRecplot2


ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = log(TotalRec), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = s1to3mm, y = log(TotalRec), color=Site))+geom_point()+facet_grid(Year~.)

##Density of ovigerous
DenOvig <-lme(ReproDensity ~  X50cm2.10cm2 + s1to3mm, random=~1|Year, data = PopSite1314AVG)
summary(DenOvig)

#DenOvig <-lme(ReproDensity ~  X50cm2.10cm2 + log(Size.1) + s1to3mm, random=~1|Year, data = PopSite1314AVG)
#summary(DenOvig)

Reproplot <- ggplot(data = PopSite1314AVG, aes(y = ReproDensity, x =  X50cm2.10cm2, color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('density of ovigerous females ('~number~ per~ m^2*')')) +xlab(bquote('density of qualtiy habitat ('~percent~cobbles~ per~ m^2*')'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
Reproplot

Reproplot2 <- ggplot(data = PopSite1314AVG, aes(y = ReproDensity, x =  log(Size.1), color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('density of ovigerous females ('~number~ per~ m^2*')')) +xlab(bquote('log(habitat size('~m^2*'))'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
Reproplot2

ggplot(data = PopSite1314AVG, aes(x = X50cm2.10cm2 , y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)

##Total Ovigerous
TotOvig <-lme(log(TotalRepros) ~ log(Size.1) + ReproDensity, random=~1|Year, data = PopSite1314AVG)
summary(TotOvig)

TotReproplot1 <- ggplot(data = PopSite1314AVG, aes(x = ReproDensity, y = log(TotalRepros), color=as.factor(PopSite1314AVG$Year)))+geom_point()+xlab(bquote('density of ovigerous females ('~number~ per~ m^2*')')) +ylab(bquote('log(total ovigerous females)'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
TotReproplot1

TotReproplot2 <- ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = log(TotalRepros), color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('log(total ovigerous females)')) +xlab(bquote('log(habitat size('~m^2*'))'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
TotReproplot2


##Avg ovig size
OvigSize <-lme(Ovigerous.size ~ X50cm2.10cm2, random=~1|Year, data = PopSite1314AVG)
summary(OvigSize)

ovigsizeplot <- ggplot(data = PopSite1314AVG, aes(y = Ovigerous.size, x =  X50cm2.10cm2, color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('average ovigerous size (mm)')) +xlab(bquote('density of qualtiy habitat ('~percent~cobbles~ per~ m^2*')'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
ovigsizeplot

ggplot(data = PopSite1314AVG, aes(x = X50cm2.10cm2, y = Ovigerous.size, color=Site))+geom_point()+facet_grid(Year~.)

##Total Embryos
TotReproOut <-lme(log(TotalEmbryos) ~ log(TotalRepros) + Ovigerous.size , random=~1|Year, data = PopSite1314AVG)
summary(TotReproOut)

embryoplot1 <- ggplot(data = PopSite1314AVG, aes(y = log(TotalEmbryos) , x =  log(TotalRepros), color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('log(total reproductive output/embryos)')) +xlab(bquote('log(total reproductive females)'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
embryoplot1

embryoplot2 <- ggplot(data = PopSite1314AVG, aes(y = log(TotalEmbryos) , x =  Ovigerous.size , color=as.factor(PopSite1314AVG$Year)))+geom_point()+ylab(bquote('log(total reproductive output/embryos)')) +xlab(bquote('average ovigerous size (mm)'))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) + scale_color_discrete(name="Year")#+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
embryoplot2


ggplot(data = PopSite1314AVG, aes(x = Ovigerous.size, y = log(TotalEmbryos), color=Site))+geom_point()+facet_grid(Year~.)

ReproRecModel <- psem(Sed, Cob, HabSize, Recruit, TotRecruit, DenOvig, TotOvig, OvigSize, TotReproOut) 
summary(ReproRecModel)
coefs(ReproRecModel)
rsquared(ReproRecModel)

#grid.arrange(Habplot, DenRecplot, TotRecplot1, TotRecplot2, Reproplot, TotReproplot1, TotReproplot2, ovigsizeplot, embryoplot1, embryoplot2)

##11 * 8.5
grid.arrange(Habplot, DenRecplot, TotRecplot1, TotRecplot2, Cobplot, Reproplot)
grid.arrange(ovigsizeplot, TotReproplot1, TotReproplot2,  embryoplot1, embryoplot2)

##PLOT OF WAVETIME and RECS
recwave <-lm(s1to3mm ~ Wavetime, data = PopSite1314AVG)
summary(recwave)

recwaveplot <- ggplot(data = PopSite1314AVG, aes(x = Wavetime, y = s1to3mm))+geom_point(position = position_jitter(w = 3, h = 0))+ylab("recruit density (#/m2)") +xlab("Wavetime (sec)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F) #+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
recwaveplot

#COASTLINE TO WAVETIMES
##2013  MAX WAVETIMES to X1km
waveorientmax <-lm(Wave.time.Max~ X1coastminus90, data = PopSite1314AVG)
summary(waveorientmax)

waveorientmaxplot <- ggplot(data = PopSite1314AVG, aes(x = X1coastminus90, y = Wave.time.Max))+geom_point(position = position_jitter(w = 0, h = 0))+ylab("Maximum time for waves to cross surfzone (sec)") +xlab("Coastline orientation (degrees)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F, color="black") #+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
waveorientmaxplot

#min wavetime
waveorientmin <-lm(Wave.time.Min~ X1coastminus90, data = PopSite1314AVG)
summary(waveorientmin)


waveorientminplot <- ggplot(data = PopSite1314AVG, aes(x = X1coastminus90, y = Wave.time.Min))+geom_point(position = position_jitter(w = 0, h = 0))+ylab("Minimum time for waves to cross surfzone (sec)") +xlab("Coastline orientation (degrees)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F, color="black") #+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
waveorientminplot

#max wave ht
PopSite1314AVG$wavehtMaxm=PopSite1314AVG$WavehtMax/3
waveht <-lm(wavehtMaxm~ X1coastminus90, data = PopSite1314AVG)
summary(waveht)


wavehtmaxplot <- ggplot(data = PopSite1314AVG, aes(x = X1coastminus90, y = wavehtMaxm))+geom_point(position = position_jitter(w = 0, h = 0))+ylab("Maximum wave height (m)") +xlab("Coastline orientation (degrees)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))# + geom_smooth(method='lm', se=F, color="black") #+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
wavehtmaxplot

grid.arrange(waveorientmaxplot,waveorientminplot,wavehtmaxplot, ncol=3)

##
plot(X1coastminus90~WavehtMax, PopSite1314AVG)



##MODEL OF SIZE 1 to SIZE 2
summary(lm(data=PopSite1314AVG,Size.1~Size.2))

###OTHER FIGURES!!!!!
###plot sediment against exposure
ggplot(data = P14, aes(y = X1coastminus90, x = X50cm2.10cm2, color= Site, size=Size.1)) + geom_point()+geom_text(aes(label=Site),hjust=0, vjust=0) #+ xlab("Coastline orientation (degrees)") + ylab("Average grain size") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = P14, aes(x = X1coastminus90, y = X50cm2.10cm2)) + geom_point()+geom_text(aes(label=Site),hjust=0, vjust=0)+ xlab("Exposure: Coastline orientation (degrees)") + ylab("Habitat quality: density of medium cobbles ") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = P13, aes(y = X1coastminus90, x = X50cm2.10cm2, color= Site)) + geom_point()+geom_text(aes(label=Site),hjust=0, vjust=0) #+ xlab("Coastline orientation (degrees)") + ylab("Average grain size") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot_ly(P14, x = ~X1coastminus90, y = ~X50cm2.10cm2, z = ~Size.1, color = ~Site) 

plot_ly(P14, x = ~s1to3mm, y = ~ReproDensity, z = ~Size.1, color = ~Site) 


ggplot(data = P14, aes(x = Size.1, y = s1to3mm, color= as.factor(Site))) + geom_point()+geom_text(aes(label=Site),hjust=0, vjust=0) #+ xlab("Coastline orientation (degrees)") + ylab("Average grain size") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
m1 <- lmer(s1to3mm~ Size.1 + (1|Year),PopSite1314AVG)
summary(m1)
m1 <- lm(s1to3mm~ Size.1,data=P13)
summary(m1)
m1 <- lm(s1to3mm~ Size.1,data=P14)
summary(m1)




##plot
ggplot(data=P14, mapping=aes(x=TotEmLog, y=TotRecLog)) +
  geom_point() +
  geom_smooth(method='lm', color="black") +   
  ylab("log(Total Recruits)") + 
  xlab("log(Total Reproduction)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(color = "black"))+geom_text(aes(label=Site),hjust=0, vjust=0)

ggplot(data=P14, mapping=aes(x=TotEmLog, y=TotRecLog)) +
  geom_point() +   
  ylab("log(Total Recruits)") + 
  xlab("log(Total Reproduction)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(color = "black"))+geom_text(aes(label=Site),hjust=0, vjust=0) + geom_abline(slope = 1)


summary(lm(log(Size.1)~X1coastminus90, PopSite1314AVG))

ggplot(data=P13, mapping=aes(x=TotEmLog, y=TotRecLog)) +
  geom_point() +
  geom_smooth(method='lm') +   
  ylab("log(Total Recruits)") + 
  xlab("log(Total Reproduction)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+geom_text(aes(label=Site),hjust=0, vjust=0)

ggplot(data=P14, mapping=aes(y=s1to3mm, x=ReproDensity, color=Site,size=Size.1)) +
  geom_point() +
  geom_smooth(method='lm') +   
  xlab("Repro Density") + 
  ylab("Recruit Density") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+geom_text(aes(label=Site),hjust=0, vjust=0)

ggplot(data=P13, mapping=aes(y=s1to3mm, x=ReproDensity, color=Site)) +
  geom_point() +
  geom_smooth(method='lm') +   
  xlab("Repro Density") + 
  ylab("Recruit Density") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+geom_text(aes(label=Site),hjust=0, vjust=0)

P14$SitereorderE <- factor(P14$Site, levels=c("MK", "CB", "PP", "FB", "MB", "KC", "PSG", "AC", "WP", "TC", "HC", "AP"))

ggplot(data=P14, mapping=aes(y=s1to3mm, x=Sitereorder, fill=SitereorderE)) + geom_bar(stat="identity") + coord_flip()

P14$SitereorderS <- factor(P14$Site, levels=c("KC","PSG", "PP", "HC", "MB", "TC", "WP", "FB", "MK", "CB", "AP", "AC"))

ggplot(data=P14, mapping=aes(y=ReproDensity, x=SitereorderS)) + geom_bar(stat="identity") + coord_flip()

ggplot(data=P14, mapping=aes(y=ReproDensity, x=SitereorderE)) + geom_bar(stat="identity") + coord_flip()


P14$SitereorderE2 <- factor(P14$Site, levels=c("AP", "HC", "TC", "WP", "AC", "PSG", "KC", "MB", "FB", "PP", "CB", "MK"))
melted <- melt(P14, id.vars=c("SitereorderE2"), measure.vars = c("s1to3mm","ReproDensity"), na.rm=T)
melted$recruits = melted$s1to3mm * 4
ggplot(data=melted, mapping=aes(y=value, x=SitereorderE2, fill=variable)) + geom_bar(stat="identity", position='dodge') +xlab("Site") +ylab("individuals per m2")



###SIZE CLASSES! YES!
Pop1314$'1 to 3 mm'= Pop1314$s1to3mm*4
Pop1314$'4 to 6 mm'= Pop1314$s4to6mm*4
Pop1314$'7 to 9 mm'= Pop1314$s7to9mm*4
Pop1314$'10 to 12 mm'= Pop1314$s10to12mm*4
Pop1314$'13 to 15 mm'= Pop1314$s13to15mm*4
Pop1314$'> 16 mm'= Pop1314$s16plusmm*4

require(data.table)
melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(45:50), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314sizecast=dcast(P1314, Site+Year~variable, value.var="mean")
P1314merge=merge(P1314sizecast,SiteInfo, by=c("Site","Year"))

P1314$Site_Orient = factor(P1314$Site,levels=c("AC", "AP", "WP", "TC", "HC","PSG", "KC", "PP","MB", "FB", "MK", "CB"))

ggplot(data = subset(P1314, Year=="2014"), aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Orient~., scale="free") + ylab(bquote('density of individuals ('~number~ per~ m^2*')')) + xlab("Size class")


##2013 Size classes for 
###SIZE CLASSES! YES!
Pop13DF$'1 to 3 mm'= Pop13DF$s1to3mm*4
Pop13DF$'4 to 6 mm'= Pop13DF$s4to6mm*4
Pop13DF$'7 to 9 mm'= Pop13DF$s7to9mm*4
Pop13DF$'10 to 12 mm'= Pop13DF$s10to12mm*4
Pop13DF$'13 to 15 mm'= Pop13DF$s13to15mm*4
Pop13DF$'> 16 mm'= Pop13DF$s16plusmm*4
Pop13DF$'> 10 mm'= (Pop13DF$s16plusmm + Pop13DF$s13to15mm + Pop13DF$s10to12mm) * 4

require(data.table)
melted <- melt(Pop13DF, id.vars=c("Site", "Year", "Week"), measure.vars = c(175:180), na.rm=T)
P13size=ddply(melted, c("variable", "Site", "Year", "Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P13sizecast=dcast(P13size, Site+Year+Week~variable, value.var="mean")
#P13merge=merge(P13sizecast,SiteInfo, by=c("Site","Year", "Week"))

#P13size$Site_Orient = factor(P1314$Site,levels=c("AC", "AP", "WP", "TC", "HC","PSG", "KC", "PP","MB", "FB", "MK", "CB"))

ggplot(data = P13size, aes(x = variable, y = mean))+geom_bar(stat = "identity")  + ylab(bquote('density of individuals ('~number~ per~ m^2*')')) + xlab("Size class") + facet_grid(Site~Week, scale="free")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2)

#ggplot(data = P13size, aes(x = Week, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site~.) + ylab(bquote('density of > 10 mm individuals ('~number~ per~ m^2*')')) + xlab("Sampling Period")

###USEABLE POPULATION
PopSite1314AVG$ActualHabitatSize = PopSite1314AVG$Size.1 * PopSite1314AVG$X50cm2.10cm2

plot(ActualHabitatSize~s1to3mm, PopSite1314AVG)

##CLUSTER BASED
data14 <- P14[,c("Site","X1coastminus90","X50cm2.10cm2","Size.1","ReproDensity", "s1to3mm")]
mydata <- scale(data14[,c(4:6)]) # standardize variables
# K-Means Cluster Analysis
fit <- kmeans(mydata,3) # 2 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata14 <- data.frame(data14, fit$cluster)
# Ward Hierarchical Clustering
d <- dist(mydata[,c(1:3)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, labels=mydata14$Site) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")

#EXPOSURE
data14 <- P14[,c("Site","X1coastminus90","X50cm2.10cm2","Size.1","ReproDensity", "s1to3mm")]
mydata <- scale(data14[,c(2)]) # standardize variables
# K-Means Cluster Analysis
fit <- kmeans(mydata,2) # 2 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata14 <- data.frame(data14, fit$cluster)
# Ward Hierarchical Clustering
d <- dist(mydata[,c(1:3)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, labels=mydata14$Site) # display dendogram
groups <- cutree(fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=2, border="red")

#Habitat quality
data14 <- P14[,c("Site","X1coastminus90","X50cm2.10cm2","Size.1","ReproDensity", "s1to3mm")]
mydata <- scale(data14[,c(3)]) # standardize variables
# K-Means Cluster Analysis
fit <- kmeans(mydata,2) # 2 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata14 <- data.frame(data14, fit$cluster)
# Ward Hierarchical Clustering
d <- dist(mydata[,c(1)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, labels=mydata14$Site) # display dendogram
groups <- cutree(fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")

#Habitat size
data14 <- P14[,c("Site","X1coastminus90","X50cm2.10cm2","Size.1","ReproDensity", "s1to3mm")]
mydata <- data14[,c(4)] # standardize variables
# K-Means Cluster Analysis
fit <- kmeans(mydata,2)# 2 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata14 <- data.frame(data14, fit$cluster)
# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, labels=mydata14$Site) # display dendogram
groups <- cutree(fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=2, border="red")


#RECRUITS
data14 <- P14[,c("Site","X1coastminus90","X50cm2.10cm2","Size.1","ReproDensity", "s1to3mm")]
mydata <- scale(data14[,c(6)]) # standardize variables
# K-Means Cluster Analysis
fit <- kmeans(mydata,2) # 2 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata14 <- data.frame(data14, fit$cluster)
# Ward Hierarchical Clustering
d <- dist(mydata[,c(1)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, labels=mydata14$Site) # display dendogram
groups <- cutree(fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=2, border="red")



plot(X1coastminus90~WavehtMax, PopSite1314AVG)
plot(X1coastminus90~WavehtAvg, PopSite1314AVG)
plot(X1coastminus90~WavehtMin, PopSite1314AVG)

##END



