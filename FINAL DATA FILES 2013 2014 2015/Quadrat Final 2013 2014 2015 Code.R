###QUADRAT FINAL 2013, 2014, 2015 Population Data

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
library(igraph)

##DATA 2013
##PETROS & COBBLE
#set working directory
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA")

#Read in 2013 quadrat data
Pop13=read.csv("17Apr2017petroscobblebios2013.csv", header=TRUE)
Pop13DF=data.frame(Pop13)
attach(Pop13DF)

#change to Date format
class(Pop13DF$Date)
Pop13DF$Date <- as.Date(Pop13DF$Date, format='%m/%d/%y')
class(Pop13DF$Date)
#check columns
unique(Site)
unique(Date)

#subset data set 2013 so only includes up to % flipped so can be merged with 2014
Pop13DFsub=Pop13DF[,c(1:62)]

#2014 Petros and Cobble
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

#combine 2013 2014 
Pop1314=rbind(Pop13DFsub,Pop14DF)

#Consolidated Pop Info
Pop1314$s1mm=rowSums(Pop1314[,10:14], na.rm=T)    
Pop1314$s2mm=rowSums(Pop1314[,15:17], na.rm=T)  
Pop1314$s3mm=rowSums(Pop1314[,18:21], na.rm=T)    
Pop1314$s4mm=rowSums(Pop1314[,22:26], na.rm=T)  
Pop1314$s5mm=rowSums(Pop1314[,27:29], na.rm=T)  
Pop1314$s6mm=rowSums(Pop1314[,30:31], na.rm=T)  
Pop1314$s1to3mm=rowSums(Pop1314[,c("s1mm", "s2mm", "s3mm")], na.rm=T) 

#Consolidated pigs and mm
Pop1314$s1mmcombined=rowSums(Pop1314[,c("s1mm", "Clear")], na.rm=T)  
Pop1314$s2mmcombined=rowSums(Pop1314[,c("s2mm", "Pigmented.1")], na.rm=T)  
Pop1314$s3mmcombined=rowSums(Pop1314[,c("s3mm", "Pigmented.2")], na.rm=T) 
Pop1314$s4mmcombined=rowSums(Pop1314[,c("s4mm", "Pigmented.3")], na.rm=T) 
Pop1314$s1to3mmcombined=rowSums(Pop1314[,c("s1mmcombined", "s2mmcombined", "s3mmcombined")], na.rm=T) 
Pop1314$OvigeroustoFemale=Pop1314$Ovigerous/Pop1314$Females.All
Pop1314$NotLiveable=rowSums(Pop1314[,c("rocky.bench", "cemented.rock.1m2.10cm2", "bare.sand..fine.very.course.sand.")], na.rm=T) 

##SITE INFO
quadratsitemerge1314=merge(Pop1314,SiteInfo,sort=FALSE)
#View(quadratsitemerge1314)
colnames(quadratsitemerge1314)
quadratsitemerge1314$Size.of.opening=as.numeric(quadratsitemerge1314$Size.of.opening)
quadratsitemerge1314$Totalliveable=rowSums(quadratsitemerge1314[,c("X1m2.50cm2", "X50cm2.10cm2", "X.10cm2..course.sand", "bare.cobble...2cm.")], na.rm=T) 

quadsite2013=subset(quadratsitemerge1314, Year=="2013")
quadsite2014=subset(quadratsitemerge1314, Year=="2014")

network_plot(correlate(quadratsitemerge1314[,c(46:49,52:57,79:80,102,107:111,116,126:128)]), min_cor=0.3)
network_plot(correlate(quadsite2013[,c(46:49,52:57,79:80,102,107:111,116,126:128)]), min_cor=0.4)
network_plot(correlate(quadsite2014[,c(46:49,52:57,79:80,102,107:111,116,126:128)]), min_cor=0.3)

network_plot(correlate(quadsite2014[,c(52:58,71:77)]), min_cor=0.5)



###Subset 
quadsite42014=subset(quadratsitemerge1314, Year=="2014")
merge4quadsites2014=subset(quadsite42014, Site=="AC"| Site=="AP" | Site=="FB" | Site=="TC")

quadallsites2014a=subset(quadratsitemerge1314, Year=="2014")
quadallsites2014=subset(quadallsites2014a, Week=="Week 0")

quadallsites2013=subset(quadratsitemerge1314, Year=="2013")

###Are Ovigerous densities different?
ggplot(data = quadratsitemerge1314, aes(x = Site, y = Ovigerous))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(Ovigerous ~ Site + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(Ovigerous ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

###Are recruit densities different?
ggplot(data = quadratsitemerge1314, aes(x = Site, y = recruits3mm))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(recruits3mm ~ Site + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(recruits3mm ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)


###
ggplot(data = quadratsitemerge1314, aes(x = X50cm2.10cm2, y = recruits3mm))+geom_point()+facet_grid(Year~Site)

ggplot(data = quadratsitemerge1314, aes(x = X50cm2.10cm2, y = Ovigerous))+geom_point()+facet_grid(Year~Site)


##PLOTS
ggplot(data = quadratsitemerge1314, aes(x = Total.degrees.opening, y = X50cm2.10cm2, color=GeoType))+geom_point()+facet_grid(Year~.)
##OVIGEROUS BY 50-10cm
ggplot(data = quadratsitemerge1314, aes(x = X50cm2.10cm2, y = Ovigerous, color=Site))+geom_point()+facet_grid(Year~.)

mixed.lmer1 <- lmer(Ovigerous ~ X50cm2.10cm2 +  (1|Year), data = quadratsitemerge1314)
summary(mixed.lmer1)
anova(mixed.lmer1)

##HEADING s1to3mm
ggplot(data = quadratsitemerge1314, aes(x = Heading, y = s1to3mmcombined, color=Site))+geom_point()+facet_grid(Year~.)

mixed.lmer1 <- lmer(Heading ~ s1to3mmcombined +  (1|Year), data = quadratsitemerge1314)
summary(mixed.lmer1)
anova(mixed.lmer1)

##STACKED BARPLOT OF SEDIMENT
melted <- melt(quadratsitemerge1314, id.vars=c("Year", "Same.geo", "Heading"), measure.vars = c(50:53,55:56,54,57), na.rm=T)
sediment1314=ddply(melted, c("Year", "variable", "Same.geo", "Heading"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sed=ggplot(data = sediment1314, aes(x = Heading, y = mean, fill=Same.geo))+geom_bar(stat = "identity", width=0.5)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(variable~Year)#+scale_fill_manual(values = c("chocolate", "orangered3", "firebrick3", "darkgoldenrod1", "grey80", "burlywood", "darkblue", "black"))
sed

##STACKED BARPLOT OF SIZE CLASSES
melted <- melt(quadratsitemerge1314, id.vars=c("Year", "Total.degrees.opening"), measure.vars = c(71:77), na.rm=T)
size1314=ddply(melted, c("Total.degrees.opening", "Year", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
size=ggplot(data = size1314, aes(x = Total.degrees.opening, y = mean, fill=variable))+geom_bar(stat = "identity", width=2)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(.~Year, scale="free")#+scale_fill_manual(values = c("darkred", "orangered3", "chocolate", "darkgoldenrod1", "grey80", "burlywood", "darkblue", "black"))

grid.arrange(sed,size)

##STACKED BARPLOT OF SIZE CLASSES BY SITE
melted <- melt(quadratsitemerge1314, id.vars=c("Year", "Site"), measure.vars = c(71:77), na.rm=T)
size1314=ddply(melted, c("Site", "Year", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
size=ggplot(data = size1314, aes(x = Site, y = mean, fill=variable))+geom_bar(stat = "identity", width=1)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(.~Year, scale="free")#+scale_fill_manual(values = c("darkred", "orangered3", "chocolate", "darkgoldenrod1", "grey80", "burlywood", "darkblue", "black"))
size

melted <- melt(quadratsitemerge1314, id.vars=c("Year", "Site"), measure.vars = c(49), na.rm=T)
size1314=ddply(melted, c("Site", "Year", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ovig=ggplot(data = size1314, aes(x = Site, y = mean, fill=variable))+geom_bar(stat = "identity", width=1)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(.~Year, scale="free")#+scale_fill_manual(values = c("darkred", "orangered3", "chocolate", "darkgoldenrod1", "grey80", "burlywood", "darkblue", "black"))
ovig

#SED BY SITE
melted <- melt(quadratsitemerge1314, id.vars=c("Year", "Site"), measure.vars = c(50:53,55:56,54,57), na.rm=T)
sediment1314=ddply(melted, c("Site", "Year", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sed=ggplot(data = sediment1314, aes(x = Site, y = mean, fill=variable))+geom_bar(stat = "identity", width=0.4)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(.~Year, scale="free")#+scale_fill_manual(values = c("chocolate", "orangered3", "firebrick3", "darkgoldenrod1", "grey80", "burlywood", "darkblue", "black"))
sed


head=ggplot(data = mergesite1314, aes(x = Site, y = Heading))+geom_bar(stat = "identity", width=0.4)+theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+facet_grid(.~Year, scale="free")#+scale_fill_manual(values = c("chocolate", "orangered3", "firebrick3", "darkgoldenrod1", "grey80", "burlywood", "darkblue", "black"))

grid.arrange(ovig,size)
grid.arrange(sed,head)

##OVIGEROUS BY 50-10cm
melted <- melt(quadratsitemerge1314, id.vars=c("Year","Site"), measure.vars = c(50:57,69,83:90,81), na.rm=T)
sediment1314=ddply(melted, c("Site", "Year", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
sed1314cast=cast(sediment1314, Year+Site~variable, value="mean")

ggplot(data = sed1314cast, aes(x = NotLiveable, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~., scale="free")

###GENERALIZED LINEAR MODELS
##
hist(quadratsitemerge1314$s1to3mm)
##check distribution of dependent variables
quadratsitemerge1314$s1to3mm.t <- quadratsitemerge1314$s1to3mm + 1
qqp(quadratsitemerge1314$s1to3mm.t, "norm")
qqp(quadratsitemerge1314$s1to3mm.t, "lnorm")
nbinom <- fitdistr(quadratsitemerge1314$s1to3mm.t, "Negative Binomial")
qqp(quadratsitemerge1314$s1to3mm.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(quadratsitemerge1314$s1to3mm.t, "Poisson")
qqp(quadratsitemerge1314$s1to3mm.t, "pois", poisson$estimate)
gamma <- fitdistr(quadratsitemerge1314$s1to3mm.t, "gamma")
qqp(quadratsitemerge1314$s1to3mm.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

mixed.lmer <- glmer(s1to3mm ~ Heading + (1|Year), data = quadratsitemerge1314, family="poisson")

###1to3mm by Heading
mixed.lmer <- lmer(s1to3mm ~ Heading + (1|Year), data = quadratsitemerge1314)
summary(mixed.lmer)
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))
plot
#compare models
mixed.lmer <- lmer(s1to3mm ~ Heading + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(s1to3mm ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)


###1to3mm by Heading
mixed.lmer <- glmer(s1to3mm ~ Heading + (1|Year), data = quadratsitemerge1314, family="poisson")
summary(mixed.lmer)
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))
plot
#compare models
mixed.lmer <- lmer(s1to3mm ~ Heading + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(s1to3mm ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

#Opening and heading
ggplot(data = quadratsitemerge1314, aes(x = Heading, y = s1to3mmcombined, color=Site))+geom_point()+facet_grid(Year~.)

mixed.lmer1 <- lmer(Heading ~ Total.degrees.opening*Heading +  (1|Year), data = quadratsitemerge1314)
summary(mixed.lmer1)
anova(mixed.lmer1)


##
melted <- melt(quadratsitemerge1314, id.vars=c("Site", "Year", ""), measure.vars = c(6:84), na.rm=T)
all1314quad=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
all1314quad



###OTHER
ggplot(data = Pop13DF, aes(x = Gobiesox.maeandricus.ALL, y = X12, color=Site))+geom_point()

rb=ggplot(data = quadratsitemerge1314, aes(x = Heading, y = rocky.bench, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = quadratsitemerge1314, aes(x = Size.2, y = rocky.bench, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = quadratsitemerge1314, aes(x = Total.degrees.opening, y = rocky.bench, color=Site, size=Size.2))+geom_point()+facet_grid(Year~.)

ggplot(data = quadratsitemerge1314, aes(x = Heading, y = Size.2, color=Site))+geom_point()+facet_grid(Year~.)

cr=ggplot(data = quadratsitemerge1314, aes(x = Heading, y = cemented.rock.1m2.10cm2, color=Site))+geom_point()+facet_grid(Year~.)

grid.arrange(rb,cr)

ggplot(data = quadratsitemerge1314, aes(x = Total.degrees.opening, y = recruits3mm, color=Site))+geom_point()+facet_grid(Year~Size.cat+Exposed.to.wNW.Swell..300.)

#TEMP
prewatertemp13$date <- as.Date(prewatertemp13$Date,format="%m/%d/%y")
prewatertemp13$date
watertempsub=subset(prewatertemp13, date > as.Date("2013-05-31") & date < as.Date("2013-08-14"))

ggplot(data = watertempsub, aes(x = Date.Time, y = Water.Temp.C, color=Site))+geom_point()

ggplot(data = Pop13DF, aes(x = Date, y = recruits3mm, color=Site))+geom_point()

###CLUSTER
# K-Means Cluster Analysis-SEDIMENT
sub2014=subset(Pop1314, Year =="2014")
NA14=na.omit(sub2014[,c(1,52:57)])
fit <- kmeans(NA14[,c(2:7)], 2)
# get cluster means 
aggregate(NA14[,c(2:7)],by=list(fit$cluster),FUN=mean)
# append cluster assignment
Clus14 <- data.frame(NA14, fit$cluster)

melted <- melt(Clus14, id.vars=c("Site", "fit.cluster"), measure.vars = c(2:7), na.rm=T)
sediment14=ddply(melted, c("Site", "fit.cluster", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = sediment14, aes(x = fit.cluster, y = mean, color=Site))+geom_point()+facet_grid(variable~.)

# K-Means Cluster Analysis- SIZE ALL
sub2014=subset(Pop1314, Year =="2014")
NAsize2014=na.omit(sub2014[,c(1,63:68,32:45)])
fit <- kmeans(NAsize2014[,c(2:21)], 2)
# get cluster means 
aggregate(NAsize2014[,c(2:21)],by=list(fit$cluster),FUN=mean)
# append cluster assignment
ClusSize14 <- data.frame(NAsize2014, fit$cluster)

melted <- melt(ClusSize14, id.vars=c("Site", "fit.cluster"), measure.vars = c(2:21), na.rm=T)
size14=ddply(melted, c("Site", "fit.cluster", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = size14, aes(x = fit.cluster, y = mean, color=Site))+geom_point()+facet_grid(variable~.)

melted <- melt(ClusSize14, id.vars=c("Site"), measure.vars = c("fit.cluster"), na.rm=T)
size14=ddply(melted, c("Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = size14, aes(x = Site, y = mean))+geom_point()

# K-Means Cluster Analysis- SIZE
sub2014=subset(Pop1314, Year =="2014")
NAsize2014=na.omit(sub2014[,c(1,71:77)])
fit <- kmeans(NAsize2014[,c(3:8)], 2)
# get cluster means 
aggregate(NAsize2014[,c(3:8)],by=list(fit$cluster),FUN=mean)
# append cluster assignment
ClusSize14 <- data.frame(NAsize2014, fit$cluster)

melted <- melt(ClusSize14, id.vars=c("Site", "fit.cluster"), measure.vars = c(2:8), na.rm=T)
size14=ddply(melted, c("Site", "fit.cluster", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = size14, aes(x = fit.cluster, y = mean, color=Site))+geom_point()+facet_grid(variable~.)

melted <- melt(ClusSize14, id.vars=c("Site"), measure.vars = c("fit.cluster"), na.rm=T)
size14=ddply(melted, c("Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = size14, aes(x = Site, y = mean))+geom_point()+facet_grid(variable~.)

# K-Means Cluster Analysis- SLOPE/HAB SIZE
sub2014=subset(quadratsitemerge1314, Year =="2014")
NAslope2014=na.omit(sub2014[,c(1,117:119,95)])
fit <- kmeans(NAslope2014[,c(2:5)], 2)
# get cluster means 
aggregate(NAslope2014[,c(2:5)],by=list(fit$cluster),FUN=mean)
# append cluster assignment
clusslope14 <- data.frame(NAslope2014, fit$cluster)

melted <- melt(clusslope14, id.vars=c("Site", "fit.cluster"), measure.vars = c(2:5), na.rm=T)
size14=ddply(melted, c("Site", "fit.cluster", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = size14, aes(x = fit.cluster, y = mean, color=Site))+geom_point()+facet_grid(variable~.)


# K-Means Cluster Analysis- HEADING INFO
sub2014=subset(quadratsitemerge1314, Year =="2014")
NAhead2014=na.omit(sub2014[,c(1,108,100:103,99)])
fit <- kmeans(NAhead2014[,c(2:7)], 2)
# get cluster means 
aggregate(NAhead2014[,c(2:7)],by=list(fit$cluster),FUN=mean)
# append cluster assignment
clushead14 <- data.frame(NAhead2014, fit$cluster)

melted <- melt(clushead14, id.vars=c("Site", "fit.cluster"), measure.vars = c(2:7), na.rm=T)
size14=ddply(melted, c("Site", "fit.cluster", "variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = size14, aes(x = fit.cluster, y = mean, color=Site))+geom_point()+facet_grid(variable~.)
