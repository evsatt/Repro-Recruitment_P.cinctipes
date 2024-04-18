###2002 Donahue data Population Data

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

##DATA 2002
#set working directory
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015")

#Read in 2013 quadrat data
Pop2002a=read.csv("2002PetroPredCompCobble.csv", header=TRUE)
Pop2002=data.frame(Pop2002a)
attach(Pop2002)

colnames(Pop2002)

#Size structure
melted <- melt(Pop2002, id.vars=c("Site"), measure.vars = c(6:15), na.rm=T)
OF=ddply(melted, c("Site","variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
OF
ggplot(data = OF, aes(x = variable, y = mean))+geom_bar(stat = "identity", size=2, position=position_dodge(width=1))+facet_grid(Site~., scale="free")

#Ovigerous
melted <- melt(Pop2002, id.vars=c("Site"), measure.vars = c(17), na.rm=T)
OF=ddply(melted, c("Site","variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
OF
ggplot(data = OF, aes(x = Site, y = mean))+geom_bar(stat = "identity", size=2, position=position_dodge(width=1))

#
melted <- melt(Pop2002, id.vars=c("Site"), measure.vars = c(6:7), na.rm=T)
OF=ddply(melted, c("Site","variable"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
OF
ggplot(data = OF, aes(x = Site, y = mean, fill=variable))+geom_bar(stat = "identity", size=2, position=position_dodge(width=1))+geom_errorbar(data=OF, aes(ymin=mean-sem, ymax=mean+sem, width=0.1),position=position_dodge(width=1))

#Means for SITES
melted <- melt(Pop2002, id.vars=c("Site"), measure.vars = c(5:17,19:42), na.rm=T)
melt2002=ddply(melted, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
melt2002

cast2002=cast(melt2002, Site~variable, value="mean")
cast2002
#NETWORK PLOT- Total PC and OVIGEROUS have NAs
network_plot(correlate(cast2002[,c(2:12,15:32)]), min_cor=0.7)


