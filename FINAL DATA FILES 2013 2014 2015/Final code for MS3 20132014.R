


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
View(Pop1314)

melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(3:44), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314
P1314cast=dcast(P1314, Site+Year~variable, value.var="mean")

setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015")
SiteInfo=read.csv("FINAL Petro site info 2013 2014.csv", header=TRUE)

PopSite1314=merge(Pop1314,SiteInfo, by=c("Site","Year"))

View(PopSite1314)

PopSite1314NANO <- PopSite1314[complete.cases(PopSite1314[ ,c("Site", "Year", "ReproDensity", "QualityHabitat", "TotalLiveableHabitat", "s1to3mm", "Slope.average", "Slope.min", "Width.average","Surfzone.size..m.", "Coastline.hab.angle", "Heading.minus.coast", "Total.degrees.opening")]),c("Site", "Year", "ReproDensity", "QualityHabitat", "TotalLiveableHabitat", "s1to3mm", "Slope.average", "Slope.min", "Width.average","Surfzone.size..m.", "Coastline.hab.angle", "Heading.minus.coast","Total.degrees.opening")]
attach(PopSite1314NANO)
unique(PopSite1314NANO$Year)

##FULL MODEL 2013/2014

PopSite1314NANO$OvigerousLOG=log(PopSite1314NANO$ReproDensity+0.00001)
PopSite1314NANO$s1to3mmLOG=log(PopSite1314NANO$s1to3mm+0.00001)
OvigMNB <-lme(OvigerousLOG ~ s1to3mmLOG + QualityHabitat, random=~1|Site/Year, data = PopSite1314NANO)
summary(OvigMNB)

hist(s1to3mm)
PopSite14NANO$s1to3mmLOG=log(PopSite14NANO$s1to3mm+0.00001)
RecMNB <-lme(s1to3mmLOG ~ Surfzone.size..m. + QualityHabitat, random=~1|Site/Year, data = PopSite1314NANO)
summary(RecMNB)

hist(X50cm2.10cm2)
RockMNB<-lme(QualityHabitat ~ Heading, random=~1|Site/Year, data = PopSite1314NANO)
summary(RockMNB)

plotmodel <- psem(OvigMNB, RecMNB,RockMNB) 
summary(plotmodel)



##FULL MODEL 2013/2014

PopSite1314NANO$OvigerousLOG=log(PopSite1314NANO$ReproDensity+0.00001)
PopSite1314NANO$s1to3mmLOG=log(PopSite1314NANO$s1to3mm+0.00001)
OvigMNB <-lme(OvigerousLOG ~ s1to3mmLOG + QualityHabitat, random=~1|Year/Site, data = PopSite1314NANO)
summary(OvigMNB)

hist(s1to3mm)
PopSite1314NANO$s1to3mmLOG=log(PopSite1314NANO$s1to3mm+0.00001)
RecMNB <-lme(s1to3mmLOG ~ Slope.min + QualityHabitat, random=~1|Year/Site, data = PopSite1314NANO)
summary(RecMNB)

hist(QualityHabitat)
RockMNB<-lme(QualityHabitat ~ Slope.min, random=~1|Year/Site, data = PopSite1314NANO)
summary(RockMNB)

plotmodel <- psem(OvigMNB, RecMNB,RockMNB) 
summary(plotmodel)


ggplot(data = PopSite1314NANO, aes(x = ReproDensity, y = s1to3mm, color=Site))+geom_point()+ facet_grid(Year~.)
Larv<-lme(s1to3mm ~ ReproDensity, random=~1|Year/Site, data = PopSite1314, na.action=na.omit)
summary(Larv)

###SIZE STRUCTURES*****
require(data.table)
setnames(Pop1314, old = c("s1to3mm","s1mm","s2mm","s3mm","s4mm","s5mm","s6mm","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20"), new = c("recruits","1mm","2mm","3mm","4mm","5mm","6mm","7mm","8mm","9mm","10mm","11mm","12mm","13mm","14mm","15mm","16mm","17mm","18mm","19mm","20mm"))
melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(6:11), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314sizecast=dcast(P1314, Site+Year~variable, value.var="mean")
P1314merge=merge(P1314sizecast,SiteInfo, by=c("Site","Year"))

#P1314$Site_Slope = factor(P1314$Site, levels=c('AP','TC','PC','AC', 'MB', 'FB', 'MK', 'HC', 'PSG','CB','WP','KC','PP'))
P1314$Site_Orient = factor(P1314$Site,levels=c("AP", "HC", "TC", "WP", "AC", "KC", "MB", "FB", "CB", "MK","PSG", "PP", "PC"))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Orient~Year, scale="free")
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Orient~Year)



P1314$Site_regslope = factor(P1314$Site, levels=c('AP','PC','AC','TC','FB', 'MB', 'MK', 'CB', 'HC', 'PSG','WP','KC','PP'))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_regslope~Year, scale="free")
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_regslope~Year)

P1314$Site_Lat = factor(P1314$Site, levels=c('MB','KC','PC','AP','TC','WP', 'AC', 'FB', 'MK', 'HC','PP', 'PSG','CB'))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Lat~Year, scale="free")
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Lat~Year)

P1314$Site_Slope = factor(P1314$Site, levels=c('AP','TC','PC','AC', 'MB', 'FB', 'MK', 'HC', 'PSG','WP','KC','PP', 'CB'))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Slope~Year, scale="free")
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Slope~Year)


P1314$Site_Slopeavg = factor(P1314$Site, levels=c('PC','AP','TC','PP','AC', 'MB', 'FB', 'PSG', 'HC', 'CB','KC','WP','MK'))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Slopeavg~Year, scale="free")
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Slopeavg~Year)

P1314$Site_Grouped = factor(P1314$Site, levels=c('PC','AC','TC','CB','FB','MB', 'MK', 'AP', 'PSG','KC','WP','HC','PP'))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Grouped~Year, scale="free")
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Grouped~Year)

###SEDIMENT
melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(33:40), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314sizecast=dcast(P1314, Site+Year~variable, value.var="mean")
P1314merge=merge(P1314sizecast,SiteInfo, by=c("Site","Year"))

P1314$Site_Slope = factor(P1314$Site, levels=c('AP','TC','PC','AC', 'MB', 'FB', 'MK', 'HC', 'PSG','CB','WP','KC','PP'))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Slope~Year)

###SLOPES*****
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/Site level information")
Slope1=read.csv("Slope20132014.csv", header=TRUE)
Slope1$Site_Slope = factor(Slope1$Site, levels=c('PC','AP','TC','PP','AC', 'MB', 'FB', 'PSG', 'HC', 'CB','KC','WP','MK'))
melted <- melt(Slope1, id.vars=c("Site_Slope"), measure.vars = c("Slope"), na.rm=T)
Slopedata=ddply(melted, c("variable", "Site_Slope"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = Slopedata, aes(x = Site_Slope, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2)

boxplot(Slope~Site, data=Slope1)

###
melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(3,6), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314sizecast=dcast(P1314, Site+Year~variable, value.var="mean")
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site_Slope~Year, scale="free")


#######
z = cor(P1314merge[,c(-21,-37,-38,-39,-40,-41,-42,-47, -43,-44,-45,-46,-9,-1)])
zdf=as.data.frame(z)
ggpairs(P1314merge[,c(42,50,26:33,39:49,64,65)])
colnames(P1314merge)
ggplot(data = P1314merge, aes(x = Heading.N, y = Ovigerous.size))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = P1314merge, aes(x = Slope.min, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")


#SLOPE MIN AND COASTLINE REG 0.5km have interesting relationship...
ggplot(data = P1314merge, aes(x = Slope.min, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = P1314merge, aes(x = Slope.average, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
summary(lme(recruits+s4to6mm+s7to9mm+s10to12mm+s13to15mm+s16plusmm~Slope.average, random=~1|Year, data=P1314merge))

##SLOPE 9<
sub=subset(P1314merge,Site=="HC" |Site=="PSG" |Site=="WP" |Site=="KC" | Site=="PP")
#ggpairs(sub[,c(42,50,26:33,39:49,64,65)])
ggplot(data = sub, aes(x = Slope.min, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub, aes(x = Slope.min, y = Ovigerous.size, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub, aes(x = Slope.min, y = Heading, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub, aes(x = Size.2, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")

sub2=subset(P1314merge,Site=="AC" |Site=="AP" |Site=="PC" |Site=="TC" | Site=="MK"| Site=="CB"| Site=="MB"| Site=="FB")
#ggpairs(sub2[,c(42,50,26:33,39:49,64,65)])
ggplot(data = sub2, aes(x = Slope.min, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub2, aes(x = Slope.min, y = Ovigerous.size, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub2, aes(x = Slope.min, y = Heading, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub2, aes(x = Size.2, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")

###OTHER GROUPINGS FOR slope and coastline
##SLOPE <10 and <375 degrees
sub=subset(P1314merge,Site=="AP" |Site=="AC" |Site=="TC" |Site=="FB" | Site=="MB")
#ggpairs(sub[,c(42,50,26:33,39:49,64,65)])
ggplot(data = sub, aes(x = Slope.min, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")

##SLOPE 10-20 and <350 degrees
sub=subset(P1314merge,Site=="PSG" |Site=="WP" |Site=="KC" |Site=="HC")
#ggpairs(sub[,c(42,50,26:33,39:49,64,65)])
ggplot(data = sub, aes(x = Slope.min, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub, aes(x = Slope.average, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")

##SLOPE 10-20 and <350 degrees
sub=subset(P1314merge,Site=="CB" |Site=="MK" |Site=="PP")
ggplot(data = sub, aes(x = Slope.min, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")
ggplot(data = sub, aes(x = Slope.average, y = Coastline.reg.0.5.km, color=Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")

###GROUP BY COASTLINE AND SLOPE
P14merge=subset(PopSite1314AVG, Year=="2014")
numbers_only <- P14merge[,c(64,67)]
rownames(numbers_only) <- P14merge[,1]
d   <- dist(numbers_only, method="euclidean")
fit <- hclust(d, method="ward")
plot(fit)

P14merge=subset(PopSite1314AVG, Year=="2014")
numbers_only <- P14merge[,c(12:14)]
rownames(numbers_only) <- P14merge[,1]
d   <- dist(numbers_only, method="euclidean")
fit <- hclust(d, method="ward")
plot(fit)

P14merge=subset(P1314merge, Year=="2014")
numbers_only <- P14merge[,c(12:31)]
rownames(numbers_only) <- P14merge[,1]
d   <- dist(numbers_only, method="euclidean")
fit <- hclust(d, method="ward")
plot(fit)

P14merge=subset(PopSite1314AVG, Year=="2014")
numbers_only <- P14merge[,c(48,64,95)]
rownames(numbers_only) <- P14merge[,1]
d   <- dist(numbers_only, method="euclidean")
fit <- hclust(d, method="ward")
plot(fit)


###TEST REPRO with slope min stuff
melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(3,6), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314sizecast=dcast(P1314, Site+Year~variable, value.var="mean")
P1314merge=merge(P1314sizecast,SiteInfo, by=c("Site","Year"))

ggpairs(P1314merge[,c(3,4,24)])
sub=subset(P1314merge,Site=="HC" |Site=="PSG" |Site=="WP" |Site=="KC" | Site=="PP" | Site=="CB")
ggpairs(sub[,c(3,4,24)])
sub2=subset(P1314merge,Site=="AC" |Site=="AP" |Site=="PC" |Site=="TC" | Site=="MK"|  Site=="MB"| Site=="FB")
ggpairs(sub2[,c(3,4,24)])
ggplot(data = P1314merge, aes(y = recruits, x = Site))+geom_point(stat = "identity") + facet_grid(.~Year, scale="free")

###TOTAL REPRO AND REC
PopSite1314AVG=merge(P1314cast,SiteInfo, by=c("Site","Year"))
PopSite1314AVG$TotalRepros=(PopSite1314AVG$ReproDensity*4)*PopSite1314AVG$Size.1
PopSite1314AVG$TotalEmbryos=(PopSite1314AVG$TotalRepros*PopSite1314AVG$Embryos.per.ovig)
PopSite1314AVG$TotalRec=(PopSite1314AVG$s1to3mm*4)*PopSite1314AVG$Size.1


ggplot(data = PopSite1314AVG, aes(x = log(TotalEmbryos), y = log(TotalRec), color=Site))+geom_point()+ facet_grid(Year~.)
Rel<-lme(log(TotalRec) ~ log(TotalEmbryos), data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)

Rel<-lm(log(TotalRec) ~ log(TotalEmbryos), data = subset(PopSite1314AVG, Year=="2013"))
summary(Rel)
Rel

Rel<-lm(log(TotalRec) ~ log(TotalEmbryos), data = subset(PopSite1314AVG, Year=="2014" | Site == "AC"| Site == "AP"| Site == "FB"| Site == "TC"))
summary(Rel)
Rel

Rel<-lm(log(TotalEmbryos) ~ log(TotalRepros), data = subset(PopSite1314AVG, Year=="2014" | Site == "AC"| Site == "AP"| Site == "FB"| Site == "TC"))
summary(Rel)
Rel



PopSite1314AVG$Avggrainsize=((PopSite1314AVG$X1m2.50cm2*75)+(PopSite1314AVG$X50cm2.10cm2*30)+(PopSite1314AVG$X.10cm2..course.sand*4)+(PopSite1314AVG$bare.cobble...2cm.*2)+(PopSite1314AVG$bare.sand..fine.very.course.sand.*1))/100
sub14=subset(PopSite1314AVG, Year=="2014")
ggpairs(sub14[,c(93,98,48:53,64, 91:97,99:100)])
network_plot(correlate(sub14[,c(67,72,3,6,64,95,48)]), min_cor=0.4)

ggplot(data = PopSite1314AVG, aes(x = Slope.min, y = Avggrainsize, color=Site))+geom_point()+ facet_grid(Year~.)
Rel<-lme(Avggrainsize ~ Slope.min, data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)

ggplot(data = PopSite1314AVG, aes(x = Surfzone.size..m., y = Slope.min, color=Site, size=Avggrainsize))+geom_point()+ facet_grid(Year~.)
Rel<-lme(Avggrainsize ~ Surfzone.size..m. * Slope.min, data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)

ggplot(data = PopSite1314AVG, aes(x = Surfzone.size..m., y = Wavetime, color=Site, size=Avggrainsize))+geom_point()+ facet_grid(Year~.)
Rel<-lme(Avggrainsize ~ Surfzone.size..m. * Wavetime, data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)

ggplot(data = PopSite1314AVG, aes(x = Wavetime, y = Avggrainsize, color=Site))+geom_point()+ facet_grid(Year~.)
Rel<-lme(Avggrainsize ~ Slope.min, data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)

ggplot(data = PopSite1314AVG, aes(x = Size.1, y = Avggrainsize, color=Site))+geom_point()+ facet_grid(Year~.)
Rel<-lme(Avggrainsize ~ Size.1 , data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = Ovigerous.size, color=Site))+geom_point()+ facet_grid(Year~.)
Rel<-lme(Ovigerous.size ~ Avggrainsize , data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)

ggplot(data = PopSite1314AVG, aes(x = Slope.min, y = Avggrainsize, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Coastline.reg.0.5.km, y = Avggrainsize, color=Site ))+geom_point()+ facet_grid(Year~.)



ggplot(data = PopSite1314AVG, aes(x = TotalRepro, y = TotalRec, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = QualityHabitat, y = ReproDensity, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Slope.min , y = QualityHabitat, color=Site))+geom_point()+ facet_grid(Year~.)
lm(QualityHabitat~Slope.min, data=PopSite1314AVG)

ggplot(data = PopSite1314AVG, aes(x = Slope.min , y = recruits , color=Site))+geom_point()+ facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = QualityHabitat, y = Surfzone.size..m., color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Slope.min, y = recruits , color=Site))+geom_point()+ facet_grid(Year~.)

Rel<-lme(recruits ~ Coastline.reg.0.5.km, data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)
ggplot(data = PopSite1314AVG, aes(x = Coastline.reg.0.5.km, y = recruits , color=Site))+geom_point()+ facet_grid(Year~.)

Rel<-lme(Slope.min ~ Coastline.reg.0.5.km , data = PopSite1314AVG, random=~1|Year, na.action=na.omit)
summary(Rel)
ggplot(data = PopSite1314AVG, aes(x = Coastline.reg.0.5.km, y = Slope.min , color=Site))+geom_point()+ facet_grid(Year~.)




subgreater9=subset(PopSite1314AVG,Site=="HC" |Site=="PSG" |Site=="WP" |Site=="KC" | Site=="PP"| Site=="CB")
ggpairs(subgreater9[,c(3,4,6,13:16,24,26,28:33)])
ggplot(data = subgreater9, aes(x = Heading, y = Size.of.opening, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = subgreater9, aes(x = QualityHabitat, y = ReproDensity, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = subgreater9, aes(x = Heading.N, y = QualityHabitat, color=GeoType))+geom_point()+ facet_grid(Year~.)
ggplot(data = subgreater9, aes(x = Slope.min, y = Size.1 , color=Site))+geom_point()+ facet_grid(Year~.)



subless9=subset(PopSite1314AVG,Site=="AP" |Site=="AC" |Site=="TC" |Site=="FB" | Site=="MB"|Site=="MK" | Site=="PC")
ggpairs(subless9[,c(3,4,6,13:16,24,26,28:33)])
ggplot(data = subless9, aes(x = Heading, y = Size.of.opening, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = subless9, aes(x = QualityHabitat, y = ReproDensity, color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = subless9, aes(x = Heading.N, y = QualityHabitat, color=GeoType))+geom_point()+ facet_grid(Year~.)
ggplot(data = subless9, aes(x = ReproDensity, y = recruits , color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = subless9, aes(x = Slope.min, y = Size.1 , color=Site))+geom_point()+ facet_grid(Year~.)

ggplot(data = subset(PopSite1314AVG,Site=="HC" |Site=="PSG" |Site=="WP" |Site=="KC" | Site=="PP"| Site=="CB"), aes(x = TotalRepro, y = TotalRec, color=Site))+geom_point()+ facet_grid(Year~.)
Rel<-lm(TotalRec ~ TotalRepro, data = subset(PopSite1314AVG,Site=="HC" |Site=="PSG" |Site=="WP" |Site=="KC" | Site=="PP"| Site=="CB"), na.action=na.omit)
summary(Rel)

ggplot(data = subset(PopSite1314AVG,Site=="AP" |Site=="AC" |Site=="TC" |Site=="FB" | Site=="MB"|Site=="MK" | Site=="PC"), aes(x = TotalRepro, y = log(TotalRec), color=Site))+geom_point()+ facet_grid(Year~., scale="free")
Rel<-lme(log(TotalRec) ~ TotalRepro, data = subset(PopSite1314AVG,Site=="AP" |Site=="AC" | Site=="TC" |Site=="FB" | Site=="MB"|Site=="CB" |Site=="MK" |Site=="PP"| Site=="PC"), random=~1|Year, na.action=na.omit)
summary(Rel)




#Test 2013/2014 simultaneously
Rel<-lme(TotalRec ~ TotalRepro, random=~1|Year, data = PopSite1314AVG, na.action=na.omit)
summary(Rel)

#Test 2013/2014 simultaneously just sites samples regularly
Rel<-lme(TotalRec ~ TotalRepro, random=~1|Year, data = subset(PopSite1314AVG, Site=="AC" | Site=="AP" | Site=="FB" | Site=="TC"| Site=="PC") , na.action=na.omit)
summary(Rel)
ggplot(data = subset(PopSite1314AVG, Site=="AC" | Site=="AP" | Site=="FB" | Site=="TC"| Site=="PC"), aes(x = TotalRepro, y = TotalRec, color=Site))+geom_point()+ facet_grid(Year~.)

#test relationship between reproduction and recruitment for 2013
Rel13<-lm(TotalRec ~ TotalRepro, data = subset(PopSite1314AVG, Year=="2013"), na.action=na.omit)
summary(Rel13)

#test relationship between reproduction and recruitment for 2014
Rel14<-lm(TotalRec ~ TotalRepro, data = subset(PopSite1314AVG, Year=="2014"), na.action=na.omit)
summary(Rel14)

#test relationship between reproduction and recruitment for 2014 only 4 sites
Rel14<-lm(TotalRec ~ TotalRepro, data = subset(PopSite1314AVG, Site=="AC" | Site=="AP" | Site=="FB" | Site=="TC"), na.action=na.omit)
summary(Rel14)

###NMDS
Pop1314na=na.omit(Pop1314[,c("Site","Year","X.1m2", "X1m2.50cm2", "X50cm2.10cm2", "X.10cm2..course.sand", "cemented.rock.1m2.10cm2", "bare.cobble...2cm.", "bare.sand..fine.very.course.sand.", "rocky.bench")])
fit <- kmeans(Pop1314na[,c("X.1m2", "X1m2.50cm2", "X50cm2.10cm2", "X.10cm2..course.sand", "cemented.rock.1m2.10cm2", "bare.cobble...2cm.", "bare.sand..fine.very.course.sand.", "rocky.bench")])
NMDS1314=metaMDS(Pop1314na[,c("X.1m2", "X1m2.50cm2", "X50cm2.10cm2", "X.10cm2..course.sand", "cemented.rock.1m2.10cm2", "bare.cobble...2cm.", "bare.sand..fine.very.course.sand.", "rocky.bench")], trymax=1999, k=3, dist="bray")

NMDS1314

#Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(NMDS1314)

#plot NMDS- 1 Way
NMDS1 = NMDS1314$points[,1]
NMDS2 = NMDS1314$points[,2]
NMDS = data.frame(NMDS1 = NMDS1, NMDS2 = NMDS2, Site = Pop1314na$Site)

NMDSplot=ggplot(NMDS, aes(x=NMDS1, y=NMDS2, col=Pop1314na$Site)) + geom_point(size=2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.2),panel.border = element_rect(colour = "black", fill=NA, size=1))+annotate("text", label = "Stress: 0.12", x = 1.5, y = -1.25, size = 3.5, colour = "black")+ggtitle("August")
NMDSplot

###NMDS SIZE STRUCT
Pop1314na=na.omit(Pop1314[,c("Site","Year","recruits","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm")])
Pop1314na$total=rowSums(Pop1314na[,c("recruits","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm")])
Pop1314na0 = Pop1314na[rowSums(Pop1314na[,c("recruits","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm")])!=0, ] 

NMDS1314=metaMDS(Pop1314na0[,c("recruits","s4to6mm","s7to9mm","s10to12mm", "s13to15mm", "s16plusmm")], trymax=1999, k=2, dist="bray")
NMDS1314

#Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(NMDS1314)

#plot NMDS- 1 Way
NMDS1 = NMDS1314$points[,1]
NMDS2 = NMDS1314$points[,2]
NMDS = data.frame(NMDS1 = NMDS1, NMDS2 = NMDS2, Site = Pop1314na0$Site)

NMDSplot=ggplot(NMDS, aes(x=NMDS1, y=NMDS2, col=Pop1314na0$Site)) + geom_point(size=2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.2),panel.border = element_rect(colour = "black", fill=NA, size=1))+annotate("text", label = "Stress: 0.08", x = 1.5, y = -1.25, size = 3.5, colour = "black")+ggtitle("")
NMDSplot

###PLOTS
melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(3:26), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314sizecast=dcast(P1314, Site+Year~variable, value.var="mean")
P1314AVGmerge=merge(P1314sizecast,SiteInfo, by=c("Site","Year"))

melted <- melt(PopSite1314, id.vars=c("Site", "Year"), measure.vars = c(3,4,6), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = P1314, aes(x = Site, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(variable~Year, scale="free")

ggplot(data = P1314, aes(x = QualityHabitat, y = Slope.min, color=Site))+geom_point()+ facet_grid(Year~.)


melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(3:26), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
P1314sizecast=dcast(P1314, Site+Year~variable, value.var="mean")
P1314AVGmerge=merge(P1314sizecast,SiteInfo, by=c("Site","Year"))

ggplot(data = Pop1314, aes(x = QualityHabitat, y = Slope.min, color=Site))+geom_point()+ facet_grid(Year~.)

###SIZES OF ROCKS
melted <- melt(PopSite1314, id.vars=c("Site", "Year"), measure.vars = c(28:36), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = P1314, aes(x = variable, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site~Year)




ggplot(data = PopSite1314AVG, aes(x = Ovigerous.size, y = log(TotalEmbryos), color=Site))+geom_point()+ facet_grid(Year~.)

##Total Embryos per 1mm
#OvigSlope <-lme(OvigSlope ~ Surfzone.size..m., random=~1|Year, data = PopSite1314AVG)
#summary(Rec)

##Density of Reproductive individuals
#IndivRepro <-lme(AllReproAge ~ Avggrainsize + s1to3mm + Size.1, random=~1|Year, data = PopSite1314AVG)
#summary(Rec)

##Grain size
#Grain <-lme(Avggrainsize ~ Slope.min * Surfzone.size..m. , random=~1|Year, data = PopSite1314AVG)
#summary(Grain)






ggplot(data = PopSite1314AVG, aes(x = log(TotalRepros), y = log(TotalEmbryos), color=Site))+geom_point()+ facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Ovigerous.size, y = log(TotalEmbryos), color=Site))+geom_point()+ facet_grid(Year~.)

##IMPORTANT
ggplot(data = PopSite1314AVG, aes(x = Size.1, y = Difference.from.300.Northery.heading , color=Site))+geom_point()+ facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = Slope.min, y = Coastline.reg.0.5.km , color=Site))+geom_point()+ facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = Slope.min, y = Avggrainsize , color=Site ,size= Difference.from.300.Northery.heading))+geom_point()+ facet_grid(Year~.)




sub14=subset(PopSite1314AVG, Year=="2014")
network_plot(correlate(sub14[,c(71,61,62,50,48,64,93,67, 97:101, 3, 6)]), min_cor=0.4)





###OVIG SIZE DIST
#set working directory
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2014 FINAL DATA")

#Read in 2013 quadrat data
Ovig14=read.csv("2014OvigInfo.csv", header=TRUE)
Ovig14DF=data.frame(Ovig14)
attach(Ovig14DF)
Ovigsub=subset(Ovig14DF, Ovigerous..Y.N. == "Y")
attach(Ovigsub)
Ovigminimal=Ovigsub[,c(1,6)]

ggplot(Ovigminimal, aes(x=Size..mm.)) + 
  geom_histogram(binwidth=1)+facet_grid(Location~., scale="free")

