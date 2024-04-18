###SITE LEVEL INFORMATION 2013, 2014, 2015 Population Data

###READ IN ALL DATA- GO TO FILE FINAL DATA READ IN

###AVERAGE QUADRAT LEVEL DATA TO INTEGRATE WITH SITE LEVEL- SPATIAL
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

#only columns I want to work with
mergefinal=mergefinal2013[,c(1,164,54,43:49,24:35,158:163,115,117:118,154:157,248:253,186:188,254:261,269:273,266,284:286, 165:171,62:64)]
network_plot(correlate(mergefinal[,c(2:51,53:70)]), min_cor=0.96)


#Interesting relationships 2013
ggplot(data = mergefinal, aes(x = Difference.from.300..NW., y = Gobiesox.maeandricus.ALL, color=Site))+geom_point()

ggplot(data = mergefinal, aes(x = Gobiesox.maeandricus.ALL, y = X12, color=Site))+geom_point()

##MERGE 2013 2014 WITH SITE INFO
melted <- melt(Pop1314, id.vars=c("Site", "Year"), measure.vars = c(6:90), na.rm=T)
all1314quad=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
all1314quad

all1314quadcast13=cast(all1314quad, Year+Site~variable, value="mean")

mergesite1314=merge(all1314quadcast13,SiteInfo)
mergesite2013=subset(mergesite1314, Year=="2013")
mergesite2014=subset(mergesite1314, Year=="2014")
mergesite20144sites=subset(mergesite1314, Year=="2014", Site=="AC"| Site=="AP" | Site=="FB" | Site=="TC")




network_plot(correlate(mergesite2013[,c(60:65,29:39,68:74,75:77,81,43:46,48:54,98:100,101,104,115:117)]), min_cor=0.9)
network_plot(correlate(mergesite2014[,c(60:65,29:39,68:74,75:77,81,43:46,49:54,98:100,101,104,115:117)]), min_cor=0.75)
corrplot(cor(mergesite2013[,c(82,87,46,49:54,92,96:103)]), type="upper")
corrplot(cor(mergesite2014[,c(82,87,46,49:54,92,96:103)]), type="upper")

submergesite=mergesite1314[,c(1,2,60:80,85,88:92,94,43:46,49:54,103:105)]

submergesite2013=subset(submergesite, Year=="2013")
submergesite2014=subset(submergesite, Year=="2014")

network_plot(correlate(submergesite[,c(3:43)]), min_cor=0.7)
network_plot(correlate(submergesite2013[,c(3:43)]), min_cor=0.86)
network_plot(correlate(submergesite2014[,c(9:43)]), min_cor=0.6)

corrplot(cor(submergesite2013[,c(24:30,34,35:43,19:20)]), type="upper")
corrplot(cor(submergesite2014[,c(24:30,34,35:43,19:20)]), type="upper")
network_plot(correlate(submergesite2013[,c(24:30,34,35:43,19:20)]), min_cor=0.8)
network_plot(correlate(submergesite2014[,c(24:30,34,35:43,19:20)]), min_cor=0.4)

corrplot(cor(submergesite2013[,c(35:40)]), type="upper")
corrplot(cor(submergesite2014[,c(35:40)]), type="upper")


#for just 4 sites with pigs/clear
submergesite42014=na.omit(mergesite1314[,c(1,2,3:6,85,88:92,94)])
submergesite42014$cleartopig=rowSums(submergesite42014[,3:6])
network_plot(correlate(submergesite42014[,c(7:14)]), min_cor=0.9)
=ggplot(data = submergesite42014, aes(x = cleartopig, y = Heading, color=Site))+geom_point()

##MOST IMPORTANT
ggplot(data = mergesite1314, aes(x = Size.2, y = s1to3mmcombined, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = submergesite, aes(x = Size.2, y = Ovigerous, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = submergesite, aes(x = Size.2, y = Females.All, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = submergesite, aes(x = Size.2, y = Males.All, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = mergesite1314, aes(x = Total.degrees.opening, y = X50cm2.10cm2, color=Site))+geom_point()+facet_grid(Year~Exposed.to.wNW.Swell..300.)

ggplot(data = submergesite, aes(x = Difference.from.300..NW., y = Ovigerous, color=Site, size=s1to3mm))+geom_point()+facet_grid(Year~.)
ggplot(data = submergesite, aes(x = Difference.from.300..NW., y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = submergesite, aes(x = Heading, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = submergesite, aes(x = Heading, y = Ovigerous, color=Site))+geom_point()+facet_grid(Year~.)


##OTHER PLOTS
ggplot(data = quadratsitemerge1314, aes(x = Total.degrees.opening, y = Totalliveable, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(Totalliveable ~ Total.degrees.opening + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(Totalliveable ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

#ARE SITES DIFFERENCE IN OVIGEROUS
ggplot(data = quadratsitemerge1314, aes(x = Site, y = Ovigerous))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(Totalliveable ~ Site + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(Totalliveable ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

##OVIGEROUS AND 50cm
ggplot(data = quadratsitemerge1314, aes(x = X50cm2.10cm2, y = OvigeroustoFemale, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(Ovigerous ~ X50cm2.10cm2 + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(Ovigerous ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

###50cm and HEADING
ggplot(data = mergesite1314, aes(x = Total.degrees.opening, y = rocky.bench, color=Site, size=Slope.average))+geom_point()+facet_grid(Year~.)

mixed.lmer <- lmer(X50cm2.10cm2 ~ Heading + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(X50cm2.10cm2 ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

corrplot(cor(mergesite1314[,c(46,48:54,76,77,86,81,89:93,95)]), type="upper")

##HEADING 1-3
ggplot(data = quadratsitemerge1314, aes(x = Heading, y = s1to3mmcombined, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(s1to3mmcombined ~ Heading + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(s1to3mmcombined ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

#size and recruits
ggplot(data = quadratsitemerge1314, aes(x = Size.2, y = s1to3mmcombined, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(s1to3mmcombined ~ Size.2 + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(s1to3mmcombined ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

###HEADING 50-10cm
ggplot(data = quadratsitemerge1314, aes(x = Heading, y = X50cm2.10cm2, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(X50cm2.10cm2+X.10cm2..course.sand+cemented.rock.1m2.10cm2+bare.cobble...2cm.+bare.sand..fine.very.course.sand.+rocky.bench ~ Heading + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(X50cm2.10cm2+X.10cm2..course.sand+cemented.rock.1m2.10cm2+bare.cobble...2cm.+bare.sand..fine.very.course.sand.+rocky.bench ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

###50-10cm and Ovigerous
mixed.lmer <- lmer(X50cm2.10cm2 ~ Size.2 + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(X50cm2.10cm2 ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

###heading and opening
mixed.lmer <- lmer(X50cm2.10cm2 ~ Heading*Total.degrees.opening + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(X50cm2.10cm2 ~ (1|Year), data = quadratsitemerge1314, REML=F)

anova(mixed.lmer,mixed.null)

###Ovigerous Ideal Liveable
ggplot(data = quadratsitemerge1314, aes(x = IdealLiveableRocks, y = Ovigerous, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(Ovigerous ~ IdealLiveableRocks + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(Ovigerous ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

###Heading Ideal Liveable
ggplot(data = quadratsitemerge1314, aes(x = Heading, y = IdealLiveableRocks, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(IdealLiveableRocks ~ Heading + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(IdealLiveableRocks ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)

head=ggplot(data = mergesite1314, aes(x = Heading.N, y = Heading.S, color=Rocky.bench.v..cemented, size=Total.degrees.opening))+geom_point()+facet_grid(Year~.)
rec=ggplot(data = mergesite1314, aes(x = Heading, y = s1to3mmcombined, color=Site))+geom_point()+facet_grid(Year~.)
grid.arrange(head,rec)


###PCA of SITE VARIABLES
##PCA
envir=mergesite2014[,c(98:100,97,106,115:117,101,93)]
cor(envir)
pcapop <- prcomp(envir,center = TRUE, scale. = TRUE) 
pcapop
summary(pcapop)
biplot(pcapop)
autoplot(prcomp(envir), data = mergesite2014, colour = 'Site',   loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)
mergesite2014$PC1<-pcapop$x[,1]
mergesite2014$PC2<-pcapop$x[,2]
mergesite2014$PC3<-pcapop$x[,3]
colnames(mergesite2014)

corrplot(cor(mergesite2014[,c(119:121,68,87,46,98:100,97,106,115:117,101,93)]), type="upper")
network_plot(correlate(mergesite2014[,c(120:122,68,87,46,98:100,97,106,115:117,101,93,102)]), min_cor=0.5)

ggplot(data = mergesite1314, aes(x = Heading, y = s1to3mmcombined, color=Site, size=Slope.Max))+geom_point()+facet_grid(Year~., scale="free")
ggplot(data = mergesite1314, aes(x = Heading.N, y = Total.degrees.opening, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = mergesite1314, aes(x = Heading.N, y = Total.degrees.opening, color=Sites.north.and.south))+geom_point()+facet_grid(Year~.)
ggplot(data = mergesite1314, aes(x = Heading.N, y = Heading.S, color=Site))+geom_point()+facet_grid(Year~.)

##OVIGEROUS AND SIZE
ggplot(data = mergesite1314, aes(x = Size.2, y = Ovigerous, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = quadratsitemerge1314, aes(x = Size.2, y = Ovigerous, color=Site))+geom_point()+facet_grid(Year~.)
mixed.lmer <- lmer(Ovigerous ~ Size.2 + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(Ovigerous ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)


ggplot(data = mergefinal2013, aes(x = wavetime.sec, y = PLcrabs.y, color=Site))+geom_point()
ggplot(data = mergefinal2013, aes(x = wavetime.sec, y = s1to3mm, color=Site))+geom_point()
wt=ggplot(data = wavechla13, aes(x = Date, y = wavetime.sec, color=Site))+geom_point()
rec=ggplot(data = Pop13DF, aes(x = Date, y = recruits3mm, color=Site))+geom_point()
grid.arrange(wt,rec, wavedir)

ggplot(data = mergesite2013, aes(x = X50cm2.10cm2, y = ALL, color=Site, size=Size.2))+geom_point()

ggplot(data = mergesite2014, aes(x = Slope.min, y = Slope.Max, color=Site,size=Size.2))+geom_point()

ggplot(data = mergesite2013, aes(x = recruits3mm, y = Ovigerous, color=Site))+geom_point()

ggplot(data = mergesite2014, aes(x = s1to3mmcombined, y = Ovigerous, color=Site))+geom_point()

ggplot(data = subset(mergesite1314, bare.sand..fine.very.course.sand. < 10), aes(x = bare.sand..fine.very.course.sand., y = recruits3mm, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = mergesite1314, aes(x = ALL, y = bare.cobble...2cm., color=Site, size=ALL))+geom_point()+facet_grid(Year~.)



mixed.lmer <- lmer(s1to3mmcombined ~ Heading + (1|Year), data = quadratsitemerge1314, REML=F)
mixed.null <- lmer(s1to3mmcombined ~ (1|Year), data = quadratsitemerge1314, REML=F)
anova(mixed.lmer,mixed.null)


####CORRELATIONS
###2014 4 Sites
merge4sites2014=subset(Pop1314, Year=="2014")
merge4sites2014a=subset(merge4sites2014, Site=="AC"| Site=="AP" | Site=="FB" | Site=="TC")
merge20144sites <- melt(merge4sites2014a, id.vars=c("Site"), measure.vars = c(6:96), na.rm=T)
merge4sites2014=ddply(merge20144sites, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
merge4sites2014

foursites2014cast=cast(merge4sites2014, Site~variable, value="mean")
foursites2014=merge(foursites2014cast,SiteInfo)

network_plot(correlate(foursites2014[,c(26:29,32:38,50:55,56:58,63:66,68:74,80,86:89,94,104:106)]), min_cor=0.9)

z=cor(foursites2014[,c(26:29,32:38,50:55,56:58,63:66,68:74,80,86:89,94,104:106)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .7)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.7)
write.csv(zdf1, file = "Foursites2014.csv")


####2014 ALL SITES WEEK 0
mergeALLsites2014=subset(Pop1314, Year=="2014")
mergeALLsites2014a=subset(mergeALLsites2014, Week=="Week 0")
merge2014ALLsites <- melt(mergeALLsites2014a, id.vars=c("Site"), measure.vars = c(6:96), na.rm=T)
mergeALLsites2014=ddply(merge2014ALLsites, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
mergeALLsites2014

allsites2014cast=cast(mergeALLsites2014, Site~variable, value="mean")
allsites2014=merge(allsites2014cast,SiteInfo)


network_plot(correlate(allsites2014[,c(22,28:34,76,81:85,90,100:102)]), min_cor=0.6)
#subsetall2014=subset(allsites2014, Ovigerous < 3)
#network_plot(correlate(subsetall2014[,c(22:25,28:33,45:54,68,72:76,90:92)]), min_cor=0.6)

z=cor(allsites2014[,c(22:25,28:33,45:54,68,72:76,90:92)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .5)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.5)
write.csv(zdf1, file = "ALLsites2014.csv")


####2013 ALL 5 SITES 
mergeALLsites2013=subset(Pop1314, Year=="2013")
merge2013ALLsites <- melt(mergeALLsites2013, id.vars=c("Site"), measure.vars = c(6:88), na.rm=T)
mergeALLsites2013=ddply(merge2013ALLsites, c("variable", "Site"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
mergeALLsites2013

allsites2013cast=cast(mergeALLsites2013, Site~variable, value="mean")
allsites2013=merge(allsites2013cast,SiteInfo)


network_plot(correlate(allsites2013[,c(38:41,43:49,54:59,62:71,77:79,85,89:92,93,98,107:109)]), min_cor=0.7)

z=cor(allsites2013[,c(38:41,43:49,54:59,62:71,77:79,85,89:92,93,98,107:109)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .7)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.7)
write.csv(zdf1, file = "ALLsites2013.csv")

##IMPORTANT FACTORS
#rocky bench// total degree opening
ggplot(data = subset(allsites2013), aes(x = Total.degrees.opening, y = rocky.bench, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Total.degrees.opening, y = rocky.bench, color=Site))+geom_point()

#Total.degrees.opening// Size.2
ggplot(data = subset(allsites2013), aes(x = Total.degrees.opening, y = Size.2, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Total.degrees.opening, y = Size.2, color=Site))+geom_point()

#Size.2//Ovigerous
ggplot(data = subset(allsites2013), aes(x = Size.2, y = Ovigerous, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Size.2, y = Ovigerous, color=Site))+geom_point()

#recruits3mm//Ovigerous
ggplot(data = subset(allsites2013), aes(x = recruits3mm, y = Ovigerous, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = recruits3mm, y = Ovigerous, color=Site))+geom_point()

#rocky bench//Ovigerous
ggplot(data = subset(allsites2013), aes(x = rocky.bench, y = Ovigerous, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = rocky.bench, y = Ovigerous, color=Site))+geom_point()

#rocky bench//Ovigerous
ggplot(data = subset(allsites2013), aes(x = X50cm2.10cm2, y = Ovigerous, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = X50cm2.10cm2, y = Ovigerous, color=Site))+geom_point()

#Size.2//s1113mm
ggplot(data = subset(allsites2013), aes(x = Size.2, y = s1113mm, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Size.2, y = s1113mm, color=Site))+geom_point()

#Size.2//MAles
ggplot(data = subset(allsites2013), aes(x = Size.2, y = Males.All, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Size.2, y = Males.All, color=Site))+geom_point()

#Size.2//Females.ALL
ggplot(data = subset(allsites2013), aes(x = Size.2, y = Size.of.opening, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Size.2, y = Size.of.opening, color=Site))+geom_point()

#Size.2//X50cm10cm
ggplot(data = subset(allsites2013), aes(x = Size.2, y = X50cm2.10cm2, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Size.2, y = X50cm2.10cm2, color=Site))+geom_point()

#Heading S//No sex all
ggplot(data = subset(allsites2013), aes(x = Heading.S, y = No.Sex.All, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Heading.S, y = No.Sex.All, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Heading.S, y = No.Sex.All, color=Site))+geom_point()

#Heading//3-5mm
ggplot(data = subset(allsites2013), aes(x = Heading, y = s35mm, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Heading, y = s35mm, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Heading, y = s35mm, color=Site))+geom_point()


#HeadingS//ALL
ggplot(data = subset(allsites2013), aes(x = Heading.S, y = ALL, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Heading.S, y = ALL, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Heading.S, y = ALL, color=Site))+geom_point()


#MALes ALL //SLOPE AVERAGE
ggplot(data = subset(allsites2013), aes(x = Slope.average, y = Males.All, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Slope.average, y = Males.All, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Slope.average, y = Males.All, color=Site))+geom_point()

#810 //SLOPE AVERAGE
ggplot(data = subset(allsites2013), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()

#Ovigerous //NonLiveable
ggplot(data = subset(allsites2013), aes(x = NonLiveableRocks, y = Ovigerous, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = NonLiveableRocks, y = Ovigerous, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = NonLiveableRocks, y = Ovigerous, color=Site))+geom_point()

#Slope.average //1720
ggplot(data = subset(allsites2013), aes(x = Slope.average, y = s1720mm, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Slope.average, y = s1720mm, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Slope.average, y = s1720mm, color=Site))+geom_point()

#Slope.average //810
ggplot(data = subset(allsites2013), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()

#Slope.average //810
ggplot(data = subset(allsites2013), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Slope.average, y = s810mm, color=Site))+geom_point()

#Females.All //Slope.Max
ggplot(data = subset(allsites2013), aes(x = Females.All, y = Slope.Max, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Females.All, y = Slope.Max, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Females.All, y = Slope.Max, color=Site))+geom_point()


#Females.All //Slope.Max
ggplot(data = subset(allsites2013), aes(x = Females.All, y = Gravel, color=Site))+geom_point()
ggplot(data = subset(allsites2014), aes(x = Females.All, y = Gravel, color=Site))+geom_point()
ggplot(data = subset(foursites2014), aes(x = Females.All, y = Gravel, color=Site))+geom_point()



#####TEMPORAL
####2013 ALL 5 SITES 

time2013 <- melt(mergefinal2013, id.vars=c("Site","Week"), measure.vars = c(6:293), na.rm=T)
mergeALLsites2013=ddply(merge2013ALLsites, c("variable", "Site", "Week"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
mergeALLsites2013

allsitesweek2013cast=cast(mergeALLsites2013, Site+Week~variable, value="mean")
allsites2013week=merge(allsitesweek2013cast,SiteInfo)


network_plot(correlate(allsites2013week[,c(55:63,86,)]), min_cor=0.7)

z=cor(allsites2013[,c(38:41,43:49,54:59,62:71,77:79,85,89:92,93,98,107:109)])
zdf <- as.data.frame(as.table(z))
subset(melt(zdf), value > .7)
zdf1<-subset(zdf, 1 > abs(Freq) & abs(Freq) > 0.7)
write.csv(zdf1, file = "ALLsites2013.csv")
