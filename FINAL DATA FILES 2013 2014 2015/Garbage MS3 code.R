##GARBAGE MS3 Code






PopSite2013=subset(PopSite1314AVG, Year=="2013")
cor.test(log(PopSite2013$TotalEmbryos), log(PopSite2013$TotalRec), method="pearson")

PopSite20144sites=subset(PopSite1314AVG, Year=="2014" | Site=="AC"| Site=="AP"| Site=="FB"| Site=="TC")
cor.test(log(PopSite20144sites$TotalEmbryos), log(PopSite20144sites$TotalRec), method="pearson")

PopSite2014=subset(PopSite1314AVG, Year=="2014")
cor.test(log(PopSite2014$TotalEmbryos), log(PopSite2014$TotalRec), method="pearson")

table=PopSite1314AVG[,c(1,2, 100, 106, 6, 48, 3 ,73)]

summary(lme(Slope.average ~ Avggrainsize, random=~1|Year, data = subset(PopSite1314AVG,Year=="2014")))

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = Slope.average, color=Site))+geom_point()+facet_grid(Year~.)

####FINAL STRUCTURAL EQUATION MODEL
#Habitat Size
HabSize <-lme(log(Size.1) ~ X1kmcoastlineangle + QualityHabitat, random=~1|Year, data = PopSite1314AVG)
summary(HabSize)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)


##Density of recruits
Recruit <-lme(s1to3mm ~ X1kmcoastlineangle + QualityHabitat, random=~1|Year, data = PopSite1314AVG)
summary(Recruit)


ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = QualityHabitat, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)

##Total recruits
TotRecruit <-lme(log(TotalRec) ~ s1to3mm + log(Size.1), random=~1|Year, data = PopSite1314AVG)
summary(TotRecruit)

ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = log(TotalRec), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = s1to3mm, y = log(TotalRec), color=Site))+geom_point()+facet_grid(Year~.)

##Density of ovigerous
DenOvig <-lme(ReproDensity ~  X50cm2.10cm2 + s1to3mm, random=~1|Year, data = PopSite1314AVG)
summary(DenOvig)

ggplot(data = PopSite1314AVG, aes(x = X50cm2.10cm2 , y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)

##Total Ovigerous
TotOvig <-lme(log(TotalRepros) ~ log(Size.1) + ReproDensity, random=~1|Year, data = PopSite1314AVG)
summary(TotOvig)


##Avg ovig size
OvigSize <-lme(Ovigerous.size ~ X50cm2.10cm2, random=~1|Year, data = PopSite1314AVG)
summary(OvigSize)

ggplot(data = PopSite1314AVG, aes(x = X50cm2.10cm2, y = Ovigerous.size, color=Site))+geom_point()+facet_grid(Year~.)

##Total Embryos
TotReproOut <-lme(log(TotalEmbryos) ~ log(TotalRepros) + Ovigerous.size , random=~1|Year, data = PopSite1314AVG)
summary(TotReproOut)

ggplot(data = PopSite1314AVG, aes(x = Ovigerous.size, y = log(TotalEmbryos), color=Site))+geom_point()+facet_grid(Year~.)

ReproRecModel <- psem(HabSize, Recruit, TotRecruit, DenOvig, TotOvig, OvigSize, TotReproOut) 
summary(ReproRecModel)

###Coastline by Hab
ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = X50cm2.10cm2 , color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = X50cm2.10cm2 , color=Site, size=Size.1))+geom_point()+facet_grid(Year~.)


#####DENSITY OF RECRUITS AND OVIGEROUS
melted <- melt(PopSite1314, id.vars=c("Site", "Year"), measure.vars = c(3,6), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = P1314, aes(x = variable, y = mean, fill=Site))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site~Year, scale="free")
ggplot(data = P1314, aes(x = variable, y = mean, fill=Site))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(Site~Year)

melted <- melt(PopSite1314, id.vars=c("Site", "Year"), measure.vars = c(3), na.rm=T)
P1314=ddply(melted, c("variable", "Site", "Year"), summarise, mean = mean(value), sd = sd(value), sem = sd(value)/sqrt(length(value)))
ggplot(data = P1314, aes(x = Site, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2) + facet_grid(.~Year)



###NMDS for Exposure, Hab, Size
#2014
Pop14=subset(PopSite1314AVG, Year=="2014")
Pop14cond=Pop14[,c("Size.1", "QualityHabitat", "X1kmcoastlineangle")]
POPNMDS=metaMDS(Pop14cond, trymax=1999, k=2, dist="bray")
POPNMDS 
stressplot(POPNMDS) #Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions

#plot of NMDS
NMDS1 = POPNMDS$points[,1]
NMDS2 = POPNMDS$points[,2]
NMDS = data.frame(NMDS1 = NMDS1, NMDS2 = NMDS2)

Nplot <- ggplot(NMDS, aes(x=NMDS1, y=NMDS2, col=Pop14$Site)) + geom_point(size=2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.2),legend.title=element_blank(),legend.text=element_text(size=12),panel.border = element_rect(colour = "black", fill=NA, size=1))#+annotate("text", label = "Stress: 0.15", x = 1, y = -1.35, size = 3.5, colour = "black")
Nplot

library(scatterplot3d)
attach(mtcars)
scatterplot3d(Pop14$QualityHabitat,Pop14$Size.1,Pop14$X1kmcoastlineangle, type="h")


p <- plot_ly(Pop14, x = ~QualityHabitat, y = ~log(Size.1), z = ~X1kmcoastlineangle, color = ~Site)
p

# create column indicating point color
Pop14$pcolor[Pop14$Site=="AC"] <- "red"
Pop14$pcolor[Pop14$Site=="AP"] <- "blue"
Pop14$pcolor[Pop14$Site=="TC"] <- "darkgreen"
Pop14$pcolor[Pop14$Site==4] <- "red"
Pop14$pcolor[Pop14$Site==6] <- "blue"
Pop14$pcolor[Pop14$Site==8] <- "darkgreen"
Pop14$pcolor[Pop14$Site==4] <- "red"
Pop14$pcolor[Pop14$Site==6] <- "blue"
Pop14$pcolor[Pop14$Site==8] <- "darkgreen"
with(mtcars, {
  s3d <- scatterplot3d(disp, wt, mpg,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates no. of cylinders
                       type="h", lty.hplot=2,       # lines to the horizontal plane
                       scale.y=.75,                 # scale y axis (reduce by 25%)
                       main="3-D Scatterplot Example 4",
                       xlab="Displacement (cu. in.)",
                       ylab="Weight (lb/1000)",
                       zlab="Miles/(US) Gallon")
  s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
       labels=row.names(mtcars),       # text to plot
       pos=4, cex=.5)                  # shrink text 50% and place to right of points)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Number of Cylinders",
         c("4", "6", "8"), fill=c("red", "blue", "darkgreen"))
})



####---------
####FINAL STRUCTURAL EQUATION MODEL WITH protected v exposed differences
#Grain Size
ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)

GrainEx <-lme(log(Avggrainsize) ~ X1kmcoastlineangle + Surfzone.size..m., random=~1|Year, data = subset(PopSite1314AVG, X1kmcoastlineangle < 325))
summary(GrainEx)

GrainProt <-lme(log(Avggrainsize) ~ X1kmcoastlineangle + Surfzone.size..m., random=~1|Year, data = subset(PopSite1314AVG, X1kmcoastlineangle > 325))
summary(GrainProt)

ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)

##Density of recruits
Recruit <-lme(s1to3mm ~ X1kmcoastlineangle * Avggrainsize, random= ~ 1|Year, data = PopSite1314AVG)
summary(Recruit)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325) , aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(sub, Avggrainsize > 11), aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)

sub=subset(P14, X1kmcoastlineangle > 325 )
ggplot(data = subset(sub, Avggrainsize < 12), aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
Recruit <-lm(s1to3mm ~ Avggrainsize, data = subset(sub, Avggrainsize < 12))
summary(Recruit)

ggplot(data = subset(sub, Avggrainsize > 11), aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
Recruit <-lm(s1to3mm ~ Avggrainsize, data = subset(P14, Avggrainsize > 11))
summary(Recruit)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(P14, X1kmcoastlineangle < 325), aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(P14, X1kmcoastlineangle > 325), aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(P14, X1kmcoastlineangle < 325), aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(P14, X1kmcoastlineangle > 325), aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)


##Total recruits
TotRecruit <-lme(log(TotalRec) ~ s1to3mm + log(Size.1), random=~1|Year, data = PopSite1314AVG)
summary(TotRecruit)
ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = log(TotalRec), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = s1to3mm, y = log(TotalRec), color=Site))+geom_point()+facet_grid(Year~.)

##Density of ovigerous
DenOvig <-lme(ReproDensity ~  log(Size.1) + Avggrainsize + s1to3mm, random=~1|Year, data = PopSite1314AVG)
summary(DenOvig)

ggplot(data = PopSite1314AVG, aes(x = log(Size.1) , y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)

##Total Ovigerous
TotOvig <-lme(log(TotalRepros) ~ log(Size.1) + ReproDensity , random=~1|Year, data = PopSite1314AVG)
summary(TotOvig)

##Avg ovig size
OvigSize <-lme(Ovigerous.size ~ Avggrainsize + log(Size.1), random=~1|Year, data = PopSite1314AVG)
summary(OvigSize)

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = Ovigerous.size, color=Site))+geom_point()+facet_grid(Year~.)

##Total Embryos
TotReproOut <-lme(log(TotalEmbryos) ~ log(TotalRepros) + Ovigerous.size , random=~1|Year, data = PopSite1314AVG)
summary(TotReproOut)

ggplot(data = PopSite1314AVG, aes(x = Ovigerous.size, y = log(TotalEmbryos), color=Site))+geom_point()+facet_grid(Year~.)

ReproRecModel <- psem(HabSize, Recruit, DenOvig, TotOvig, OvigSize, TotReproOut, TotRecruit) 
summary(ReproRecModel)


###PLOTS FOR SEM INFO
ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
##???
ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = log(Size.1), color=Site))+geom_point(size=2)+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = log(Size.1), color=Site))+geom_point(size=2)+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = Ovigerous.size, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = ReproDensity, y = TotalRepros, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = s1to3mm, y = TotalRec, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = TotalRec, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Difference.from.300.Northery.heading, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Slope.min, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = subset(PopSite1314AVG, Year == "2014"), aes(x = Slope.min, y = Difffrom294AngIncid, color=Site))+geom_point(size=3)
ggplot(data = subset(PopSite1314AVG, Year == "2014"), aes(x = Slope.min, y = Avggrainsize, color=Site))+geom_point()

ggplot(data = PopSite1314AVG, aes(x = ReproDensity, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = PL.crabs, color=Site))+geom_point()+facet_grid(Year~.)
PL <-lm(PL.crabs ~ Slope.min,  data = subset(PopSite1314AVG, Year=="2013"))
summary(PL)

exposure=ggplot(data = PopSite1314AVG, aes(y = Difference.from.300.Northery.heading, x = Site))+geom_point()+facet_grid(Year~.)
surfzone=ggplot(data = PopSite1314AVG, aes(y = Avggrainsize, x = Site))+geom_point()+facet_grid(Year~.)
grid.arrange(exposure, surfzone)

####2nd best AVERAGE STRUCTURAL EQUATION MODEL
##Exogenous variables = slope, surfzone, size of hab

#Slope
Slope <-lme(Slope.average ~ X1kmcoastlineangle + m.to.land.in.300dir, random=~1|Year, data = PopSite1314AVG)
summary(Slope)

#Habitat Size
HabSize <-lme(log(Size.1) ~ Difference.from.300.Northery.heading * Coastline.reg.0.5.km, random=~1|Year, data = PopSite1314AVG)
summary(HabSize)


##Density of recruits
Recruit <-lme(s1to3mm ~ Coastline.reg.0.5.km * Difference.from.300.Northery.heading + Avggrainsize, random=~1|Year, data = PopSite1314AVG)
summary(Recruit)

##Density of ovigerous
DenOvig <-lme(ReproDensity ~ log(Size.1) + Avggrainsize + s1to3mm , random=~1|Year, data = PopSite1314AVG)
summary(DenOvig)

DenOvig <-lme(log(ReproDensity) ~  Avggrainsize, random=~1|Year, data = PopSite1314AVG)
summary(DenOvig)


##Total Ovigerous
TotOvig <-lme(TotalRepros ~ log(Size.1) + ReproDensity , random=~1|Year, data = PopSite1314AVG)
summary(TotOvig)

##Avg ovig size
OvigSize <-lme(Ovigerous.size ~ Avggrainsize + log(Size.1), random=~1|Year, data = PopSite1314AVG)
summary(OvigSize)

##Total Embryos
hist(PopSite1314AVG$TotalEmbryos)
TotReproOut <-lme(TotalEmbryos ~ TotalRepros + Ovigerous.size , random=~1|Year, data = PopSite1314AVG)
summary(TotReproOut)

ReproRecModel <- psem(HabSize, Recruit, DenOvig, TotOvig, OvigSize, TotReproOut) 
summary(ReproRecModel)




###GRAPH FOR EXPECTATIONS
ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = Avggrainsize, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = Avggrainsize, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)


ggplot(data = PopSite2014, aes(x = ReproDensity, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite2014, X1kmcoastlineangle < 325), aes(x = ReproDensity, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite2014, X1kmcoastlineangle > 325), aes(x = ReproDensity, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite2014, aes(x = log(ReproDensity), y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite2014, X1kmcoastlineangle < 325), aes(x = log(ReproDensity), y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite2014, X1kmcoastlineangle > 325), aes(x = log(ReproDensity), y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)
ReproEx<-lm(log(ReproDensity) ~ Avggrainsize + log(Size.1), data = subset(PopSite2014, X1kmcoastlineangle < 325))
summary(ReproEx)
ReproProt<-lm(log(ReproDensity) ~ Avggrainsize + log(Size.1), data = subset(PopSite2014, X1kmcoastlineangle > 325))
summary(ReproProt)

RecEx<-lm(s1to3mm ~ Avggrainsize + log(Size.1), data = subset(PopSite2014, X1kmcoastlineangle < 325))
summary(ReproEx)
ReproProt<-lm(s1to3mm ~ Avggrainsize + log(Size.1), data = subset(PopSite2014, X1kmcoastlineangle > 325))
summary(ReproProt)

Grain<-lme(Avggrainsize ~ X1kmcoastlineangle,random=~1|Year, data = PopSite1314AVG)
summary(Grain)
ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.) + scale_radius()

Grain<-lme(Avggrainsize ~ log(Size.1) ,random=~1|Year, data = PopSite1314AVG)
summary(Grain)
ggplot(data = PopSite1314AVG, aes(x = log(Size.1), y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.) + scale_radius()



##IMPORTANT??
RmR <-lme(ReprominusRecs ~ X1kmcoastlineangle * Avggrainsize , random=~1|Year, data = PopSite1314AVG)
summary(RmR)
ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = Avggrainsize, color=Site, size=ReprominusRecs))+geom_point()+facet_grid(Year~.) + scale_radius()


###OTHER
RmR <-lme(ReprominusRecs ~ Avggrainsize + log(Size.1) , random=~1|Year, data = PopSite1314AVG)
summary(RmR)


ggplot(data = PopSite1314AVG, aes(x = ReprominusRecs, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = ReprominusRecs, y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = Avggrainsize, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = Avggrainsize, y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle , y = Avggrainsize, color=Site))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = X1kmcoastlineangle , y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)
exposed=ggplot(data = subset(PopSite2014, X1kmcoastlineangle < 325), aes(x = Avggrainsize, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)+ ggtitle("exposed")
protected=ggplot(data = subset(PopSite2014, X1kmcoastlineangle > 325), aes(x = Avggrainsize, y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.) + ggtitle("protected")
grid.arrange(exposed,protected)


ggplot(data = PopSite1314AVG, aes(x = Avggrainsize , y = log(Size.1), color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = log(Size.1), y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = log(Size.1), y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = log(Size.1), y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = log(Size.1), y = ReproDensity, color=Site))+geom_point()+facet_grid(Year~.)


ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle < 325), aes(x = log(Size.1), y = Avggrainsize, color=Site, size=ReproDensity))+geom_point()+facet_grid(Year~.)
ggplot(data = subset(PopSite1314AVG, X1kmcoastlineangle > 325), aes(x = log(Size.1), y = Avggrainsize, color=Site, size=s1to3mm))+geom_point()+facet_grid(Year~.)

ggplot(data = PopSite1314AVG, aes(x = ReproDensity , y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)
exposed=ggplot(data = subset(PopSite2014, X1kmcoastlineangle < 325), aes(x = ReproDensity, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.)+ ggtitle("exposed")
protected=ggplot(data = subset(PopSite2014, X1kmcoastlineangle > 325), aes(x = ReproDensity, y = s1to3mm, color=Site))+geom_point()+facet_grid(Year~.) + ggtitle("protected")
grid.arrange(exposed,protected)

fitless325 <- glm(formula=s1to3mm ~ s(ReproDensity), family=Gamma, data=subset(PopSite2014, X1kmcoastlineangle < 325))
summary(fitless325)

fitmore325 <- glm(formula=s1to3mm ~ s(ReproDensity), family=Gamma, data=subset(PopSite2014, X1kmcoastlineangle > 325))
summary(fitmore325)

sub=subset(PopSite2014, X1kmcoastlineangle < 325)
xx <- seq(0,30, length=17)
plot(sub$ReproDensity,sub$s1to3mm)
lines(xx, predict(glmGamma, data.frame(x=xx)), col="red")



fit1 <- lm(s1to3mm ~ poly(ReproDensity, 2, raw=TRUE), data=subset(PopSite2014, X1kmcoastlineangle > 325))
summary(fit1)



##SAND TO RECRUITS
sandrec <-lm(s1to3mm~ bare.cobble...2cm., data = subset(PopSite1314AVG, Year=="2013"))
summary(sandrec)

sandrecplot <- ggplot(data = subset(PopSite1314AVG, Year=="2014"), aes(x = , y = s1to3mm, color=Site))+geom_point(position = position_jitter(w = 3, h = 0))+ylab("recruit density") +xlab("bare sand and fine sediments")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_smooth(method='lm', se=F, color="black") #+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
sandrecplot

sandrec <-lm(s1to3mm~ bare.sand..fine.very.course.sand., data = subset(PopSite1314AVG, Year=="2013"))
summary(sandrec)

sub <- subset(PopSite1314AVG, bare.sand..fine.very.course.sand. < 10)
sandrecplot <- ggplot(data = subset(sub, Year=="2013"), aes(x = bare.sand..fine.very.course.sand., y = s1to3mm, color=as.factor(X1coastminus300)))+geom_point()+ylab("recruit density") +xlab("bare sand and fine sediments")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))  #+ geom_text(label=PopSite1314AVG$Site, hjust = 0, nudge_x = 2, size=3.5)
sandrecplot

plot(PopSite1314AVG$bare.sand..fine.very.course.sand.~PopSite1314AVG$s1to3mm)
