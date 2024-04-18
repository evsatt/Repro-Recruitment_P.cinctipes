#read in files
Petro20122013=read.csv("20122013Petro.csv", header=TRUE,as.is=TRUE)
PetroApril2013=read.csv("2013AprilPetro.csv", header=TRUE,as.is=TRUE)
PetroMay2013=read.csv("2013MayPetro.csv", header=TRUE,as.is=TRUE)
PetroJune2013=read.csv("2013JunePetro.csv", header=TRUE,as.is=TRUE)
PetroJune22013=read.csv("2013June2Petro.csv", header=TRUE,as.is=TRUE)  
PetroJuly2013=read.csv("2013JulyPetro.csv", header=TRUE,as.is=TRUE) 
PetroJuly22013=read.csv("2013July2Petro.csv", header=TRUE,as.is=TRUE) 
PetroAug2013=read.csv("2013AugPetro.csv", header=TRUE,as.is=TRUE) 
PetroDec2013=read.csv("2013DecPetro.csv", header=TRUE,as.is=TRUE) 

#combine data frames
PetroAll=do.call("rbind", list(Petro20122013, PetroApril2013, PetroMay2013,PetroJune2013,PetroJune22013,PetroJuly2013,PetroJuly22013,PetroAug2013,PetroDec2013))

#change date column
PetroAll$Date.new <- as.Date(PetroAll$Date, format="%m/%d/%Y")

#check columns
unique(PetroAll$Site)
unique(PetroAll$Date.new)
unique(PetroAll$Quadrat)
unique(PetroAll$Sex)
unique(PetroAll$Size)
unique(PetroAll$Ovigerous)

##COBBLE
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA/2013Cobble")

Cobble20122013=read.csv("20122013Cobble.csv", header=TRUE,as.is=TRUE) 
CobbleApril2013=read.csv("2013AprilCobble.csv", header=TRUE,as.is=TRUE) 
CobbleMay2013=read.csv("2013MayCobble.csv", header=TRUE,as.is=TRUE) 
CobbleJune2013=read.csv("2013JuneCobble.csv", header=TRUE,as.is=TRUE) 
CobbleJune22013=read.csv("2013June2Cobble.csv", header=TRUE,as.is=TRUE) 
CobbleJuly2013=read.csv("2013JulyCobble.csv", header=TRUE,as.is=TRUE)  
CobbleJuly22013=read.csv("2013July2Cobble.csv", header=TRUE,as.is=TRUE)  
CobbleAug2013=read.csv("2013AugCobble.csv", header=TRUE,as.is=TRUE)  

#combine data frames
CobbleAll=do.call("rbind", list(Cobble20122013, CobbleApril2013, CobbleMay2013,CobbleJune2013,CobbleJune22013,CobbleJuly2013,CobbleJuly22013,CobbleAug2013))

#change date column
CobbleAll$Date.new <- as.Date(CobbleAll$Date, format="%m/%d/%Y")
colnames(CobbleAll)

#check columns
unique(CobbleAll$Site)
unique(CobbleAll$Date.new)
unique(CobbleAll$Transect)
unique(CobbleAll$Quadrat.number)

View(CobbleAll)

##PREDCOMP
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA/2013PredComp")

PredComp20122013=read.csv("20122013PredComp.csv", header=TRUE,as.is=TRUE)
PredCompApril2013=read.csv("2013AprilPredComp.csv", header=TRUE,as.is=TRUE)
PredCompMay2013=read.csv("2013MayPredComp.csv", header=TRUE,as.is=TRUE)
PredCompJune2013=read.csv("2013JunePredComp.csv", header=TRUE,as.is=TRUE)
PredCompJune22013=read.csv("2013June2PredComp.csv", header=TRUE,as.is=TRUE)  
PredCompJuly2013=read.csv("2013JulyPredComp.csv", header=TRUE,as.is=TRUE)
PredCompJuly22013=read.csv("2013July2PredComp.csv", header=TRUE,as.is=TRUE) 
PredCompAug2013=read.csv("2013AugPredComp.csv", header=TRUE,as.is=TRUE) 

#combine data frames
PredCompAll=do.call("rbind", list(PredComp20122013, PredCompApril2013, PredCompMay2013,PredCompJune2013,PredCompJune22013,PredCompJuly2013,PredCompJuly22013,PredCompAug2013))
colnames(PredCompAll)

#change date column
PredCompAll$Date.new <- as.Date(PredCompAll$Date, format = "%m/%d/%y")

#check columns
unique(PredCompAll$Site)
unique(PredCompAll$Date.new)
unique(PredCompAll$Quadrat)
unique(PredCompAll$Transect)

#delete rows with 0s
PredCompAll<-PredCompAll[!(PredCompAll$Site==0),]

##ALGAE
setwd("~/Desktop/Final Pop 2013:2014:2015 Data/FINAL DATA FILES 2013 2014 2015/2013 FINAL DATA/2013Algae")

Algae20122013=read.csv("20122013Algae.csv", header=TRUE)
AlgaeApril2013=read.csv("2013AprilAlgae.csv", header=TRUE)
AlgaeMay2013=read.csv("2013MayAlgae.csv", header=TRUE)
AlgaeJune2013=read.csv("2013JuneAlgae.csv", header=TRUE)
AlgaeJune22013=read.csv("2013June2Algae.csv", header=TRUE)  
AlgaeJuly2013=read.csv("2013JulyAlgae.csv", header=TRUE)  
AlgaeJuly22013=read.csv("2013July2Algae.csv", header=TRUE) 
AlgaeAug2013=read.csv("2013AugAlgae.csv", header=TRUE) 
