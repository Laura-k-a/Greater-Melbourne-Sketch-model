# Title:                  Melbourne BE-TR Stratifieded stops final models stepwise 
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script develops and evaluates four sketch models for Melbourne:
# Model 1:All transit modes
# Model 2:Train
# Model 3:Tram
# Model 4:Bus

###'Method of calibration: multivariate multiple linear regression of built environment and sociodemographic variables on transit ridership for]
###'Method of forecast evaluation: The RMSE of individual models; and of the cmobined model applied for bus, train & tram, is compared. 
# Data: Ridership data includes average normal (school) weekday ridership 
# Train, Tram ridership, averaged for 2018, by Victorian Department of Transport. 
# Bus ridership, averaged for 4 months from August - November 2018, provided by bus planning team at the Victorian Department of Transport
#Refer to [insert doi for figshare] for ontology and reference for built environment.
#Copyright statement: This script is the product of Laura Aston

install.packages("tidyverse")  # data manipulation
library(tidyverse)  # data manipulation
library(gridExtra)
library(dplyr)# mutate function
library(car)#VIF function
library(Hmisc)#rcorr function
library(lm.beta)
library(Metrics)

options(max.print= 1000000)
options(scipen = 999)
par(mfrow=c(2,2))
setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Sketch model Greater Melbourne")

#Refer to sampling script
Melb_Data<- read.csv("BE-TR_AllStops_data_v3.20201006.csv", header=TRUE, sep=",")
row.names(Melb_Data) <- Melb_Data[,c(2)]


Allmodes<- Melb_Data[which (Melb_Data$Standard. == 'yes'),]
bus_sketch<- Melb_Data[which (Melb_Data$Standard. == 'yes'&
                                Melb_Data$Mode=='bus'),]
tram_sketch<- Melb_Data[which (Melb_Data$Standard. == 'yes'&
                                 Melb_Data$Mode=='tram'),]
train_sketch<- Melb_Data[which (Melb_Data$Standard. == 'yes'&
                                 Melb_Data$Mode=='train'),]


#step 4 Simple correlations
Corrdata.Allmodes<-Allmodes[,c(21,22,56, 57, 25:50, 53, 54, 55, 58, 59)]
Corrdata.bus<-bus_sketch[,c(21,22,56, 57, 25:50, 53, 54, 55, 58, 59)]
Corrdata.tram<-tram_sketch[,c(21,22,56, 57, 25:50, 53, 54, 55, 58, 59)]
Corrdata.train<-train_sketch[,c(21,22,56, 57, 25:50, 53, 54, 55, 58, 59)]
#Option 1 for Correlation matrices with p-values
Corrdata.Allmodes<-rcorr(as.matrix(Corrdata.Allmodes))
Corrdata.bus<-rcorr(as.matrix(Corrdata.bus))
Corrdata.tram<-rcorr(as.matrix(Corrdata.tram))
Corrdata.train<-rcorr(as.matrix(Corrdata.train))
#option 2 for flat correlation matrix
#Set up a custom function to flatten
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

options(max.print=1000000)

Corrdata.Allmodes<-flattenCorrMatrix(Corrdata.Allmodes$r,Corrdata.Allmodes$P)
capture.output(Corrdata.Allmodes,file="Corrdata.allmodes.csv")

Corrdata.bus<-flattenCorrMatrix(Corrdata.bus$r,Corrdata.bus$P)
capture.output(Corrdata.bus,file="Corrdata.bus.csv")

Corrdata.tram<-flattenCorrMatrix(Corrdata.tram$r,Corrdata.tram$P)
capture.output(Corrdata.tram,file="Corrdata.tram.csv")

Corrdata.train<-flattenCorrMatrix(Corrdata.train$r,Corrdata.train$P)
capture.output(Corrdata.train,file="Corrdata.train.csv")


#take an equal sample of each mode; so 220 per mode
#bus
bus_sketch_sample<-sample(bus_sketch$Mode_rad, 220, replace = FALSE, prob = NULL)
bus_sketch_sample<-as.data.frame(bus_sketch_sample)
colnames(bus_sketch_sample)[1]  <- "Mode_rad"

tram_sketch_sample<-sample(tram_sketch$Mode_rad, 220, replace = FALSE, prob = NULL)
tram_sketch_sample<-as.data.frame(tram_sketch_sample)
colnames(tram_sketch_sample)[1]  <- "Mode_rad"

train_sketch_sample<-sample(train_sketch$Mode_rad, 220, replace = FALSE, prob = NULL)
train_sketch_sample<-as.data.frame(train_sketch_sample)
colnames(train_sketch_sample)[1]  <- "Mode_rad"

#bind the samples
Allmodes.sample<-rbind(train_sketch_sample,tram_sketch_sample,bus_sketch_sample)
Allmodes.sample<-merge(Allmodes.sample, Allmodes, by = "Mode_rad", all = FALSE)
row.names(Allmodes.sample) <- Allmodes.sample[,c(1)]



#Melb all vif
Melb.all.LM.vif<-vif(lm(ln_uncensored_0.1_Pat ~ X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist+ X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.sample))


Melb.all.LM.vif #ln_emp high -> remove


#melb all maximally adjusted
Melb.all.LM.1.1<-lm(ln_uncensored_0.1_Pat ~  X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist+ X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.sample)

Melb.all.LM.1.1<-lm.beta(Melb.all.LM.1.1)
summary(Melb.all.LM.1.1) #R2 = 0.7321

###'removed in order
###'FTE
###'Tram
###'ACDist
###'OS
###'Comm
###'bach
###'balance
Melb.all.LM.1<-lm(ln_uncensored_0.1_Pat ~  X3_Popden+ X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS+ X26_O_Train_LOS	+ X29_Censored_MeanSize, data =Allmodes.sample)

Melb.all.LM.1<-lm.beta(Melb.all.LM.1)
summary(Melb.all.LM.1) #R2 = 0.7335

plot(Melb.all.LM.1)

which(rownames(Allmodes.sample) == "221-train-800") #199
which(rownames(Allmodes.sample) == "219-train-800") #195
which(rownames(Allmodes.sample) == "220-train-800") #198
which(rownames(Allmodes.sample) == "4455-bus-400") #335
#remove potentially influential outliers 
Allmodes.sample.rd2<- Allmodes.sample[-c(199, 195, 198, 335),]

#rd 2
Melb.all.LM.2.1<-lm(ln_uncensored_0.1_Pat ~  X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist+ X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.sample.rd2)

Melb.all.LM.2.1<-lm.beta(Melb.all.LM.2.1)
summary(Melb.all.LM.2.1) #R2 = 0.7664

###'removed in order
###'PropOS
###'ACDist
###'Balance
###'PropFTE
###'Comm
Melb.all.LM.2.1<-lm(ln_uncensored_0.1_Pat ~  X3_Popden+ X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist +  X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS	+ X29_Censored_MeanSize + X32_PropBach, data =Allmodes.sample.rd2)

Melb.all.LM.2.1<-lm.beta(Melb.all.LM.2.1)
summary(Melb.all.LM.2.1) #R2 = 0.7676

plot(Melb.all.LM.2.1)


Melb.all.LM.2.1.vif<-vif(Melb.all.LM.2.1)
Melb.all.LM.2.1.vif #fine

capture.output(Melb.all.LM.2.1.vif, file = "Melb.all.LM.2.1.vif.txt")
capture.output(summary(Melb.all.LM.2.1), file = "Melb.all.LM.PM.txt")


#bus sketch model
bus.VIF<-vif(lm(ln_uncensored_0.1_Pat ~ X2ln.ln_Emp + X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =bus_sketch))
#removed FTZ as was aliased
bus.VIF #ok

#maximally adjusted model
bus.LM.1.1<-lm(ln_uncensored_0.1_Pat ~ X2ln.ln_Emp + X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =bus_sketch)

bus.LM.1.1<-lm.beta(bus.LM.1.1)
summary(bus.LM.1.1) #R2 = 0.524

plot(bus.LM.1.1)
capture.output(summary(bus.LM.1.1), file = "bus.LM.1.MA.txt")


#removed in order
###'cycle connect
###'parking
###'Tram_O_LoS
###'Balance
bus.LM.1<-lm(ln_uncensored_0.1_Pat ~ X2ln.ln_Emp + X3_Popden+ X6_PropComm + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =bus_sketch.rd2)

bus.LM.1<-lm.beta(bus.LM.1)
summary(bus.LM.1) #R2 = 0.5237

plot(bus.LM.1)

bus.LM.1.VIF<-vif(bus.LM.1)
bus.LM.1.VIF
capture.output(bus.LM.1.VIF, file = "bus.LM.1.vif.txt")
capture.output(summary(bus.LM.1), file = "bus.LM.1.txt")

#tram
#NS for tram (from corr matrix):
#X15_Parkiteer
#X22.Parking_m.2
#X27_O_LOS
#X32_PropBach

tram.VIF<-vif(lm(ln_uncensored_0.1_Pat ~ X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore+ X16_CBDDist + X17_ACDist + X18_ACCount + X21._FTZ + X19_PropUrban + X20_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS, data =tram_sketch))

tram.VIF#ln_emp is 12 -> remove (probably colinear with FTZ and CBD dist)
#Emp access still high a 5.6. Keep for now

tram.LM.1.1<-lm(ln_uncensored_0.1_Pat ~ X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X21._FTZ + + X19_PropUrban + X20_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS, data =tram_sketch)

tram.LM.1.1<-lm.beta(tram.LM.1.1)
summary(tram.LM.1.1) #R2 = 0.6033
capture.output(summary(tram.LM.1.1), file = "tram.LM.1.MA.txt")

###'removed in order
###'Emp access
###'O_Tram_LOS
###'AC_Dist
###'Prop_FTE
###'FTZ
###'PopDen
###'AC Count
###'CBD Dist
###'Housing Div
###'LUEntropy
###'propUrban
###'PropOS


tram.LM.1<-lm(ln_uncensored_0.1_Pat ~ X6_PropComm + X8_Balance  +X11_IntDensity + X12_CycleConnect + X13_DestScore + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS	+ X29_Censored_MeanSize, data =tram_sketch)

tram.LM.1<-lm.beta(tram.LM.1)
summary(tram.LM.1) #R2 = 0.6067
 

plot(tram.LM.1)

#127 tram
which(rownames(tram_sketch) == "127-tram") #1
which(rownames(tram_sketch) == "170-tram") #712
which(rownames(tram_sketch) == "204-tram") #710
#remove potentially influential outliers 
tram_sketch.rd2<- tram_sketch[-c(1, 712, 710),]

#tram rd. 2
tram.LM.2.1<-lm(ln_uncensored_0.1_Pat ~ X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X21._FTZ + + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS, data =tram_sketch.rd2)

tram.LM.2.1<-lm.beta(tram.LM.2.1)
summary(tram.LM.2.1) #R2 = 0.7017

###'removed in order
###'mean size
###'CBDDist
###'emp access
###'ACDist
###'ACCount
###'Housing Div

tram.LM.2<-lm(ln_uncensored_0.1_Pat ~ X3_Popden+ X6_PropComm + X8_Balance + X9_LUEntropy + X11_IntDensity + X12_CycleConnect + X13_DestScore + X21._FTZ + + X19_PropUrban +X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X31_PropOS, data =tram_sketch.rd2)

tram.LM.2<-lm.beta(tram.LM.2)
summary(tram.LM.2) #R2 = 0.7032

plot(tram.LM.2)

tram.LM.2.vif<-vif(tram.LM.2)
tram.LM.2.vif #fine

capture.output(summary(tram.LM.2), file = "tram.LM.2.PM.txt")
capture.output(tram.LM.2.vif, file = "tram.LM.2.vif")

#train Not significant
#X12_CycleConnect
#X30_MedInc
#X28_Censored_PropFTE


#X3ln.ln_Pop is stronger than untransformed

train.VIF<-vif(lm(ln_uncensored_0.1_Pat ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS+  X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =train_sketch))
train.VIF ##emp den is 10 -> remove. Popden is 7.2, keep for now

#maximally adjusted model
train.LM.1.1<-lm(ln_uncensored_0.1_Pat ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS+  X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =train_sketch)

train.LM.1.1<-lm.beta(train.LM.1.1)
summary(train.LM.1.1) #R2 = 0.779
capture.output(summary(train.LM.1.1), file = "train.LM.1.MA.txt")

###' removed in  order
###' #Intdensity
###' mean size
###' lu
###' comm
###' bach
###' pop
###' dest score
train.LM.1<-lm(ln_uncensored_0.1_Pat ~ X8_Balance +  X10_HousingDiv +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS+  X31_PropOS, data =train_sketch)

train.LM.1<-lm.beta(train.LM.1)
summary(train.LM.1) #R2 = 0.784

plot(train.LM.1)


#2 train, 597, 609 (potentially)
which(rownames(train_sketch) == "658-train") #1
which(rownames(train_sketch) == "597-train") #12
which(rownames(train_sketch) == "609-train")#11
which(rownames(train_sketch) == "655-train") #2
which(rownames(train_sketch) == "652-train") #3
#remove potentially influential outliers 
train_sketch.rd2<- train_sketch[-c(1, 2, 3, 12, 11),]


#train rd. 2
train.LM.2<-lm(ln_uncensored_0.1_Pat ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X21._FTZ + X22.Parking_m.2 + X19_PropUrban + X20_EmpAccess + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS+  X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =train_sketch.rd2)

train.LM.2<-lm.beta(train.LM.2)
summary(train.LM.2) #R2 = 0.8553

###'removed in order
###' int density
###' AC Count
###' Emp Access
###' mean size
###' FTZ
train.LM.2<-lm(ln_uncensored_0.1_Pat ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X22.Parking_m.2 + X19_PropUrban + X23_C_ln_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS + X32_PropBach, data =train_sketch.rd2)

train.LM.2<-lm.beta(train.LM.2)
summary(train.LM.2) #R2 = 0.855

plot(train.LM.2)

capture.output(summary(train.LM.2), file = "train.LM.2.PM.txt")

#RMSE for mode-specific models
train.sketch.predictions<-as.data.frame(bus.sketch.predictions <- predict(train.LM.2, train_sketch))
capture.output(train.sketch.predictions,file = "train.sketch.predictions.csv")
rmse_train<-sqrt(mean(residuals(train.LM.2)^2))
rmse_train

bus.sketch.predictions<-as.data.frame(bus.sketch.predictions <- predict(bus.LM.1, bus_sketch))
capture.output(bus.sketch.predictions,file = "bus.sketch.predictions.csv")
rmse_bus<-sqrt(mean(residuals(bus.LM.1)^2))
rmse_bus

tram.sketch.predictions<-as.data.frame(tram.sketch.predictions <- predict(tram.LM.2, tram_sketch))
capture.output(tram.sketch.predictions,file = "tram.sketch.predictions.csv")
rmse_tram<-sqrt(mean(residuals(tram.LM.2)^2))
rmse_tram

rmse_all<-sqrt(mean(residuals(Melb.all.LM.1.5)^2))
rmse_all

#RMSE for within-sample models
Melb.bus.predictions <- predict(Melb.all.LM.2.1, bus_sketch.rd2)
rmse_Melb.bus.predictions<-rmse(bus_sketch$ln_uncensored_0.1_Pat, Melb.bus.predictions)
rmse_Melb.bus.predictions

Melb.tram.predictions <- predict(Melb.all.LM.2.1, tram_sketch.rd2)
rmse_Melb.tram.predictions<-rmse(tram_sketch.rd2$ln_uncensored_0.1_Pat, Melb.tram.predictions)
rmse_Melb.tram.predictions

Melb.train.predictions <- predict(Melb.all.LM.2.1, train_sketch.rd2)
rmse_Melb.train.predictions<-rmse(train_sketch.rd2$ln_uncensored_0.1_Pat, Melb.train.predictions)
rmse_Melb.train.predictions



