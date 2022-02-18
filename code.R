library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readr)

library(lattice)
library(ggplot2)
library(caret)
library(dplyr)

#==============================
#Importing the dataset
#==============================
crashData <- read.csv("C:/Users/Halima/Desktop/github/crash-incidents-data-analysis/cpd-crash-incidents .csv", sep=";")
View(crashData)
summary(crashData)

min(crashData$Crash_Date)
max(crashData$Crash_Date)
head(crashData)

#==============================
#Data Visualizations
#==============================
#road configuration 
x = data.frame(table(crashData[['Road_Configuration']]))
piepercent = round(100*x$Freq/sum(x$Freq),1)
pie(x$Freq, labels = paste(x$Var1, sep = ' ', piepercent, '%'), main = "Percentage of Crashes on Different Road Configurations", col = brewer.pal(6,"Pastel2"))
#weather
w = table(crashData$Weather)
extraMargin = par(mar = c(8,4,4,2) + 0.1)
par(extraMargin)
barplot(w,main = "Crash Frequency with Weather",xlab = " ",ylab="Number of Crashes",las=2,cex.names = 0.7,cex.axis = 0.8, col = brewer.pal(10,"RdBu"))
mtext("Weather", side=1, line=9)
par(mar=c(12,5,5,4))
#number of crashes over time
c = table(crashData$TA_Date)
View(c)
plot(c, las=2,cex.axis = 0.8, xlab = " ",ylab ="Number of Crashes",main="Number of Crashes per Day")
mtext("Date", side=1, line=6)

#==============================
#Hypothesis Testing
#==============================
crashData$Date = str_trunc(crashData$Crash_Date,20,"right",ellipsis=" ")
trimws(crashData$Date)
crashData$TimeOnly = str_trunc(crashData$Date,10,"left",ellipsis=" ")
trimws(crashData$TimeOnly)

range(crashData$TimeOnly)
crashData$TimeOnly = as.POSIXct(crashData$TimeOnly, format ="%H:%M:%S")
crashData$TimeOnly = format(crashData$TimeOnly, format ="%H:%M:%S")
#Create subset of table with time after 8 pm and before 5am
nightCrash = subset(crashData, crashData$TimeOnly >= "20:00:00")
nightCrashBefore5 = subset(crashData, crashData$TimeOnly < "05:00:00")
nightCrash = rbind(nightCrash, nightCrashBefore5)
#Create subset of table with time before 8 pm and after 5 am
dayCrash = subset(crashData, crashData$TimeOnly < "20:00:00")
dayCrash = dayCrash[!(dayCrash$TimeOnly %in% nightCrashBefore5$TimeOnly),]

mean.night = mean(nightCrash$Fatality)
mean.day = mean(dayCrash$Fatality)

sd.night = sd(nightCrash$Fatality)
sd.day = sd(dayCrash$Fatality)

num.night = length(nightCrash$Fatality)
num.day = length(dayCrash$Fatality)

zscore = (mean.day - mean.night) / sqrt ( (sd.day^2/num.day) + (sd.night^2/num.night))
zscore

p = pnorm(zscore)
p

#==============================
#Prediction
#==============================
#Source Errors.R
source("D:/Rutgers/Fall 2021/Data 101/Assignment 11/Errors.R")

#create test and train dataset with 50% training dataset and 50% testing
dt = sort(sample(nrow(crashData),nrow(crashData)*.5))
crashDataTrain = crashData[dt,]
crashDataTest = crashData[-dt,]
crashDataTestWithout = subset(crashDataTest, select = -c(Fatality,Injury))
range(crashData$Injury)
#create prediction model

crashDataTrain$Weather = as.factor(crashDataTrain$Weather)
crashDataTrain$Traffic_Control = as.factor(crashDataTrain$Traffic_Control)
crashDataTrain$Road_Configuration = as.factor(crashDataTrain$Road_Configuration)
crashDataTrain$Road_Conditions = as.factor(crashDataTrain$Road_Conditions)
crashDataTrain$Light_Condition = as.factor(crashDataTrain$Light_Condition)
crashDataTrain$Vehicle.Type = as.factor(crashDataTrain$Vehicle.Type)

modelFatality = lm(Fatality ~ Weather+Traffic_Control+Road_Configuration+Road_Conditions+Light_Condition+Vehicle.Type,data=crashDataTrain)
modelInjury = lm(Injury ~ Weather+Traffic_Control+Road_Configuration+Road_Conditions+Light_Condition+Vehicle.Type,data=crashDataTrain)

modelFatality$xlevels[["Weather"]] <- union(modelFatality$xlevels[["Weather"]], levels(crashDataTestWithout[["Weather"]]))
modelFatality$xlevels[["Traffic_Control"]] <- union(modelFatality$xlevels[["Traffic_Control"]], levels(crashDataTestWithout[["Traffic_Control"]]))
modelFatality$xlevels[["Road_Configuration"]] <- union(modelFatality$xlevels[["Road_Configuration"]], levels(crashDataTestWithout[["Road_Configuration"]]))
modelFatality$xlevels[["Road_Conditions"]] <- union(modelFatality$xlevels[["Road_Conditions"]], levels(crashDataTestWithout[["Road_Conditions"]]))
modelFatality$xlevels[["Vehicle.Type"]] <- union(modelFatality$xlevels[["Vehicle.Type"]], levels(crashDataTestWithout[["Vehicle.Type"]]))




modelInjury$xlevels[["Weather"]] <- union(modelInjury$xlevels[["Weather"]], levels(crashDataTestWithout[["Weather"]]))
modelInjury$xlevels[["Traffic_Control"]] <- union(modelInjury$xlevels[["Traffic_Control"]], levels(crashDataTestWithout[["Traffic_Control"]]))
modelInjury$xlevels[["Road_Configuration"]] <- union(modelInjury$xlevels[["Road_Configuration"]], levels(crashDataTestWithout[["Road_Configuration"]]))
modelInjury$xlevels[["Road_Conditions"]] <- union(modelInjury$xlevels[["Road_Conditions"]], levels(crashDataTestWithout[["Road_Conditions"]]))
modelInjury$xlevels[["Vehicle.Type"]] <- union(modelInjury$xlevels[["Vehicle.Type"]], levels(crashDataTestWithout[["Vehicle.Type"]]))

summary(modelInjury)
summary(modelFatality)

#Test the prediction model
crashDataTestWithout$Weather = as.factor(crashDataTestWithout$Weather)
crashDataTestWithout$Traffic_Control = as.factor(crashDataTestWithout$Traffic_Control)
crashDataTestWithout$Road_Configuration = as.factor(crashDataTestWithout$Road_Configuration)
crashDataTestWithout$Road_Conditions = as.factor(crashDataTestWithout$Road_Conditions)
crashDataTestWithout$Light_Condition = as.factor(crashDataTestWithout$Light_Condition)
crashDataTestWithout$Vehicle.Type = as.factor(crashDataTestWithout$Vehicle.Type)



testPredFatality = predict(modelFatality, crashDataTestWithout)
testPredInjury = predict(modelInjury, crashDataTestWithout)

regr.error(testPredFatality,crashDataTest$Fatality)
regr.error(testPredInjury,crashDataTest$Injury)

#==============================
#The End
#==============================
