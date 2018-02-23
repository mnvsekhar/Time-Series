library("forecast")
library("stats")
library("dplyr")
library("imputeTS")
library("arima")

rm(list=ls(all=TRUE))

#Reading the CSV file

setwd("C:/Users/Sekhar/OneDrive/INSOFE/PHD/TS/")

TS_Original = read.csv("Train.csv")

#Filtering the "WomenClothing" data

TS_Women <- TS_Original[TS_Original$ProductCategory == 'WomenClothing',]

#Filtering the Sales Column alone

TS_Women <- TS_Women[,4,drop=FALSE]

#Transforming the data into the Time Series model.

timeseriesWomen <- ts(TS_Women, frequency = 12, start = c(2009,1))

#Checking the values in the above created Time Series model

timeseriesWomen

statsNA(timeseriesWomen)

#Basic plot of the above created Time Series for a high level picture

plot(timeseriesWomen)

#Ploting the NA distribution

plotNA.distribution(timeseriesWomen)

#NAs imputation using Interpolation and graph visualization

TS_InterpolateWomen = na.interpolation(timeseriesWomen)

plotNA.distribution(timeseriesWomen)
plot(TS_InterpolateWomen)

#Women Clothing prediction using HoltWinter, seasonal = "additive"

tsHWWomen <- HoltWinters(TS_InterpolateWomen, seasonal = "additive")

plot(tsHWWomen)

tsHoltPredictWomen <- predict(tsHWWomen, n.ahead = 12, prediction.interval = TRUE)

tsHoltPredictWomen

plot(tsHoltPredictWomen)

plot(forecast(tsHWWomen,12))

aArimaWomen <- auto.arima(TS_InterpolateWomen,ic='aic')
aArimaWomenForecast <- forecast(aArimaWomen, h=12)
aArimaWomenForecast

#Timeseries prediction for "MenClothing"

TS_Men <- TS_Original[TS_Original$ProductCategory == 'MenClothing',]
TS_Men <- TS_Men[,4,drop=FALSE]
timeseriesMen <- ts(TS_Men, frequency = 12, start = c(2009,1))
timeseriesMen
statsNA(timeseriesMen)
plot(timeseriesMen)
plotNA.distribution(timeseriesMen)
TS_InterpolateMen = na.interpolation(timeseriesMen)
plotNA.distribution(timeseriesMen)
plot(TS_InterpolateMen)
tsHWMen <- HoltWinters(TS_InterpolateMen, seasonal = "additive")
plot(tsHWMen)
tsHoltPredictMen <- predict(tsHWMen, n.ahead = 12, prediction.interval = TRUE)
tsHoltPredictMen
plot(tsHoltPredictMen)
plot(forecast(tsHWMen,12))

aArimaMen <- auto.arima(TS_InterpolateMen,ic='aic')
aArimaMenForecast <- forecast(aArimaMen, h=12)
aArimaMenForecast

#Timeseries prediction for "OtherClothing"

TS_Oth <- TS_Original[TS_Original$ProductCategory == 'OtherClothing',]
TS_Oth <- TS_Oth[,4,drop=FALSE]
timeseriesOth <- ts(TS_Oth, frequency = 12, start = c(2009,1))
timeseriesOth
statsNA(timeseriesOth)
plot(timeseriesOth)
plotNA.distribution(timeseriesOth)
TS_InterpolateOth = na.interpolation(timeseriesOth)
plotNA.distribution(timeseriesOth)
plot(TS_InterpolateOth)
tsHWOth <- HoltWinters(TS_InterpolateOth, seasonal = "additive")
plot(tsHWOth)
tsHoltPredictOth <- predict(tsHWOth, n.ahead = 12, prediction.interval = TRUE)
tsHoltPredictOth
plot(tsHoltPredictOth)
plot(forecast(tsHWOth,12))

aArimaOth <- auto.arima(TS_InterpolateOth,ic='aic')
aArimaOthForecast <- forecast(aArimaOth, h=12)
aArimaOthForecast