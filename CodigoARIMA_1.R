library(forecast)
library(fpp2)
library(xts)

library(readr)
library(tseries)


setwd('/home/noble_mannu/Documents/PhD/Third/STAT_2270_Data_Mining/Project/Analysis')
# setwd('C:/Users/Mitzi/Downloads')


df <- read.csv('data.csv')


data <- df$USD.Peso.mexicanoFIX


train <- data[261:1042] # Data up to 2019
train <- ts(train, frequency = 260)

test <- data[1043:1065] # Data up to Oct 2020

#El auto arima
#fit.arima <- auto.arima(train,lambda = NULL)
fit.arima <- Arima(train, order = c(2,1,0))

prono1<- forecast(fit.arima, h = 23)

plot(prono1)

mean(abs(100*(test - prono1$mean)/test))



