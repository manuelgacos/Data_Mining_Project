library(forecast)
library(fpp2)
library(xts)

setwd('/home/noble_mannu/Documents/PhD/Third/STAT_2270_Data_Mining/Project/Analysis')

df <- read.csv('data.csv')
df <- df [, -c(1,5)]

data <- df$USD.Peso.mexicanoFIX

# train <- data[1:1195] # Data up to July 2020
train <- data[521:781] # Data up to 2018
# test <- data[1195:length(data)] # Data up to Oct 2020
test <- data[782:804] # Data up to Dec 2019

bound.hidden <- function(inputs){
  temp.hidden <- floor(999/(2+inputs))
  if(temp.hidden >= inputs){
    ifelse(temp.hidden >= 2*inputs, 2*inputs, inputs)
  } else if( temp.hidden < inputs)
    temp.hidden
}

rmse.size <- function(size, data = NULL, p = NULL, P = NULL, 
                      repeats = NULL, h = NULL, test = NULL, xreg = NULL){
  fit <- nnetar(y = data, p = p, P = P, size = size, repeats = repeats, xreg = xreg)
  yhat <- forecast(fit, h = h)
  rmse <- sqrt(mean( (test - yhat$mean)^2 ))
  return(rmse)
}

rmse.hidden <- function(data = NULL, inputs = 1, P = 0, repeats = 20, h = 1, 
                        test = NULL, xreg = NULL){
  usable <- bound.hidden(inputs)
  output <- sapply(1:usable, rmse.size, data = data, p = inputs, P = P, 
                   repeats = repeats, h = h, test = test, xreg = xreg)
  return(output)
}

rmse.repeat <- function(iterations, data = NULL, inputs = 1, P = 0, repeats = 20,
                        h = 1, test = NULL, xreg = NULL){
  empty_list <- list()
  for(i in 1:iterations){
    rmse <- rmse.hidden(data = data, inputs = inputs, P = P, repeats = repeats, 
                        h = h, test = test, xreg = xreg)
    empty_list[[i]] <- rmse
  }
  do.call(rbind,empty_list)
}

# data <- ts(data, frequency = 260)

train <- ts(train, frequency = 261)
fit.arima <- auto.arima(train, max.p = 10, max.P = 0, lambda = NULL)
fit.arima <- Arima(y = train, order = c(1,1,1))
yhat <- forecast(fit.arima, h = 10)
rmse <- sqrt(mean( (test[1:15] - yhat$mean)^2 )) # This is the metric I'm goinf to use
plot(forecast(fit.arima, h = 10))

acf(train)
pacf(train)
decompose(data)

fit1 <- nnetar(train, p = 10, P = 0, size = 20, repeats = 20)
yhat <- forecast(fit1, h = 22)
rmse <- sqrt(mean( (test - yhat$mean)^2 )) # This is the metric I'm goinf to use
#fcast <- forecast(fit1, h=22, PI = TRUE)
autoplot( forecast(fit1, h=22) )

for(i in 1:20){
  temp <- rmse.repeat( iterations = 10, data = train, inputs = i, repeats = 20,
                       h = 208, test = test)
  assign(paste0('rmse',i), temp)
}
