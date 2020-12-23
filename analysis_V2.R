library(forecast)
library(fpp2)
library(xts)

#setwd('/home/noble_mannu/Documents/PhD/Third/STAT_2270_Data_Mining/Project/Analysis')

df <- read.csv('data.csv')
#df <- df [, -c(1,5)]

data <- df$USD.Peso.mexicanoFIX

# train <- data[1:1195] # Data up to July 2020
train <- data[261:1042] # Data up to 2019
# test <- data[1195:length(data)] # Data up to Oct 2020
test <- data[1043:1065] # Data up to Oct 2020

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

set.seed(1991)
for(i in 1:15){
  temp <- rmse.repeat( iterations = 10, data = train, inputs = i, repeats = 20,
                       h = 23, test = test)
  assign(paste0('rmse',i), temp)
}

load('n1.RData')
load('n2.RData')

errors10 <- colMeans(rmse10)[5:20]
errors11 <- colMeans(rmse11)[5:22]
errors12 <- colMeans(rmse12)[5:24]
errors13 <- colMeans(rmse13)[5:26]
errors14 <- colMeans(rmse14)[5:28]
errors15 <- colMeans(rmse15)[5:30]

v <- c(min(errors10), min(errors11), min(errors12), min(errors13), min(errors14),
       min(errors15))
which.min(v)

colMeans(rmse12)

which.min(errors12)

fit <- nnetar(train, p = 10, P = 0, size = 6, repeats = 20)
yhat <- forecast(fit, h = 23)
rmse <- sqrt(mean( (test - yhat$mean)^2 )) # This is the metric I'm goinf to use
mape <- mean( abs(100*(test - yhat$mean)/test) )
#fcast <- forecast(fit, h=23, PI = FALSE)
#autoplot( fcast )
autoplot( forecast(fit, h = 23) )

plot( c(train,test), type = 'l' )
