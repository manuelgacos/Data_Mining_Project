library(forecast)
library(fpp2)
library(xts)
library(randomForest)

#setwd('/home/noble_mannu/Documents/PhD/Third/STAT_2270_Data_Mining/Project/Analysis')

df <- read.csv('data.csv')
#df <- df [, -c(1,5)]

data <- df$USD.Peso.mexicanoFIX

# train <- data[1:1195] # Data up to July 2020
train <- ts(data[261:1042], frequency = 260) # Data up to 2019
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

mape <- function(model, h = 23, test = NULL){
  yhat <- forecast(model, h = h)
  output <- mean( abs(100*(test - yhat$mean)/test) )
  return(output)
}

mape.random <- function(lag, data = NULL, test = NULL){
  past <- lag+2
  df.lag <- data[,2:past]
  train.for <- df.lag[261:1042,]
  test.for <- as.data.frame(df.lag[1043:1065,2:ncol(df.lag)])
  if(lag == 1){
    names(test.for) <- 'Lag1'
  }
  forest <- randomForest(as.formula( paste0('USD.Peso.mexicanoFIX','~','.') ),
                         data = train.for, ntree = 500)
  yhat.for <- predict(forest, newdata = test.for)
  output <- mean( abs(100*(test - yhat.for)/test) )
  return( output )
}

empty_list1 <- list()
set.seed(1991)
for(i in 1:5){
  model <- list()
  k <- bound.hidden(i)
  for (j in 1:k) {
    temp <- nnetar(train, p = i, size = j, repeats = 20, lambda = NULL)
    model[[j]] <- temp
  }
  empty_list1[[i]] <- model
}

empty_list2 <- list()
set.seed(1991)
for(i in 6:10){
  model <- list()
  k <- bound.hidden(i)
  for (j in 1:k) {
    temp <- nnetar(train, p = i, size = j, repeats = 20, lambda = NULL)
    model[[j]] <- temp
  }
  empty_list2[[i]] <- model
}

empty_list2 <- list()
set.seed(1991)
for(i in 6:10){
  model <- list()
  k <- bound.hidden(i)
  for (j in 1:k) {
    temp <- nnetar(train, p = i, size = j, repeats = 20, lambda = NULL)
    model[[j]] <- temp
  }
  empty_list2[[i]] <- model
}

empty_list3 <- list()
set.seed(1991)
for(i in 11:15){
  model <- list()
  k <- bound.hidden(i)
  for (j in 1:k) {
    temp <- nnetar(train, p = i, size = j, repeats = 20, lambda = NULL)
    model[[j]] <- temp
  }
  empty_list3[[i]] <- model
}

empty_list4 <- list()
set.seed(1991)
for(i in 16:20){
  model <- list()
  k <- bound.hidden(i)
  for (j in 1:k) {
    temp <- nnetar(train, p = i, size = j, repeats = 20, lambda = NULL)
    model[[j]] <- temp
  }
  empty_list4[[i]] <- model
}

# Analysis of results of ANN

load('models1.RData')

all.models <- c(empty_list1,empty_list2,empty_list3,empty_list4)
  
precision <- list()
for(i in 1:20){
  models <- all.models[[i]]
  temp <- sapply(models, mape, h = 23, test = test)
  precision[[i]] <- temp
}

errors <- sapply(precision, min)
which.min(errors)
which.min(precision[[5]])

mape(all.models[[5]][[9]], test = test)

# This is the best neural network
set.seed(1991)
fit <- all.models[[5]][[9]]
yhat <- forecast(fit, h = 23)
mape <- mean( abs(100*(test - yhat$mean)/test) )
fcast <- forecast(fit, h=23)
autoplot( forecast(fit, h = 23, PI = FALSE) )

# Random forest

df.lag <- read.csv('data_lag.csv')

set.seed(1991)
errors.for <- sapply(1:15, mape.random, data = df.lag, test = test)
which.min(errors.for)

set.seed(1991)
mape.random( data =  df.lag, lag = 12, test = test)
