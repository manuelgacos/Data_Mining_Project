library(xts)
library(ggplot2)

setwd('/home/noble_mannu/Documents/PhD/Third/STAT_2270_Data_Mining/Project/Analysis')
source('/home/noble_mannu/Documents/PhD/Third/STAT_2270_Data_Mining/Project/Analysis/functions.R')

df <- read.csv('data.csv')

data <- df$USD.Peso.mexicanoFIX

train <- ts(data[261:1042], frequency = 260) # Data up to 2019
test <- data[1043:1065] # Data Jan 2020

# The following code trained the neural networks. The results of this code are 
#   stored in the 'results.RData' file.

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

# Here I perform the analysis of the results of ANN
#   NOTE: The result is also stored in the file 'results.RData'.

all.models <- c(empty_list1,empty_list2,empty_list3,empty_list4)
  
precision <- list()
for(i in 1:20){
  models <- all.models[[i]]
  temp <- sapply(models, mape, h = 23, test = test)
  precision[[i]] <- temp
}

load('results.RData')

all.models <- c(empty_list1,empty_list2,empty_list3,empty_list4)

errors <- sapply(precision, min)
which.min(errors)
which.min(precision[[5]])

mape(all.models[[5]][[9]], test = test)

# This is the best neural network
fit <- all.models[[5]][[9]]
yhat <- forecast(fit, h = 23)
mape <- mean( abs(100*(test - yhat$mean)/test) )
fcast <- forecast(fit, h=23)

# Plotting the forecast from the best neural network
autoplot( forecast(fit, h = 23, PI = TRUE) ) + 
  labs( title = 'Forecast from NNAR (5,9)', 
        x = 'Time', y = 'Exchange rate') + 
  theme(plot.title = element_text(hjust = 0.5))

# This is the analysis for Random forest
df.lag <- read.csv('data_lag.csv')

set.seed(1991)
errors.for <- sapply(1:15, mape.random, data = df.lag, test = test)
which.min(errors.for)

set.seed(1991)
mape.random( data =  df.lag, lag = 12, test = test)

# Plotting the best random forest

# Getting predictions for random forest with 5 and 12 lagged entries
set.seed(1991)
forest <- lagged.random( data =  df.lag, lag = 5)
Lag5 <- predict(forest$model, newdata = forest$train)

set.seed(1991)
forest <- lagged.random( data =  df.lag, lag = 12)
Lag12 <- predict(forest$model, newdata = forest$train)

x <- 783:805
series.for <- as.data.frame( cbind(x,Lag5,Lag12) )

y <- data[261:1065]
x <- 1:805
series <- as.data.frame(cbind(y,x) )
# Plotting prediction of random forest with 
ggplot() + geom_line(data = series, aes(x=x, y=y) ) +
  geom_line(data = series.for, aes(x=x, y = Lag5, colour = 'RF5') ) +
  labs( title = 'Forecast from RF with 5 lagged steps', 
        x = 'Time', y = 'Exchange rate', color = 'Lag. RF') + 
  scale_x_continuous(breaks=c(0,260,521,782),
                     labels=c("2017", "2018", "2019",'2020')) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() + geom_line(data = series, aes(x=x, y=y) ) +
  geom_line(data = series.for, aes(x=x, y = Lag12, colour = 'RF12') ) +
  labs( title = 'Forecast from RF with 12 lagged steps', 
        x = 'Time', y = 'Exchange rate', color = 'Lag. RF') + 
  scale_x_continuous(breaks=c(0,260,521,782),
                     labels=c("2017", "2018", "2019",'2020')) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() + geom_line(data = series, aes(x=x, y=y) ) +
  geom_line(data = series.for, aes(x=x, y = Lag5, colour = 'RF5') ) +
  geom_line(data = series.for, aes(x=x, y = Lag12, colour = 'RF12') ) +
  labs( title = 'Forecast from RF 5/12 lagged steps', 
        x = 'Time', y = 'Exchange rate', color = 'Lag. RF') + 
  scale_x_continuous(breaks=c(0,260,521,782),
                     labels=c("2017", "2018", "2019",'2020')) +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting the ARIMA model

fit.arima <- Arima(train, order = c(2,1,0))

fcast.arima <- forecast(fit.arima, h = 23)

autoplot(fcast.arima) + 
  labs( title = 'Forecast from ARIMA(2,1,0)', 
      x = 'Time', y = 'Exchange rate') + 
  theme(plot.title = element_text(hjust = 0.5))

mean(abs(100*(test - fcast.arima$mean)/test))
