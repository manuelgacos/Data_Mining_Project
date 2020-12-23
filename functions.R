require(forecast)
require(fpp2)
require(randomForest)

# bound.hidden() Computes a bound for the 'size' of the hidden layer on the neural
#   network given a number of lagged observations 'inputs'
bound.hidden <- function(inputs){
  temp.hidden <- floor(999/(2+inputs))
  if(temp.hidden >= inputs){
    ifelse(temp.hidden >= 2*inputs, 2*inputs, inputs)
  } else if( temp.hidden < inputs)
    temp.hidden
}

# rmse.size() fits a neural network using nnetar() from the forecast package and
#   computes the MAPE associated to it given a 'test' set. 'h' denotes the length
#   of the test set. All other parameters are as defined for nnetar(). Returns the
#   MAPE of the model.
#   NOTE: This function is designed to made possible to iterate over the 'size'
rmse.size <- function(size, data = NULL, p = NULL, P = NULL, 
                      repeats = NULL, h = NULL, test = NULL, xreg = NULL){
  fit <- nnetar(y = data, p = p, P = P, size = size, repeats = repeats, xreg = xreg)
  yhat <- forecast(fit, h = h)
  rmse <- sqrt(mean( (test - yhat$mean)^2 ))
  return(rmse)
}

# rmse.hidden() For a given number of lagged entries 'p', computes all neural 
#   networks with 'size' 1 to the upper bound defined by the bound.hidden()
#   function. Returns the MAPE of the neural network for each possible size as
#   a vector
rmse.hidden <- function(data = NULL, inputs = 1, P = 0, repeats = 20, h = 1, 
                        test = NULL, xreg = NULL){
  usable <- bound.hidden(inputs)
  output <- sapply(1:usable, rmse.size, data = data, p = inputs, P = P, 
                   repeats = repeats, h = h, test = test, xreg = xreg)
  return(output)
}

# remse.repeat() executes rmse.hidden() as many times as indicated by 'iterations'
#   Returns the computed errors as a matrix. Each row is the result of each
#   iteration of  rmse.hidden()
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

# mape() For a given 'model' and 'test' set computes and returns the MAPE of the
#   model. 'h' is the length of the test set.
mape <- function(model, h = 23, test = NULL){
  yhat <- forecast(model, h = h)
  output <- mean( abs(100*(test - yhat$mean)/test) )
  return(output)
}

# mape.random() Fits a random forest using a dataset 'data' with lagged entries as
#   features. For a given number of past entries 'lag' and a 'test' set, returns 
#   the MAPE of the the random forest.
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

lagged.random <- function(lag, data = NULL){
  past <- lag+2
  df.lag <- data[,2:past]
  train.for <- df.lag[261:1042,]
  test.for <- as.data.frame(df.lag[1043:1065,2:ncol(df.lag)])
  if(lag == 1){
    names(test.for) <- 'Lag1'
  }
  forest <- randomForest(as.formula( paste0('USD.Peso.mexicanoFIX','~','.') ),
                         data = train.for, ntree = 500)
  return( list('model' = forest, 'train' = test.for) )
}
