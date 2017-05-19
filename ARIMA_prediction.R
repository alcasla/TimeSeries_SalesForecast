#****** Alberto Castillo Lamas - 15472955G ********
#****** alcasla90@gmail.com ***********************
#****** Ejercicio de trabajo autónomo. Series temporales. Curso 2016-2017 ********

#----------------------------------------------------------------------------------
#***** DEMO *****
# Alocate correctly data file, and before perform 'predictTS' funtion run it
#****************

#Data reading as vector
serie<-scan("data/product-GuardoToIto.dat")

predictTS(serie, 6, 12)

#----------------------------------------------------------------------------------


#**** Function predict for 'serie', a number of predictions 'numPredictions', and 
# the season has 'frequency' observations ******
predictTS = function(serie, numPredictions, frequency)
{
  #Library specialized to work with temporal series
  require(tseries)
  
  #model variable to time
  time = 1:length(serie)
  
  #I musn´t to remove trend, it´s marginal
  
  #***** Seasonality *******
  p=frequency    #period for a year
  ts = ts(serie, frequency=p)
  plot(decompose(ts)$seasonal)
  
  #get a season and repeat
  seasonality = decompose(ts)$seasonal
  seasonality = as.numeric(seasonality[1:p])
  seasonality = rep(seasonality, length(serie)/p)
  
  #remove seasonality
  serieSeason = serie - seasonality
  plot.ts(serieSeason, col="blue")
  
  
  #ARIMA model
  model = arima(serieSeason, order=c(1,0,2))
  adjustedValues = serieSeason + model$residuals
  predictions = predict(model, n.ahead=numPredictions)   #predict n future months
  predictions = predictions$pred
  
  #break up changes
  adjustedValues = adjustedValues + seasonality
  predictions = predictions + seasonality[1:length(predictions)]    #apply seasonality
  
  
  #plot known data together prediction, looks good
  plot.ts(serie, xlim=c(1, time[length(time)]+p))
  lines(adjustedValues, col="lightblue")
  lines(predictions, col="blue")
  message('Plot result: line black represents data from original serie, light blue line represents adjusted values for this data, and deep blue line is future 6 month prediction')
  
  return(predictions)
}
