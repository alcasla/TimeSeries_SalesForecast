#Library specialized to work with temporal series
require(tseries)

#Data reading as vector
serie<-scan("data/product-GuardoToIto.dat")
time = 1:length(serie)

#I musn´t to remove trend, it´s marginal

#***** Seasonality *******
p=12    #period for a year
ts = ts(serie, frequency=p)
plot(decompose(ts)$seasonal)    #obvious seasonality

seasonality = decompose(ts)$seasonal
seasonality = as.numeric(seasonality[1:p])
seasonality = rep(seasonality, length(serie)/p)

#remove seasonality
serieSeason = serie - seasonality
plot.ts(serieSeason, col="blue")


#ARIMA model
model = arima(serieSeason, order=c(1,0,2))
adjustedValues = serieSeason + model$residuals
predictions = predict(model, n.ahead=6)   #predict 6 future months
predictions = predictions$pred

#break up changes
adjustedValues = adjustedValues + seasonality
predictions = predictions + seasonality[1:length(predictions)]


#plot known data together prediction, looks good
plot.ts(serie, xlim=c(1, time[length(time)]+p))
lines(adjustedValues, col="lightblue")
lines(predictions, col="blue")
