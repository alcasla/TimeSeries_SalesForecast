#Data reading as vector
serie<-scan("data/product-GuardoToIto.dat")
serie = serie/max(serie)


###### Temporal serie analysis #######
#Library specialized to work with temporal series
require(tseries)

#create specific object, and show basics
plot.ts(serie, col="blue")
plot(decompose(ts(serie, frequency=12)))

#divide serie into train and test
#Train
serieTr = serie[1:(length(serie)-12)]
timeTr = 1:(length(serie)-12)
#Test
serieTs = serie[(length(serie)-12+1):length(serie)]
timeTs = (length(serie)-12+1):length(serie)



#------------------- Tendency --------------------#
#*********** Linear functional approach ***********#
#adjust linar model
parameters <- lm(serieTr ~ timeTr)

#Compute tendency stimation
tendTr = parameters$coefficients[1]+timeTr*parameters$coefficients[2]
tendTs = parameters$coefficients[1]+timeTs*parameters$coefficients[2] 

#show result
plot.ts(serieTr, xlim=c(1, timeTs[length(timeTs)]))
lines(timeTr, tendTr, col="red")
lines(timeTs, serieTs, col="blue")
lines(timeTs, tendTs, col="red")

#check linear theory, data normality
jarque.bera.test(parameters$residuals)
jarque.bera.test(tendTs-serieTs)

t.test(c(parameters$residuals, tendTs-serieTs))

message('Due to values obtained in tests I can affirm normality');


#*************** Moving averages approach ***********#
#Tendency stimation through moving averages with order k
for (k in 3:5) {
  filtro<-rep(1/k, k);    #filter
  
  # filter signal
  SerFil<-filter(serie, filter=filtro, sides=2, method="convolution")
  
  # Show serie and estimate tendency
  series<-matrix(c(t(serie), t(SerFil)), ncol=2);
  matplot(series, pch=2, type= "l")
  
  cat("Tendency filtered with order filter k=", k, "\n")
  pause<-readline();  #press key to continue
}

#serie without tendency - filter
serieFilT = serie - SerFil
plot.ts(serieFilT, col="blue")


#******** Conclusion
message('There aren´t tendency in serie, both tendencies are similar to flat line without incline, also 
        linear hypothesis is accepted')
#I thing it´s not neccesary remove tendency because there aren´t



#------------------- Seasonality --------------------#
ts = ts(serie, frequency=12)
seasonality = decompose(ts)

#show seasonality
plot(seasonality$seasonal)

acf(ts)    #help us to compute season length

#remove seasonality
seasonality = seasonality$seasonal[1:12]
aux = rep(seasonality, length(serieTr)/length(seasonality))

serie.SeasonalityTr = serieTr - aux
serie.SeasonalityTs = serieTs - seasonality

plot.ts(serie.SeasonalityTr, xlim=c(1,timeTs[length(timeTs)]))
lines(timeTs, serie.SeasonalityTs, col="blue")



#------------------- Stationarity --------------------#
#check with augmented Dickey-Fuller test
adf.test(serie.SeasonalityTr)

#visual tationarity checking
acf(serie.SeasonalityTr)      #serie tend quickly to 0 -> stationary, and fail Dickey-Fuller test
pacf(serie.SeasonalityTr)



#------------------- ARIMA model --------------------#
#********** Hipothesis 1 ***********#
model.H1 = arima(serie.SeasonalityTr, order=c(1,0,0))
adjustedValues.H1 = serie.SeasonalityTr + model.H1$residuals

#compute predictions
predictions.H1 = predict(model.H1, n.ahead=12)
predictValues.H1 = predictions.H1$pred

#check error 
sum(model.H1$residuals^2)
sum((predictValues.H1 - serie.SeasonalityTs)^2)

#results
plot.ts(serie.SeasonalityTr, xlim=c(1,timeTs[length(timeTs)]))
lines(adjustedValues.H1, col="lightblue")
lines(timeTs, serie.SeasonalityTs, col="red")
lines(timeTs, predictValues.H1, col="blue")

#check model
Box.test(model.H1$residuals)
jarque.bera.test(model.H1$residuals)
shapiro.test(model.H1$residuals)


#********** Hipothesis 2 ***********#
model.H2 = arima(serie.SeasonalityTr, order=c(1,0,1))
adjustedValues.H2 = serie.SeasonalityTr + model.H2$residuals

#compute predictions
predictions.H2 = predict(model.H2, n.ahead=12)
predictValues.H2 = predictions.H2$pred

#check error 
sum(model.H2$residuals^2)
sum((predictValues.H2 - serie.SeasonalityTs)^2)

#results
plot.ts(serie.SeasonalityTr, xlim=c(1,timeTs[length(timeTs)]))
lines(adjustedValues.H2, col="lightblue")
lines(timeTs, serie.SeasonalityTs, col="red")
lines(timeTs, predictValues.H2, col="blue")

#check model
Box.test(model.H2$residuals)
jarque.bera.test(model.H2$residuals)
shapiro.test(model.H2$residuals)


#********** Hipothesis 3 ***********#
model.H3 = arima(serie.SeasonalityTr, order=c(1,0,2))
adjustedValues.H3 = serie.SeasonalityTr + model.H3$residuals

#compute predictions
predictions.H3 = predict(model.H3, n.ahead=12)
predictValues.H3 = predictions.H3$pred

#check error 
sum(model.H3$residuals^2)
sum((predictValues.H3 - serie.SeasonalityTs)^2)

#results
plot.ts(serie.SeasonalityTr, xlim=c(1,timeTs[length(timeTs)]))
lines(adjustedValues.H3, col="lightblue")
lines(timeTs, serie.SeasonalityTs, col="red")
lines(timeTs, predictValues.H3, col="blue")

#check model
Box.test(model.H3$residuals)
jarque.bera.test(model.H3$residuals)
shapiro.test(model.H3$residuals)

#histogram and density function
hist(model.H3$residuals, col="blue", prob=T, ylim=c(0,6), xlim=c(-0.4,0.4))
lines(density(model.H3$residuals), col="red")


#------------------- Model selection --------------------#
#Base decison on AIC rate. There aren´t significant differences
AIC(model.H1, model.H2, model.H3)

