#install the package for ARIMA
install.packages("forecast")
library(forecast)

#load the dataset
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)

#create a time-series model for the dataset in an object
volcanodustseries_ts <- ts(volcanodust,start=c(1500))

#Display the result for the time-series model in object "valcanodust_ts"
volcanodustseries_ts

#plot the timeseries model stored in "valcanodust_ts" 
plot(volcanodustseries_ts, xlab = "Year" , ylab = "volcano dust", main = "Time series of Valcanodust")

#create a timeseries model with frequency 2
volcanodustseries_ts_model <- ts(volcanodust,frequency =2, start=c(1500))

#Display the timeseries model having frequency 2
volcanodustseries_ts_model

#plot the timeseries model having frequency 2
plot(volcanodustseries_ts_model, xlab = "Year" , ylab = "volcano dust", main="Time sereies of valcanodust of frequency")

#decomposing the timeseries which has frequency 2
volcano_new = decompose(volcanodustseries_ts_model)

#Display the result of decomposed timeseries model having frequency 2
volcano_new

#plot the timeseries model obtained from decomposition  
plot(volcano_new, xlab = "Year" , ylab = "volcano dust")

#Seasonally adjust the time series model
seasonal_adjusted = volcanodustseries_ts_model - volcano_new$seasonal

#plot the seasonally adjusted time series model
plot(seasonal_adjusted, xlab = "Year" , ylab = "volcano dust",main="Seasonally adjusted timeseries model")

#Apply the auto-correlation on initial timeseries model where frequency was not specified
acf(volcanodustseries_ts,lag.max = 20, plot = FALSE)

#plot the correlogram for initial time series model
acf(volcanodustseries_ts,lag.max = 20)

#Apply the partial auto-correlation on initial time series model 
pacf(volcanodustseries_ts,lag.max=20, plot = FALSE)

#plot the correlogram for partial auto-correlation of initial time series model
pacf(volcanodustseries_ts,lag.max=20)

#auto-arima function  is ued to find the best arima model
auto.arima(volcanodustseries_ts, ic = "bic") 

#fitting the arima model found by auto.arima
volcanodustseries_ts_arima = arima(volcanodustseries_ts, order=c(2,0,0)) 

#view the results for arima
volcanodustseries_ts_arima

#forecasting the data for the nest 30 years
volcanodustseries_arima_forecasts = forecast(volcanodustseries_ts_arima, h= 30) 

#Display the forecasting results
volcanodustseries_arima_forecasts

#plot of forecasting data
plot(volcanodustseries_arima_forecasts,xlab = "Year" , ylab = "volcano dust",main="Forecast of Volcanodust using ARIMA")  



