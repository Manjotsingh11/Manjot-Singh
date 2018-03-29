
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
dataFrontLoad <- data[data$PRODUCT == "Front Load",c(3,5)]

plot(dataFrontLoad$INDUSTRY_UNITS,dataFrontLoad$WEEK)
#converting the dataframe to timeseries
dataFrontLoad_ts <- ts(dataFrontLoad$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(dataFrontLoad_ts)

#plot the time series
ts.plot(diff(dataFrontLoad_ts))

#Augmented Dickey-Fuller Test for given series
adf.test(dataFrontLoad_ts)

#acf graph for given series
acf(dataFrontLoad_ts)

#pacf graph for given series
pacf(dataFrontLoad_ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(dataFrontLoad_ts))

#acf graph after differencing
acf(diff(dataFrontLoad_ts))

#pacf graph after differencing
pacf(diff(dataFrontLoad_ts))

#fitting ARIMA model
fitFrontLoad <- arima(dataFrontLoad_ts, c(2,0,1),seasonal = list(order = c(1, 1, 1), period = 54))

#predicting for next 2 quarters (27 weeks)
predFrontLoad <- predict(fitFrontLoad, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 1093970




#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predFrontLoad$pred[1:13]) + predFrontLoad$pred[14]/2
predicted2 <- sum(predFrontLoad$pred[15:27]) + predFrontLoad$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs((actual-predicted1-predicted2)*100)/actual -100

