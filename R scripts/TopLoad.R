
data=read.csv("shipment_Data.csv")
class(data)


#slicing time (year-week) and units alone
dataTopLoad <- data[data$PRODUCT == "Top Load",c(3,5)]

plot(dataTopLoad$INDUSTRY_UNITS,dataTopLoad$WEEK)
#converting the dataframe to timeseries
dataTopLoad_ts <- ts(dataTopLoad$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(dataTopLoad_ts)

#plot the time series
ts.plot(diff(dataTopLoad_ts))

#Augmented Dickey-Fuller Test for given series
adf.test(dataTopLoad_ts)

#acf graph for given series
acf(dataTopLoad_ts)

#pacf graph for given series
pacf(dataTopLoad_ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(dataTopLoad_ts))

#acf graph after differencing
acf(diff(dataTopLoad_ts))
#pacf graph after differencing

pacf(diff(dataTopLoad_ts))

#fitting ARIMA model
fitTopLoad <- arima(dataTopLoad_ts, c(0,0,1),seasonal = list(order = c(0,1,0), period = 54))

#predicting for next 2 quarters (27 weeks)
predTopLoad <- predict(fitTopLoad, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 3438951

#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predTopLoad$pred[1:13]) + predTopLoad$pred[14]/2
predicted2 <- sum(predTopLoad$pred[15:27]) + predTopLoad$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#94.46098