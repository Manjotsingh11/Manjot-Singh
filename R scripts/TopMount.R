
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
dataTopMount <- data[data$PRODUCT == "Top Mount",c(3,5)]

plot(dataTopMount$INDUSTRY_UNITS,dataTopMount$WEEK)
#converting the dataframe to timeseries
dataTopMount_ts <- ts(dataTopMount$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(dataTopMount_ts)

#plot the time series
ts.plot(diff(dataTopMount_ts))

#Augmented Dickey-Fuller Test for given series
adf.test(dataTopMount_ts)

#acf graph for given series
acf(dataTopMount_ts)

#pacf graph for given series
pacf(dataTopMount_ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(dataTopMount_ts))

#acf graph after differencing
acf(diff(dataTopMount_ts))
#pacf graph after differencing
pacf(diff(dataTopMount_ts))

#fitting ARIMA model
fitTopMount <- arima(dataTopMount_ts, c(1,0,1),seasonal = list(order = c(0,0,1), period = 54))

#predicting for next 2 quarters (27 weeks)
predTopMount <- predict(fitTopMount, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 2478801


#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predTopMount$pred[1:13]) + predTopMount$pred[14]/2
predicted2 <- sum(predTopMount$pred[15:27]) + predTopMount$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#95.3912