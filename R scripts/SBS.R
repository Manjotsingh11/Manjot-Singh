
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
dataSBS <- data[data$PRODUCT == "Side by Side",c(3,5)]

plot(dataSBS$INDUSTRY_UNITS,dataSBS$WEEK)
#converting the dataframe to timeseries
dataSBS_ts <- ts(dataSBS$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(dataSBS_ts)

#plot the time series
ts.plot(diff(dataSBS_ts))

#Augmented Dickey-Fuller Test for given series
adf.test(dataSBS_ts)

#acf graph for given series
acf(dataSBS_ts)

#pacf graph for given series
pacf(dataSBS_ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(dataSBS_ts))

#acf graph after differencing
acf(diff(dataSBS_ts))
#pacf graph after differencing
pacf(diff(dataSBS_ts))

#fitting ARIMA model
fitSBS <- arima(dataSBS_ts, c(0,1,1),seasonal = list(order = c(0,1,0), period = 54))

#predicting for next 2 quarters (27 weeks)
predSBS <- predict(fitSBS, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 1157769

#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predSBS$pred[1:13]) + predSBS$pred[14]/2
predicted2 <- sum(predSBS$pred[15:27]) + predSBS$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#89.42075