
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
dataMHC <- data[data$PRODUCT == "MHC",c(3,5)]

plot(dataMHC$INDUSTRY_UNITS,dataMHC$WEEK)
#converting the dataframe to timeseries
dataMHC_ts <- ts(dataMHC$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(dataMHC_ts)

#plot the time series
ts.plot(diff(dataMHC_ts))

#Augmented Dickey-Fuller Test for given series
adf.test(dataMHC_ts)

#acf graph for given series
acf(dataMHC_ts)

#pacf graph for given series
pacf(dataMHC_ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(dataMHC_ts))

#acf graph after differencing
acf(diff(dataMHC_ts))
#pacf graph after differencing
pacf(diff(dataMHC_ts))

#fitting ARIMA model
fitMHC <- arima(dataMHC_ts, c(0,0,1),seasonal = list(order = c(0,1,0), period = 54))

#predicting for next 2 quarters (27 weeks)
predMHC <- predict(fitMHC, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 2388377

#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predMHC$pred[1:13]) + predMHC$pred[14]/2
predicted2 <- sum(predMHC$pred[15:27]) + predMHC$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#86.83588
