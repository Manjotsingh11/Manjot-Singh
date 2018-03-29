
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
dataFSR <- data[data$PRODUCT == "Free Standing Ranges",c(3,5)]

plot(dataFSR$INDUSTRY_UNITS,dataFSR$WEEK)
#converting the dataframe to timeseries
FSRTs <- ts(dataFSR$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(FSRTs)

#plot the time series
ts.plot(diff(FSRTs))

#Augmented Dickey-Fuller Test for given series
adf.test(FSRTs)

#acf graph for given series
acf(FSRTs)

#pacf graph for given series
pacf(FSRTs)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(FSRTs))

#acf/pacf graph after differencing
acf(diff(FSRTs))


#pacf graph after differencing
pacf(diff(FSRTs))

#fitting ARIMA model
fitFSR <- arima(FSRTs, c(0,0,1),seasonal = list(order = c(1, 1, 0), period = 54))

#predicting for next 2 quarters (27 weeks)
predFSR <- predict(fitFSR, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 3627445


#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predFSR$pred[1:13]) + predFSR$pred[14]/2
predicted2 <- sum(predFSR$pred[15:27]) + predFSR$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#95.15735

