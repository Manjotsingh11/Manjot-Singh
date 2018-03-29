
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
dataFD <- data[data$PRODUCT == "French Door",c(3,5)]

plot(dataFD$INDUSTRY_UNITS,dataFD$WEEK)
#converting the dataframe to timeseries
dataFD_Ts <- ts(dataFD$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(dataFD_Ts)

#plot the time series
ts.plot(diff(dataFD_Ts))

#Augmented Dickey-Fuller Test for given series
adf.test(dataFD_Ts)

#acf graph for given series
acf(dataFD_Ts)

#pacf graph for given series
pacf(dataFD_Ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(dataFD_Ts))

#acf graph after differencing
acf(diff(dataFD_Ts))
#pacf graph after differencing
pacf(diff(dataFD_Ts))

#fitting ARIMA model
fit_FD <- arima(dataFD_Ts, c(0,0,1),seasonal = list(order = c(0,1,0), period = 54))

#predicting for next 2 quarters (27 weeks)
predFD <- predict(fit_FD, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 1587699


#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predFD$pred[1:13]) + predFD$pred[14]/2
predicted2 <- sum(predFD$pred[15:27]) + predFD$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#82.75347

