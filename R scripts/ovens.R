
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
data_ovens <- data[data$PRODUCT == "Built-in Ovens",c(3,5)]

plot(data_ovens$WEEK,data_ovens$INDUSTRY_UNITS)

#converting the dataframe to timeseries
data_ovens_ts <- ts(data_ovens$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)
class(data_ovens_ts)

#plot the time series
ts.plot(diff(data_ovens_ts))

#Augmented Dickey-Fuller Test for given series
library(tseries)
adf.test(data_ovens_ts)


#acf graph for given series
acf(data_ovens_ts)

#pacf graph for given series
pacf(data_ovens_ts)


#Augmented Dickey-Fuller Test after differencing
adf.test(diff(data_ovens_ts))

#acf graph after differencing
acf(diff(data_ovens_ts))

#pacf graph after differencing
pacf(diff(data_ovens_ts))

#fitting ARIMA model
fit_ovens <- arima(data_ovens_ts, c(0,1,1),seasonal = list(order = c(0, 1, 1), period = 54))

#predicting for next 2 quarters (26 weeks)
pred_oven <- predict(fit_ovens, n.ahead=27)
pred_oven

actual <- 405977



#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(pred_oven$pred[1:13]) + pred_oven$pred[14]/2
predicted2 <- sum(pred_oven$pred[15:27]) + pred_oven$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#accuracy=95.18533

