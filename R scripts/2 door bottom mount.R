
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
data_2door <- data[data$PRODUCT == "2 Door Bottom Mount",c(3,5)]

plot(data_2door$WEEK,data_2door$INDUSTRY_UNITS)

#converting the dataframe to timeseries

data_2door_ts <- ts(data_2door$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)
class(data_2door_ts)


#plot the time series
ts.plot(diff(data_2door_ts))

#Augmented Dickey-Fuller Test for given series
library(tseries)
adf.test(data_2door_ts)



#acf graph for given series
acf(data_2door_ts)
#acf(diff(data_2door_ts))
#pacf graph for given series
pacf(data_2door_ts)
#pacf(diff(data_2door_ts))
#Augmented Dickey-Fuller Test after differencing
adf.test(diff(data_2door_ts))

#acf graph after differencing
acf(diff(data_2door_ts))

#pacf graph after differencing
pacf(diff(data_2door_ts))

#fitting ARIMA model
fit_2door <- arima(data_2door_ts, c(2,1,0),seasonal = list(order = c(0, 0, 0), period = 54))

#predicting for next 2 quarters (27 weeks)
pred_2door <- predict(fit_2door, n.ahead=27)
pred_2door
#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 231379

#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(pred_2door$pred[1:13]) + pred_2door$pred[14]/2
predicted2 <- sum(pred_2door$pred[15:27]) + pred_2door$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#accuracy=95.70197 %


#plotting the graph along with forecated value for Q3 and Q4 2015
ts.plot(data_2door_ts, pred_2door$pred, lty = c(1,3))
ts.plot(data_2door_ts,2.718^pred_2door$pred, log = "y", lty = c(1,3))


