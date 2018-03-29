
data=read.csv("shipment_Data.csv")
class(data)


#slicing time (year-week) and units alone
data_dishwasher <- data[data$PRODUCT == "Dishwasher",c(3,5)]

plot(data_dishwasher$INDUSTRY_UNITS,data_dishwasher$WEEK)
#converting the dataframe to timeseries
data_dishwasher_Ts <- ts(data_dishwasher$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(data_dishwasher_Ts)

#plot the time series
ts.plot(diff(data_dishwasher_Ts))

#Augmented Dickey-Fuller Test for given series
adf.test(data_dishwasher_Ts)

#acf graph for given series
acf(data_dishwasher_Ts)

#pacf graph for given series
pacf(data_dishwasher_Ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(data_dishwasher_Ts))

#acf graph after differencing
acf(diff(data_dishwasher_Ts))

#pacf graph after differencing
pacf(diff(data_dishwasher_Ts))
#fitting ARIMA model
fit_dishwasher <- arima(data_dishwasher_Ts, c(0,0,1),seasonal = list(order = c(1, 1, 0), period = 54))

#predicting for next 2 quarters (27 weeks)
pred_dishwasher <- predict(fit_dishwasher, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 3698727


#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(pred_dishwasher$pred[1:13]) + pred_dishwasher$pred[14]/2
predicted2 <- sum(pred_dishwasher$pred[15:27]) + pred_dishwasher$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#accuracy=93.74107
#plotting the graph along with forecated value for Q3 and Q4 2015
ts.plot(data_dishwasher_Ts, pred_dishwasher$pred, lty = c(1,3))
