
data=read.csv("shipment_Data.csv")
class(data)

#slicing time (year-week) and units alone
data_freezer <- data[data$PRODUCT == "Freezer",c(3,5)]

plot(data_freezer$INDUSTRY_UNITS,data_freezer$WEEK)
#converting the dataframe to timeseries
data_freezer_Ts <- ts(data_freezer$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(data_freezer_Ts)

#plot the time series
ts.plot(diff(data_freezer_Ts))

#Augmented Dickey-Fuller Test for given series
adf.test(data_freezer_Ts)

#acf graph for given series
acf(data_freezer_Ts)

#pacf graph for given series
pacf(data_freezer_Ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(data_freezer_Ts))

#acf/pacf graph after differencing
acf(diff(data_freezer_Ts))

#pacf graph after differencing
pacf(diff(data_freezer_Ts))

#fitting ARIMA model
fit_freezer <- arima(data_freezer_Ts, c(0,1,1),seasonal = list(order = c(1, 1, 0), period = 54))

#predicting for next 2 quarters (27 weeks)
pred_freezer <- predict(fit_freezer, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 1084359


#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(pred_freezer$pred[1:13]) + pred_freezer$pred[14]/2
predicted2 <- sum(pred_freezer$pred[15:27]) + pred_freezer$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#93.30988

#plotting the graph along with forecated value for Q3 and Q4 2015
ts.plot(data_freezer_Ts, pred_freezer$pred, lty = c(1,3))
