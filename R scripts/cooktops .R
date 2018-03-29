
data=read.csv("shipment_Data.csv")
class(data)


#slicing time (year-week) and units alone
data_cooktops <- data[data$PRODUCT == "Cooktops",c(3,5)]


plot(data_cooktops$INDUSTRY_UNITS,data_cooktops$WEEK)
#converting the dataframe to timeseries
data_cooktops_ts <- ts(data_cooktops$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding classdata_cooktops
class(data_cooktops_ts)

#plot the time series
ts.plot(diff(data_cooktops_ts))

#Augmented Dickey-Fuller Test for given series
adf.test(data_cooktops_ts)

#acf graph for given series
acf(data_cooktops_ts)

#pacf graph for given series
pacf(data_cooktops_ts)

#Augmented Dickey-Fuller Test after differencing
adf.test(diff(data_cooktops_ts))

#acf graph after differencing
acf(diff(data_cooktops_ts))

#pacf graph after differencing
pacf(diff(data_cooktops_ts))

#fitting ARIMA model
fit_cooktops <- arima(data_cooktops_ts, c(0,0,1),seasonal = list(order = c(1, 1, 0), period = 54))

#predicting for next 2 quarters (27 weeks)
pred_cooktops <- predict(fit_cooktops, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 397641

#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(pred_cooktops$pred[1:13]) + pred_cooktops$pred[14]/2
predicted2 <- sum(pred_cooktops$pred[15:27]) + pred_cooktops$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs(((actual-predicted1-predicted2)*100)/actual -100)
#accuracy=98.82377

#plotting the graph along with forecated value for Q3 and Q4 2015
ts.plot(data_cooktops_ts, pred_cooktops$pred, lty = c(1,3))
