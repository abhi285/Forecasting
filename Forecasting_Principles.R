TS1 <- read.csv(file="C:/Users/Abhijith Antony/Downloads/MORTGAGE30USsince2005.csv", header=TRUE, sep=",")
TS2 <- read.csv(file="C:/Users/Abhijith Antony/Downloads/MORTGAGE15USsince2005.csv", header=TRUE, sep=",")
TS3 <- read.csv(file="C:/Users/Abhijith Antony/Downloads/MORTGAGE5USsince2005.csv", header=TRUE, sep=",")
TS4 <- read.csv(file="C:/Users/Abhijith Antony/Downloads/CPIAUCSLsince1954.csv", header=TRUE, sep=",")
TS5 <- read.csv(file="C:/Users/Abhijith Antony/Downloads/FEDFUNDSsince1954.csv", header=TRUE, sep=",")
TS6 <- read.csv(file="C:/Users/Abhijith Antony/Downloads/UNRATEsince1954.csv", header=TRUE, sep=",")

head(ts4_final)

ts1_final=TS1[-(1:200),]
ts2_final=TS2[-(1:200),]
ts3_final=TS3[-(1:200),]
ts4_final=TS4[-(1:200),]
ts5_final=TS5[-(1:200),]
ts6_final=TS6[-(1:200),]

# Q1 ) Working with ts1_final (MORTGAGE30USsince2005.csv)

#1. Data visualization


data <- ts(ts1_final$MORTGAGE30US, start=c(2008,42), end=c(2019,44), frequency=48)
#(1)visualization  
plot(data)
#(2)the identification of the model (stationary or nonstationary? which orders?)
adf.test(data,k=1)
adf.test(data,k=2)
adf.test(data,k=3)
#Dickey-Fuller = -3.0797, Lag order = 3, p-value = 0.1213
#based on this test we see that the data is not staionary, in adf Ho : time series is non 
#stationary cannot be rejected. we go ahead with differencing the data

diff_data <- data %>% diff()
plot(diff_data)
adf.test(diff_data,k=1)
adf.test(diff_data,k=2)
adf.test(diff_data,k=3)

#Dickey-Fuller = -10.501, Lag order = 3, p-value = 0.01
#based on this test we see that the data is  staionary, in adf Ho : time series is non 
#stationary can be rejected. we go ahead with identifying the model 
#and plotting acf and pacf.
acf(diff_data)
pacf(diff_data)

# here we see that the ACF is cut off at 2
#and pacf is getting cut off at 2 so we can go ahead with ARMA(2,2) model
# we ignore the seasonality

#(3) estimation of the model parameters and interpretation of significance of the parameters
(fitauto <- auto.arima(diff_data))


fit212 <- Arima(data, order=c(2,1,2),include.constant = TRUE)
# parameters found : mean : = 4.4709, phi1 : -0.2211, phi2: 0.5913, theta1 :0.2866
# theta2 :-0.4593 which is less sigma square:0.006179
#than 1 which also idicates staionarity

#(4) diagnostic checking on residuals and possible revision to your model choice;
Box.test(resid(fit212), lag = 10, type = "Ljung-Box", fitdf = 1)
checkresiduals(fit212)
#X-squared = 5.649, df = 9, p-value = 0.7745, siunce p value is high indicates 
#that residuals are white noise hence we can proceed with 


#(5) forecasts for the period in question and visualize your forecast in a figure.
(ft <-forecast(fit212, h=50))
plot(forecast(fit212,h=50))


# Q 2 )Working with ts4_final (CPIAUCSLsince1954.csv)

#1. Data visualization


data_2 <- ts(ts4_final$CPIAUCSL, start=c(1971,3), end=c(2019,10), frequency=12)
#(1)visualization  
plot(data_2)
#(2)the identification of the model (stationary or nonstationary? which orders?)
adf.test(data_2,k=1)
adf.test(data_2,k=2)
adf.test(data_2,k=3)
#Dickey-Fuller = -1.9231, Lag order = 3, p-value = 0.6109
#based on this test we see that the data is not staionary, in adf Ho : time series is non 
#stationary cannot be rejected. we go ahead and do differencing of the model.

diff_data_2 <- data_2 %>% diff()
plot(diff_data_2)
adf.test(diff_data_2,k=1)
adf.test(diff_data_2,k=2)
adf.test(diff_data_2,k=3)
#Dickey-Fuller = -13.95, Lag order = 3, p-value = 0.01
#based on this test we see that the data is  staionary, in adf Ho : time series is non 
#stationary can be rejected. we go ahead with identifying the model 
#and plotting acf and pacf.

acf(diff_data_2)
pacf(diff_data_2)
# since ACF value is cutting off after 1 and PACF value after 2 we use ARMA(1,2)

#(3) estimation of the model parameters and interpretation of significance of the parameters


fit112 <- Arima(diff_data_2, order=c(1,1,2),include.constant = TRUE)
# parameters found : mean : = 0, phi1 : 0.2185, theta1: -0.6931, theta2 : -0.3068
#which is less than 1 which also idicates staionarity, sigma square : 0.1474

#(4) diagnostic checking on residuals and possible revision to your model choice;
Box.test(resid(fit112), lag = 10, type = "Ljung-Box", fitdf = 1)
checkresiduals(fit112)
#X-squared = 7.079, df = 9, p-value = 0.6289, since p value is high indicates 
#that residuals are white noise hence we can proceed with 


#(5) forecasts for the period in question and visualize your forecast in a figure.
(ft <-forecast(fit112, h=50))
plot(forecast(fit112,h=50))


##### Q3)
data_cointegrate_1 <- ts(ts1_final$MORTGAGE30US, start=c(2008,42), end=c(2019,44), frequency=48)
data_cointegrate_2 <- ts(ts2_final$MORTGAGE15US, start=c(2008,42), end=c(2019,44), frequency=48)
data_cointegrate_3 <- ts(ts3_final$MORTGAGE5US, start=c(2008,42), end=c(2019,44), frequency=48)

usa.ts.df <- ts.union(data_cointegrate_1=ts(ts1_final$MORTGAGE30US, start=c(2008,42), end=c(2019,44), frequency=48), # package tseries
                      data_cointegrate_2=ts(ts2_final$MORTGAGE15US, start=c(2008,42), end=c(2019,44), frequency=48), 
                      data_cointegrate_3=ts(ts3_final$MORTGAGE5US, start=c(2008,42), end=c(2019,44), frequency=48),
                      data_cointegrate_1,data_cointegrate_2,data_cointegrate_3,
                      dframe=TRUE)

#Two series are cointegrated when their trends 
#are not too far apart and are in some sense similar.

plot(usa.ts.df$data_cointegrate_1)
points(usa.ts.df$data_cointegrate_2,type="l",lty=2)
#These 2 are far apart hence not cointegrated

plot(usa.ts.df$data_cointegrate_2)
points(usa.ts.df$data_cointegrate_3,type="l",lty=2)
#These 2 are not so far apart hence cointegrated


plot(usa.ts.df$data_cointegrate_1)
points(usa.ts.df$data_cointegrate_3,type="l",lty=2)
#These 2 are far apart hence not cointegrated



###### Q4)


data_var_4 <- ts(ts4_final$CPIAUCSL, start=c(1971,3), end=c(2019,10), frequency=12)
data_var_5 <- ts(ts5_final$FEDFUNDS, start=c(1971,3), end=c(2019,10), frequency=12)
data_var_6 <- ts(ts6_final$UNRATE, start=c(1971,3), end=c(2019,10), frequency=12)



usa.ts.df1 <- ts.union(data_var_4=ts(ts4_final$CPIAUCSL, start=c(1971,3), end=c(2019,10), frequency=12), # package tseries
                      data_var_5=ts(ts5_final$FEDFUNDS, start=c(1971,3), end=c(2019,10), frequency=12), 
                      data_var_6=ts(ts6_final$UNRATE, start=c(1971,3), end=c(2019,10), frequency=12),
                      data_var_4,data_var_5,data_var_6,
                      dframe=TRUE)
plot(usa.ts.df1[,1], facets=TRUE) 
plot(usa.ts.df1[,2], facets=TRUE) 
plot(usa.ts.df1[,3], facets=TRUE) 

xreg = cbind (usa.ts.df1[,2],usa.ts.df1[,3])

var1 <- VAR(uschange[,1:3], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")

summary(var1)
#The forecasts generated by the VAR(1) are plotted below.
forecast(var1) %>%
  autoplot() + xlab("Year")



# Forecasts are better for VAR since feedback relationships are allowed for in the vector autoregressive (VAR) framework. 
#In this framework, all variables are treated symmetrically. 



