library(forecast)
library(tseries)

# Lecture notes chapter 7 - Figure 7.6 Plot of data from InternetUser.txt

# Creating a vector to keep all the number of users over a 100-minute period
Y <-  c(88, 84, 85, 85, 84, 85, 83, 85, 88, 89, 91, 99, 
            104, 112, 126, 138, 146, 151, 150, 148, 147, 149, 143, 132, 131, 
            139, 147, 150, 148, 145, 140, 134, 131, 131, 129, 126, 126, 132, 
            137, 140, 142, 150, 159, 167, 170, 171, 172, 172, 174, 175, 172, 
            172, 174, 174, 169, 165, 156, 142, 131, 121, 112, 104, 102, 99, 
            99, 95, 88, 84, 84, 87, 89, 88, 85, 86, 89, 91, 91, 94, 101, 110, 
            121, 135, 145, 149, 156, 165, 171, 175, 177, 182, 193, 204, 208, 
            210, 215, 222, 228, 226, 222, 220) 

# Turning vector into a time series object and plot it out
Y <- ts(Y)
plot(Y, main="Number of Internet Users" , ylab="Number of Users", xlab="Time (Minute)")

# Stationary test - ACF/PACF & ADF/KPSS
par(mfrow=c(1,2))
acf(Y , main="ACF of Y"); pacf(Y , main="Partial ACF of Y")
par(mfrow=c(1,1))
adf.test(Y)
kpss.test(Y)

# Box-Pierce and Ljung-Box test
h = min(10,(length(Y)/5)) # non-seasonal lag
Box.test(Y, lag=h)
Box.test(Y, lag=h ,type=c("Ljung-Box"))

# Check difference
ndiffs(Y)
Y_diff <- diff(Y, lag=1)
par(mfrow=c(1,2))
acf(Y_diff , main="ACF of Differenced Y"); pacf(Y_diff , main="Partial ACF of Differenced Y")
par(mfrow=c(1,1))

# Fit model
fit <- auto.arima(Y)
fit
checkresiduals(fit)

fc <- forecast(fit, h=20)
plot(fc, main="Forecast")
