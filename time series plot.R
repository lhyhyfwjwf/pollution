library('imputeTS')

###Bakersfield plot
Baker = read.csv("D:/STA 160/bakersfield.csv")
x1 = as.vector(Baker$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(Baker$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(Baker$Daily.Max.1.hour.NO2.Concentration)
#ABSOLUTE UNIT
t = as.vector(time(Baker$Date))
dates <- as.Date(Baker$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Bakersfield',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb) (ppb)', xlab = 'time')
#AQI
x1 = as.vector(Baker$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(Baker$DAILY_AQI_VALUE_ozone)
x3 = as.vector(Baker$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
baker = data.frame(Baker[1], Baker[2], as.vector(new1), as.vector(new2), Baker[7]) 
names(baker) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(baker, file='baker.csv')
#plot
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Bakersfield(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')

x = as.vector(Baker$DAILY_AQI_VALUE_ozone)
#### fitting time series model
library(forecast)
bestfit <- list(aic=Inf)
thets <- ts(x, freq=365.25)
for(i in 1:10)
{
  fit <- auto.arima(x, xreg=fourier(thets, K=i), seasonal=FALSE)
  print(c(fit$aic, bestfit$aic))
  if(fit$aic < bestfit$aic){
    print(i)
    bestfit <- fit
  }
  else{
    break;
  }
}
par(mfrow=c(1,1))
##Thus K = 2
fc <- forecast(bestfit, xreg=fourier(thets, K=3))
bestfit
#plot(fc, main = 'Pm2.5 Time Series Fit in Bakersfield', xlab = 'time index',
#     ylab = 'PM2.5 Air Quality Index (AQI)')
#lines(as.Date(baker$Date), fitted(bestfit), col="red")

plot(as.Date(as.time(baker$Date)), x,main = 'Ozone ')
type = 'l',
t = as.vector(time(baker$Date))
lines(t, fitted(bestfit), col="red")
legend("bottomright", c("estimation fit","true value"), 
       lwd = c(2,2), col = c("red", "black"))
#residual       
ts.plot(resid(bestfit), main = 'Residual Plot of PM2.5 Model', ylab = 'residual')


###OZONE
fc <- forecast(bestfit, xreg=fourier(thets, K=1))
bestfit
plot(fc, main = 'Pm2.5 Time Series Fit in Bakersfield', xlab = 'time index',
     ylab = 'PM2.5 Air Quality Index (AQI)')
lines(as.Date(baker$Date), fitted(bestfit), col="red")
t = as.vector(time(Baker$Date))
lines(t, fitted(bestfit), col="red")
legend("bottomright", c("estimation fit","true value", "prediction"), 
       lwd = c(2,2), col = c("red", "black", "blue"))
#residual       
ts.plot(resid(bestfit), main = 'Residual Plot of PM2.5 Model', ylab = 'residual')


xs = diff(x, lag = 365.25)
xss = diff(xs)
thets <- ts(xss, freq=365.25)
for(i in 1:10)
{
  fit <- auto.arima(xss, xreg=fourier(thets, K=i), seasonal=FALSE)
  print(c(fit$aic, bestfit$aic))
  if(fit$aic < bestfit$aic){
    print(i)
    bestfit <- fit
  }
  else{
    break;
  }
}
par(mfrow=c(1,1))
##Thus K = 2
fc <- forecast(bestfit, xreg=fourier(thets, K=1))
bestfit
plot(fc, main = 'Pm2.5 Time Series Fit in Bakersfield', xlab = 'time index',
     ylab = 'PM2.5 Air Quality Index (AQI)')
lines(as.Date(baker$Date), fitted(bestfit), col="red")
t = as.vector(time(baker$Date))
lines(t, fitted(bestfit), col="red")
legend("bottomright", c("estimation fit","true value", "prediction"), 
       lwd = c(2,2), col = c("red", "black", "blue"))
#residual       
ts.plot(resid(bestfit), main = 'Residual Plot of PM2.5 Model', ylab = 'residual')





##
#x.msts <- msts(x, seasonal.periods=c(7,365.25))
#fit <- tbats(x.msts)
#fc <- forecast(fit, h = 100)
#plot(fc)
#lines(t, fitted(fit), col="red")

#par(mfrow=c(1,1))
#fit






###Fresno plot
#ABOSULUTE UNIT
Fresno = read.csv("D:/STA 160/fresno.csv")
x1 = as.vector(Fresno$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(Fresno$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(Fresno$Daily.Max.1.hour.NO2.Concentration)

t = as.vector(time(Fresno$Date))
dates <- as.Date(Fresno$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Fresno',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(Fresno$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(Fresno$DAILY_AQI_VALUE_ozone)
x3 = as.vector(Fresno$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(Fresno[1], Fresno[2], as.vector(new1), as.vector(new2), as.vector(new3)) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='fresno.csv')


par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Fresno(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')


###Los Angeles
#Absolute value
LA = read.csv("D:/STA 160/los angeles.csv")
x1 = as.vector(LA$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(LA$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(LA$Daily.Max.1.hour.NO2.Concentration)

t = as.vector(time(LA$Date))
dates <- as.Date(LA$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Los Angeles',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(LA$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(LA$DAILY_AQI_VALUE_ozone)
x3 = as.vector(LA$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(LA[1], LA[2], as.vector(new1), as.vector(new2), as.vector(new3)) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='LA.csv')

par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in LA(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')


###Modesto
#Absolute value
Modesto = read.csv("D:/STA 160/modesto.csv")
x1 = as.vector(Modesto$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(Modesto$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(Modesto$Daily.Max.1.hour.NO2.Concentration)
t = as.vector(time(Modesto$Date))
dates <- as.Date(Modesto$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Modesto',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(Modesto$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(Modesto$DAILY_AQI_VALUE_ozone)
x3 = as.vector(Modesto$DAILY_AQI_VALUE_no2)

new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(Modesto[1], Modesto[2], as.vector(new1), as.vector(new2), x3) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='modesto.csv')
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Modesto(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')


###Sacramento
#Absolute value
Sacramento = read.csv("D:/STA 160/sacramento.csv")
x1 = as.vector(Sacramento$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(Sacramento$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(Sacramento$Daily.Max.1.hour.NO2.Concentration)
t = as.vector(time(Sacramento$Date))
dates <- as.Date(Sacramento$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Sacramento',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(Sacramento$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(Sacramento$DAILY_AQI_VALUE_ozone)
x3 = as.vector(Sacramento$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(Sacramento[1], Sacramento[2], as.vector(new1), as.vector(new2), as.vector(new3)) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='Sacramento.csv')
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in Sacramento(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')


###San Francisco
#Absolute value
San_Francisco = read.csv("D:/STA 160/san francisco.csv")
x1 = as.vector(San_Francisco$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(San_Francisco$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(San_Francisco$Daily.Max.1.hour.NO2.Concentration)
t = as.vector(time(San_Francisco$Date))
dates <- as.Date(San_Francisco$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Francisco',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(San_Francisco$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(San_Francisco$DAILY_AQI_VALUE_ozone)
x3 = as.vector(San_Francisco$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(San_Francisco[1], San_Francisco[2], as.vector(new1), as.vector(new2), x3) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='San_Francisco.csv')
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Francisco(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')

###San JOse
#Absolute value
San_Jose = read.csv("D:/STA 160/san jose.csv")
x1 = as.vector(San_Jose$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(San_Jose$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(San_Jose$Daily.Max.1.hour.NO2.Concentration)
t = as.vector(time(San_Jose$Date))
dates <- as.Date(San_Jose$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Jose',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(San_Jose$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(San_Jose$DAILY_AQI_VALUE_ozone)
x3 = as.vector(San_Jose$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(San_Jose[1], San_Jose[2], as.vector(new1), as.vector(new2), x3) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='San_Jose.csv')
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Jose(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')

###San_Luis
#Absolute value
San_Luis = read.csv("D:/STA 160/san luis obispo.csv")
x1 = as.vector(San_Luis$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(San_Luis$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(San_Luis$Daily.Max.1.hour.NO2.Concentration)
t = as.vector(time(San_Luis$Date))
dates <- as.Date(San_Luis$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Luis Obispo',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(San_Luis$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(San_Luis$DAILY_AQI_VALUE_ozone)
x3 = as.vector(San_Luis$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(San_Luis[1], San_Luis[2], as.vector(new1), as.vector(new2), x3) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='San_Luis.csv')
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Luis Obispo(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')

###San_Diego
#Absolute value
San_Diego = read.csv("D:/STA 160/san diego.csv")
x1 = as.vector(San_Diego$Daily.Mean.PM2.5.Concentration)
x2 = as.vector(San_Diego$Daily.Max.8.hour.Ozone.Concentration)
x3 = as.vector(San_Diego$Daily.Max.1.hour.NO2.Concentration)
t = as.vector(time(San_Diego$Date))
dates <- as.Date(San_Diego$Date, "%m/%d/%Y")
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Diego',
     ylab = 'PM2.5 (ug/m3)')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone (ppm)')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2 (ppb)', xlab = 'time')
#AQI
x1 = as.vector(San_Diego$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(San_Diego$DAILY_AQI_VALUE_ozone)
x3 = as.vector(San_Diego$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
Frame = data.frame(San_Diego[1], San_Diego[2], as.vector(new1), as.vector(new2), as.vector(new3)) 
names(Frame) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(Frame, file='San_Diego.csv')
par(mfrow=c(3,1))
par(mar=c(3,5,2,2)+0.1)
plot(dates,x1, type = 'l', main = 'Air Pollution in San Diego(AQI)',
     ylab = 'PM2.5')
abline(a = 100, b = 0, col = 'red')
par(mar=c(3,5,1,2)+0.1)
plot(dates,x2, type = 'l', xlab = NULL,
     ylab = 'Ozone')
abline(a = 100, b = 0, col = 'red')
par(mar=c(5,5,1,2)+0.1)
plot(dates,x3, type = 'l', 
     ylab = 'NO2', xlab = 'time')
abline(a = 100, b = 0, col = 'red')


##fill in missing value

x1 = as.vector(Baker$DAILY_AQI_VALUE_PM2.5)
x2 = as.vector(Baker$DAILY_AQI_VALUE_ozone)
x3 = as.vector(Baker$DAILY_AQI_VALUE_no2)
new1 = na.kalman(x1, model = "StructTS", smooth = TRUE, nit = -1)
new2 = na.kalman(x2, model = "StructTS", smooth = TRUE, nit = -1)
new3 = na.kalman(x3, model = "StructTS", smooth = TRUE, nit = -1)
baker = data.frame(Baker[1], Baker[2], as.vector(new1), as.vector(new2), Baker[7]) 
names(baker) <- c("Date", "City", "DAILY_AQI_VALUE_PM2.5", "DAILY_AQI_VALUE_ozone", "DAILY_AQI_VALUE_no2")
write.csv(baker, file='baker.csv')
