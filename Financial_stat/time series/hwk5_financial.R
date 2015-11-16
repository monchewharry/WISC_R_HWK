library(quantmod)
getSymbols("AAPL", from= "2005-10-31" , to = "2015-10-31")
AAPL<-Ad(AAPL)
## daily log returns 
r<-diff(log(AAPL))[-1]

## a
t.test(r)# reject null hypothesis which means the mean is not zero


## b 
Box.test(r, type="Ljung-Box" , lag = 10) # accept null hypothesis which means the series has no serial correlation 

## c
par(mfrow=c(1,2))
acf(r)
pacf(r,lag.max = 50)

library(forecast)
mean.model<-auto.arima(r)
summary(mean.model)#arma(3,2) 

resid2<- (residuals(mean.model))^2
acf(resid2)
pacf(resid2, lag=20, main="")

Box.test(resid2, type = "Ljung-Box" , fitdf = 5, lag = 10)# there exits arch effect

length(resid2)
y<-resid2[12:2517]
x<-cbind(resid2[11:2516],resid2[10:2515],resid2[9:2514],resid2[8:2513]
         ,resid2[7:2512],resid2[6:2511],resid2[5:2510], resid2[4:2509],resid2[3:2508]
         , resid2[2:2507], resid2[1:2506])
summary(lm(y~x))#F-statistics is 30.8, the null hypothesis of no arch effect is rejected  

## d  
library(rugarch)
par(mfrow=c(1,1))
plot(density(r),add = T, lty= 1 )
curve(dnorm(x, mean(r), sd(r)), from = -0.2, to = 0.15, lty= 2,add = T)
legend("topright", lty=c(1,2), c("sample", "Norm"))

spec1=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 2)), distribution.model = "norm") # specify model

m1=ugarchfit(r,spec=spec1) # fit model
std.resid<-m1@fit$z
qqnorm(std.resid) 
qqline(std.resid)

m1# the model  

Box.test(std.resid, lag=10, type = "Ljung-Box", fitdf = 5)
Box.test(std.resid^2, lag=10, type = "Ljung-Box", fitdf = 5)
acf(std.resid^2)# the model is adequate 

##e

spec2=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 2)), distribution.model = "std") # specify model
m2=ugarchfit(r,spec=spec2) # fit model

m2# the model  
std.resid<-m2@fit$z
Box.test(std.resid, lag=10, type = "Ljung-Box", fitdf = 5)
Box.test(std.resid^2, lag=10, type = "Ljung-Box", fitdf = 5)
acf(std.resid^2)# the model is adequate 


## f
plot(sigma(m2))

## g  
forecast=ugarchforecast(m2, data=NULL, n.ahead = 5, n.roll= 0, out.sample = 0)
forecast

## h  

spec3=ugarchspec(
  variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 2)), distribution.model = "std") # specify model
m3=ugarchfit(r,spec=spec3) # fit model

m3# the model  
std.resid<-m3@fit$z
Box.test(std.resid, lag=10, type = "Ljung-Box", fitdf = 5)
Box.test(std.resid^2, lag=10, type = "Ljung-Box", fitdf = 5)
acf(std.resid^2)# the model is adequate 

## i

spec4=ugarchspec(
  variance.model = list(model = "fGARCH", submodel="TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 2)), distribution.model = "std") # specify model
m4=ugarchfit(r,spec=spec4) # fit model

m4# the model  
std.resid<-m4@fit$z
Box.test(std.resid, lag=10, type = "Ljung-Box", fitdf = 5)
Box.test(std.resid^2, lag=10, type = "Ljung-Box", fitdf = 5)
acf(std.resid^2)# the model is adequate 



















