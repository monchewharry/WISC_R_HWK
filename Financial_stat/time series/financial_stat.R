# simulation of AR(2)  
n <- 100
phi <- c(0.6,-.4)
x0 <- 0
set.seed(123)
arima.sim(n, model=list(ar=phi), start.innov=c(0,0), n.start=2, 
          innov=c(0,0,rt(n-2,8)))


# simulate AR(1)  
set.seed(321)
sim_series<-arima.sim(n=100,model = list(ar=0.8))

acf(sim_series)[c(1,5)]

sim10000<-vector("list")
for(i in seq(10000)){
  sim10000[[i]]<- arima.sim(n=100,model = list(ar=0.8))
}
acf.1.5<-sapply(sim10000, function(x) acf(x,lag.max = 5,plot=FALSE)$acf[,,1][c(2,6)])
rownames(acf.1.5)<-c("acf1","acf5")
acf15<-as.data.frame(t(acf.1.5))

hist(acf15$acf1,probability = T)
plot(density(acf15$acf1),main="empirical/theoretical density")

curve(dnorm(x,0.8,(1-0.8^2)/100)
      ,xlim =c(0.4,0.9),add = T)


hist(acf15$acf5,probability = T)
plot(density(acf15$acf5),main="empirical/theoretical density",ylim=c(0,5) )
curve(dnorm(x,0.8^5,(1/100)*(1+0.8^2)/(1-.8^2))
      ,xlim =c(-0.4,0.9),add = T)

# gold
gold<-read.csv("Downloads/gold.csv",colClasses = c("Date","numeric"))
library(dplyr)
head(gold)
gold<-arrange(gold,Date)
library(quantmod)
gold<-.xts(gold$Value,index = gold$Date)
chartSeries(gold,theme = "white")
r<-dailyReturn(gold,type = "log")
chartSeries(r,theme = "white")

Box.test(r,lag=10,type = "Ljung-Box")


ar1<-ar(unclass(r)[,1],method = "mle")
plot(1:12,ar1$aic[2:13],main = "AIC 1:12",type = "b")

fit<-arima(r,order=c(8,0,0))

plot(fit$residuals)
Box.test(fit$residuals,lag=10,type ="Ljung",fitdf = 8)

se<-sqrt(diag(fit$var.coef))#se
t<-fit$coef/se

which(abs(t)>1.645)
fixed<-rep(0,9)
fixed[which(abs(t)>1.645)]<-NA 
fit.new<-arima(r,order=c(8,0,0),fixed=fixed)
fit.new

predict(fit.new,3)


