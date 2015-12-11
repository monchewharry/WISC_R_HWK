## 1 
d<-read.csv("../hwk5/pgatour2006.csv",header = T)
dd<-d[-c(1,2,4,11)]

### ridge #####
require(MASS)
m <- lm.ridge(log(PrizeMoney) ~ ., data = dd, lambda = seq(0,50,0.01))
select(m)
(m1<- lm.ridge(log(PrizeMoney) ~ ., data = dd, lambda = 12.95))

ddd<-dd
ddd$PrizeMoney<-log(dd$PrizeMoney)
pairs(ddd, upper.panel = NULL)

designX<- as.matrix(cbind(1,dd[-1]))
fit<- designX %*% coef(m1)
res <- fit- log(dd$PrizeMoney)
standard.res <- res/sd(res)

layout(matrix(c(1,2,3,3),ncol = 2))
plot(standard.res,main = "standardized residuals")
plot(fit,res,main = "residuals vs fitted value")
qqnorm(standard.res,ylab="standard residual")
qqline(standard.res)
layout(1)
### PCR ####
require(pls)
pca<- pcr(log(PrizeMoney) ~ ., data = dd)
summary(pca)
newd <- data.frame(y.log=log(dd$PrizeMoney),unclass(pca$scores))
plot(newd)

designX<- as.matrix(cbind(1,unclass(pca$scores)))
fit<- pca$fitted.values
res <- log(dd$PrizeMoney) - fit
standard.res <- res/sd(res)
layout(matrix(c(1,2,3,3),ncol = 2))
plot(standard.res,main = "standardized residuals")
plot(fit,res,main = "residuals vs fitted value")
qqnorm(standard.res,ylab="standard residual")
qqline(standard.res)
layout(1)

### PLS  ####
pls = plsr(log(PrizeMoney) ~ ., data = dd)
summary(pls)
newd <- data.frame(y.log=log(dd$PrizeMoney),unclass(pls$scores))
plot(newd)

designX<- as.matrix(cbind(1,unclass(pls$scores)))
fit<- pls$fitted.values
res <- log(dd$PrizeMoney) - fit
standard.res <- res/sd(res)

layout(matrix(c(1,2,3,3),ncol = 2))
plot(standard.res,main = "standardized residuals")
plot(fit,res,main = "residuals vs fitted value")
qqnorm(standard.res,ylab="standard residual")
qqline(standard.res)
layout(1)

## 2
### transformation ####
dd2 <- read.table("HWK6q2.txt")
names(dd2) <-c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")
pairs(dd2)
full <- lm(Y~.,data = dd2)
library(car)
boxCox(full, family="yjPower", plotit = TRUE)
dd2$Y <- yjPower(dd2$Y, 0.5)
pairs(dd2)

### ridge #####
m <- lm.ridge(Y ~ ., data = dd2, lambda = seq(0,50,0.01))
select(m)
(m1<- lm.ridge(Y ~ ., data = dd2, lambda = 2.9))

pairs(dd2)
designX<- as.matrix(cbind(1,dd2[-1]))
fit<- designX %*% coef(m1)
res <- fit- dd2$Y
standard.res <- res/sd(res)

layout(matrix(c(1,2,3,3),ncol = 2))
plot(standard.res,main = "standardized residuals")
plot(fit,res,main = "residuals vs fitted value")
qqnorm(standard.res,ylab="standard residual")
qqline(standard.res)
layout(1)

### PCR ####
require(pls)
pca<- pcr(Y ~ ., data = dd2)
summary(pca)
newd <- data.frame(y=dd2$Y,unclass(pca$scores))
plot(newd)

designX<- as.matrix(cbind(1,unclass(pca$scores)))
fit<- pca$fitted.values
res <- dd2$Y - fit
standard.res <- res/sd(res)
layout(matrix(c(1,2,3,3),ncol = 2))
plot(standard.res,main = "standardized residuals")
plot(fit,res,main = "residuals vs fitted value")
qqnorm(standard.res,ylab="standard residual")
qqline(standard.res)
layout(1)

### PLS  ####
pls = plsr(Y ~ ., data = dd2)
summary(pls)
newd <- data.frame(y=dd2$Y,unclass(pls$scores))
plot(newd)

designX<- as.matrix(cbind(1,unclass(pls$scores)))
fit<- pls$fitted.values
res <- dd2$Y - fit
standard.res <- res/sd(res)

layout(matrix(c(1,2,3,3),ncol = 2))
plot(standard.res,main = "standardized residuals")
plot(fit,res,main = "residuals vs fitted value")
qqnorm(standard.res,ylab="standard residual")
qqline(standard.res)
layout(1)

# 3. glm 
# study the effect of the fuel concentration and strain type on the 
# number of Ceriodaphnia organisms.
str(cerio <- with(read.table("ceriodaphnia.txt", sep="")
                  ,data.frame(numb=V1, conc=V2, strain=factor(V3))))
gm <- glm(numb ~ ., family = poisson, data = cerio)
summary(gm)
layout(matrix(c(1,2,3,4),ncol = 2, byrow = TRUE))
plot(gm, ask = FALSE)
layout(1)

h0<-glm(numb ~ 1, family = poisson, data = cerio)
anova(h0, gm, test = "Chisq") 

# 4.logit 

str(renewal <- with(read.table("renewal.txt", sep=""),
    data.frame(renew=factor(V1, levels=1:0,labels=c("N","Y"))
               ,amt=V2)))
gml <- glm(renew ~ ., family = binomial, data = renewal)
coef(gml)
xj<-jitter(renewal$amt)
plot(xj,gml$fitted.values, ylim = c(0,1), main = "Fitted Logistic Response", xlab = "amt", ylab = "")
points(xj, as.numeric(renewal$renew)-1)
lines(lowess(xj,as.numeric(renewal$renew)-1 ), col = "red")


xbnew<- c(1,40) %*% coef(gml)

exp(xbnew) / (1+exp(xbnew)) #the probability for dues increased by 40

(log(0.75/(1- 0.75)) - coef(gml)[1])/coef(gml)[2]# the amount of increase for 75% 

exp(confint(gml, "amt", level = 0.9))# 90% CI for exp(beta1)

require(aod)
wald.test(Sigma = vcov(gml), b = coef(gml), Term = 2)

h0 <- glm(renew ~ 1, family = binomial, data = renewal)
anova(h0, gml, test = "Chisq")

# 5.bootstrap procedure for 4


# 6.poisson regression  

Isch <- with(read.table("Ischemic.txt", sep=""),
             data.frame(id=V1, cost=V2, age=V3,
              gender=factor(V4, labels=c("F","M")),
              int=V5, ndrug=V6, emerg=V7, compl=V8,
              comorb=V9, duration=V10))

gmp <- glm(emerg ~ ., family = poisson, data = Isch)
summary(gmp)

par(mfrow=c(2,2))
plot(gmp,ask = FALSE)

library(bestglm)
library(dplyr)
dd<-mutate(y=emerg,.data = Isch)#new y
dd<- select(dd,-emerg)
aic<-bestglm(Xy = dd,
             family = gaussian,
             IC = "AIC", 
             method = "exhaustive") 
aic$BestModel



