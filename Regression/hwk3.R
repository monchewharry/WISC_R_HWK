# 5  

n=100#sample size
dat<- data.frame(x1=rnorm(n,3,2),x2=rexp(n,1),x3=rgamma(n,2,3)) 
betaT<-c(beta0=2,beta1=1.2,beta2=-1.4,beta3=-0.5)
muT<-as.vector(model.matrix(~x1+x2+x3,dat) %*% betaT)#EY  

sigmasq<-runif(n,0.5,3)#variance
sigma<-sqrt(sigmasq)
yy<- muT+sigma*rnorm(n)# ONE realization  with different variance
lm(yy~.,data=dat)  
lm(yy~.,data=dat,weights = 1/sigmasq)

S<-10000 #simulation realization number
YY<- muT + sigma * matrix(rnorm(n * S), nrow = n,ncol = S)

beta_diff<-function(y){
  lmi<-lm(y~.,data = dat)
  wlmi<-lm(y~.,data = dat,weights = 1/sigmasq)
  c(lmi$coefficients-betaT,wlmi$coefficients-betaT)
}
beta_dif<-apply(YY, 2,beta_diff)

beta_dif_lm<-beta_dif[1:4,]
beta_dif_wlm<-beta_dif[5:8,]

rowMeans(beta_dif_lm^2)#beta mse OF OLS
rowMeans(beta_dif_wlm^2)#beta mse OF WLS

# 6
ants <- read.table("thatch_ant_c5del.txt", sep='', header=TRUE)
pairs(ants)
library(lattice)
splom(ants)









