ext2<-read.table("HWK2_Ext2.txt")
#ex2  
k<-2 
d<-ext2[((k-1)*90+1):(k*90),]
dim(d)
d1<-d[1:30,]
d2<-d[31:60,]
d3<-d[61:90,]
head(d1)
plot(d)
#from the scatter plot we can see that there are no obvious relationship between V1 AND V3. 
#But they both have a clear relationship with v2.
#So,I believe there is V2 is the reponse variable.  

#I first suppose X1=V1
d11<-as.data.frame(d1)
names(d11)<-c("x1","y","x2")
form1<- as.formula("y ~ 1+x1+x1:x2")
m1<-lm(form1,data=d11);summary(m1)

d21<-as.data.frame(d2)
names(d21)<-c("x1","y","x2")
form2<-as.formula("1/y ~ 1+x1+x2")
m2<-lm(form2,data=d21);summary(m2)

d31<-as.data.frame(d3)
names(d31)<-c("x1","y","x2")
form3<-as.formula("y ~ 0+x1+x2+I(x1^2)")
m3<-lm(form3,data=d31);summary(m3)
#obviously the third model is best fitted  

#ex3 
#install.packages("SenSrivastava")
library(SenSrivastava)
head(E1.19)

suppressMessages(library(dplyr))
group<-split(E1.19,E1.19$B)
plot(group$c[-3],main="type = c")
plot(group$p[-3],main="type = p")

library(ggplot2)
ggplot(E1.19,aes(x=P,y = Price,group=B,colour=B))+ xlab("#page")+geom_line()


qplot(P,Price,data=E1.19)+
  geom_smooth(aes(colour=B),method="lm",se=F)+
  facet_wrap( ~ B,scale="free_y")+
  labs(title="Key Graph")


#ex4  
grocery<-read.table("/Users/dcao28/Downloads/grocery_retailer.txt",header = T)
head(grocery)
plot(grocery)

m<-lm(Y~X1+X2+X3,data = grocery)
summary(m)

plot(resid(m))
