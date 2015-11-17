## 3  
d<-read.csv("pgatour2006.csv",header = T);head(d)
dd<-d[-c(1,2,4,11)]
head(dd)
### a  
pairs(dd,upper.panel = NULL)
ddd<-dd
ddd$PrizeMoney<-log(dd$PrizeMoney)
pairs(ddd, upper.panel = NULL)
