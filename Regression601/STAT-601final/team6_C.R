library(xlsx)
set.b<-read.csv2(file = "Set_b.csv",header = T,sep = ",")
set.c<-read.csv2(file = "Set_c.csv",header = T,sep = ",")
yy<- read.xlsx("TP53.xlsx",sheetIndex = 1)##TP53

View(set.b);View(set.c)
names(set.b)[names(set.b) %in% names(set.c)]

lm1<-lm(response~.,set.b)

drop1(lm1)
