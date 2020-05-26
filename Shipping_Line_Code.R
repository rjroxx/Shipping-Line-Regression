x<-read.csv("C:/Users/aadih/Desktop/clean_data.csv",header=TRUE,stringsAsFactors = FALSE)
y<-na.omit(x)
y$Dispatch.Qty
y1<-as.factor(y$Container.Type)
y1

y2<-as.factor(y$Port.of.Destination)
y2

y4<-as.factor(y$Stuffing.remarks)
y4
y5<-as.factor(y$Act.POL)
y5
y6<-as.factor(y$Shipping.line)
y6
y7<-as.factor(y$Country)
y7
y8<-as.factor(y$Sector)
y8
y9<-as.numeric(as.character(y$Dispatch.Qty))
y9

z1<-cbind.data.frame(y1,y2,y5,y6,y7,y8,y$NO.OF.CNTRS,y9,y$TRANSIT.TIME.AS.PER..RFQ,y$Dev.Cost)
z<-na.omit(z1)
fix(z)

b<-lm(z$`y$Dev.Cost`~ z$y2+z$y6+poly(z$`y$TRANSIT.TIME.AS.PER..RFQ`,5)+z$y5,data=z)
summary(b)
plot(b)
which.max(hatvalues(b))
library(car)
dim(y)

