#library importing
library(dplyr)
library(factoextra)

#data preparation
datamall<-read.csv("https://raw.githubusercontent.com/lutfiahusnakhoirunnisa/customersegmentation/main/Mall_Customers.csv", sep=";")
dim(datamall)
head(datamall)
sum(is.na(datamall)==TRUE)

#wss model
fviz_nbclust(datamall, kmeans, method = "wss")

#hierarki
distance<-dist(datamall)
modelhierarki<-hclust(distance,method="single")
member_hierarki<-cutree(modelhierarki,6)
##get cluster means
aggregate(datamall,list(member_hierarki),mean)
##append cluster assignment
tbmember_hierarki<- data.frame(datamall, member_hierarki)
head(tbmember_hierarki)

#k-means
modelkm <- kmeans(datamall, 6)
member_km <- modelkm$cluster
##get cluster means
aggregate(datamall,by=list(member_km),FUN=mean)
##append cluster assignment
tbmember_km <- data.frame(datamall, member_km)
head(tbmember_km)

#plot
##hierarki
library(ggplot2)
tbmember_hierarki$member_hierarki=as.factor(tbmember_hierarki$member_hierarki)
ggplot(tbmember_hierarki,aes(x=tbmember_hierarki$Annual.Income,y=tbmember_hierarki$Spending.Score,col=member_hierarki,size=Age))+geom_point()

##k-means
tbmember_km$member_km=as.factor(tbmember_km$member_km)
ggplot(tbmember_km,aes(x=tbmember_km$Annual.Income,y=tbmember_km$Spending.Score,col=member_km,size=Age))+geom_point()
