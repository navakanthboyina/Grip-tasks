library(rpart)
library(rpart.plot)
iris
View(iris)
str(iris)
head(iris)
#Normalizing
norm=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
DTdatanorm=lapply(iris[1:4],norm)
DTdatanorm
summary(DTdatanorm)
str(DTdatanorm)
#coloumn bind
DTdatanorm=cbind(DTdatanorm,iris[5])
#data subsetting
set.seed(1234)
ind<-sample(2,nrow(DTdatanorm),replace = T, prob =c(0.7,0.3))
training=DTdatanorm[ind==1,]
training
testing=DTdatanorm[ind==2,]
testing
str(testing)
#constructing model
DTmodel=rpart(Species~.,data=training,method="class")
summary(DTmodel)
plot(DTmodel)
text(DTmodel)
rpart.plot(DTmodel)
#prediting
predictmethod=predict(DTmodel,testing,type="class")
dttab=table(testing$Species,predictmethod)
#accuracy
accuracy=sum(diag(dttab))/(sum(dttab))*100
accuracy
