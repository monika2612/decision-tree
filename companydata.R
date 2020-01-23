data <- read.csv(file.choose())
View(data)
high=ifelse(data$Sales<10,"no","yes")
cd=data.frame(data,high)
View(cd)
library(caret)
library(C50)
# Data partion for model building and testing

inTraininglocal <- createDataPartition(cd$high,p=.75,list=F)
training <- cd[inTraininglocal,]
View(training)
testing <- cd[-inTraininglocal,]
#model building
model <- C5.0(training$high~.,data = training,trails = 40)
# Generating the model summary
summary(model)
pred <- predict.C5.0(model,testing[,-12])
a <- table(testing$high,pred)
a
sum(diag(a)/sum(a))
plot(model)
###Bagging####
acc<-c()

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(cd$high,p=.85,list=F)
  training1<-cd[inTraininglocal,]
  testing<-cd[-inTraininglocal,]
  
  fittree<-C5.0(training1$high~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-12])
  a<-table(testing$high,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc

summary(acc)
acc

