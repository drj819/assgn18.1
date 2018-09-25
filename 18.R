getwd()
p='C:/Users/Admin/Documents/acd'
setwd(p)
list.files()
list.dirs()
library(readr)
Wearable <- read.csv("wearable.csv") 
data1<-wearable
library(devtools)
library(helpRFunctions)
names(data) 
dim(data) library(caret) 
library(zoo) library(plyr)
data<-na.exclude(data1) 
is.na(data) 
which(is.na(data)) 
sum(is.na(data)) 
colSums(is.na(data))
str(data)
summary(data)
pairs(data[7:15])
library(doParallel)

registerDoParallel() 
set.seed(12345) 

head(dataTrain)
head(dataTest)
indexNA <- as.vector(sapply(dataTrain[,1:152],function(x) {length(which(is.na(x)))!=0})) 
dataTrain <- dataTrain[,!indexNA]
dataTrain<-na.exclude(dataTrain)

library(rpart) 
library(rpart.plot)
fit1 <- rpart(classe~.,data=dataTrain[,-1]) 
fit1
summary(fit1)

library(tree)

fit <-tree(classe~.,data=dataTrain[,-1]) 
summary(fit)


train_control<- trainControl(method="cv", number=10)
model<- train(classe~., data=dataTrain, trControl=train_control, method="C5.0Rules")
model
predictions<- predict(model,dataTest)
pred<- cbind(dataTest,predictions)

confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 

pred <-predict(fit,dataTest[,-1],type='class') 
confusionMatrix(pred,dataTest$classe)
model<- train(classe~., data=dataTrain, trControl=train_control, method="ctree2") model


predictions<- predict(model,dataTest)


pred<- cbind(dataTest,predictions)



confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix









