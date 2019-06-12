library(readxl)
file=read_excel('RS.xlsx')
file=as.data.frame(file)
library(dplyr)
working=file[,-1]
rownames(working)=file[,1]
library(data.table)
setnames(working,'S/R','Class')
working$Class=factor(working$Class,levels=c('S','R'))
#View(working)
#Training and Testing Data Set Creation
library(caTools)
set.seed(123)
split=sample.split(working$Class,SplitRatio = 0.8)
training_set=subset(working,split==TRUE)
test_set=subset(working,split==FALSE)

#Model Fitting Logistic Regression and Check which are important features
library(caret)
parameter=trainControl(method='cv',number=10)
model=train(Class~.,data=training_set,method='glm',family=binomial(link='logit'), trControl=parameter)
smry=summary(model)
smry$coefficients #42imp ones

rank=varImp(model)
dim(rank$importance)
rank$importance #42imp ones

#Validaton With Test 
pred_prob=predict(model,newdata=test_set,type='raw')
cm=table(test_set$Class,pred_prob)
cm
ala=confusionMatrix(pred_prob,test_set$Class,positive='R')
ala
#ROC & AUC
library(ROCR)
pred_prob1=predict(model,newdata=test_set,type='prob')
pred=prediction(pred_prob1[,1],test_set$Class)
rocs=performance(pred,'tpr','fpr')
plot(rocs,main='Receiver Operator Curve for Logistic Regression')
library(AUC)
perf <- performance(pred, "auc") 
perf@y.values[[1]]
