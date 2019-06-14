library('e1071')
library(readxl)
library(ROCR) 
library(dplyr)
library(data.table)
library(caTools)
library(caret)
library(AUC)
library(plotly)
library(ggthemes)
file=read_excel('RS.xlsx')
file=as.data.frame(file)
working=file[,-1]
rownames(working)=file[,1]
setnames(working,'S/R','Class')
working$Class=factor(working$Class,levels=c('S','R'))
#View(working)
#Training and Testing Data Set Creation
set.seed(123)
split=sample.split(working$Class,SplitRatio = 0.8)
training_set=subset(working,split==TRUE)
test_set=subset(working,split==FALSE)
#Sigmoidal
svm.sigmoid=svm(Class~.,data=training_set,kernel='sigmoid',coef0=-256,probability=TRUE)
svm.sigmoid
sig.tune=tune(svm,Class~.,data=training_set,kernel='sigmoid',ranges=list(coef0=c(-256,128,64,32,0,32,64,128)))
sig.tune
#Predicition and ROC
pred_sig=predict(svm.sigmoid,newdata=test_set,type='prob',probability = TRUE) 
pred_sig
confuse=confusionMatrix(test_set$Class,pred_sig,positive='R')
confuse
svm.roc <- prediction(attributes(pred_sig)$probabilities[,2], test_set$Class) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
plot(svm.auc,type='o',main='Receiver Operator Curve for Sigmoid Kernel SVM, coef0=-256') 
perf <- performance(svm.roc, "auc") 
perf@y.values[[1]]
#not a good method