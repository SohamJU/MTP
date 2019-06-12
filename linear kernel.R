library('e1071')
library(readxl)
library(ROCR) 
library(dplyr)
library(data.table)
library(caTools)
library(caret)
library(AUC)
library(plotly)
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
#Linear Kernel
svm.linear=svm(Class~.,data=training_set,kernel='linear')
svm.linear
new=tune(svm,Class~.,data=training_set,kernel='linear',ranges=list(cost= 10^(-5:2)))
new
summary(tune.linear)
plot1=ggplot(new$performances,aes(x=cost,y=dispersion))+geom_point(aes(color=error),size=4) 
plot1=ggplotly(plot1)
plot1
#Prediction and ROC
fitted.svm.linear=svm(Class~.,data=training_set,kernel='linear',cost=0.01,probability=TRUE)
summary(fitted.svm.linear)plot(svm.auc,type='o',main='Receiver Operator Curve for Linear Kernel SVM at cost=0.01') 

pred_linear=predict(fitted.svm.linear,newdata=test_set,decision.values = TRUE, probability = TRUE) 
confuse=confusionMatrix(test_set$Class,pred_linear,positive='R')
confuse
svm.roc <- prediction(attributes(pred_linear)$probabilities[,2], test_set$Class) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
perf <- performance(svm.roc, "auc") 
perf@y.values[[1]]


