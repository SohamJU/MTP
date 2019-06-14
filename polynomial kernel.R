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
#Polynomial Kernel
#poly.svm=svm(Class~.,data=training_set,kernel='polynomial',degree=2,cost=0.01,coef0=0, probability=TRUE)
#summary(poly.svm)
rad=tune(svm,Class~.,data=training_set,kernel='polynomial',ranges=list(cost= 10^(-3:2),degree=c(2:5)))
rad
plot2=ggplot(rad$performances,aes(x=degree,y=cost,color=factor(error)))+geom_jitter(size=3)  +labs(title='Hyperparamter Tuning for Polynomial Function') +theme_economist()
plot2$labels$colour='Error' 
plot2=ggplotly(plot2,tooltip=c('cost','degree'))
plot2
poly.svm=svm(Class~.,data=training_set,kernel='polynomial',degree=2,cost=1000,coef0=0, probability=TRUE)
summary(poly.svm)
 #Predicition and ROC
pred_poly=predict(poly.svm,newdata=test_set,probability = TRUE) 
pred_poly
confuse=confusionMatrix(test_set$Class,pred_poly,positive='R')
confuse
svm.roc <- prediction(attributes(pred_poly)$probabilities[,2], test_set$Class) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
plot(svm.auc,type='o',main='Receiver Operator Curve for Radial Kernel SVM, cost=100, degree=2') 
perf <- performance(svm.roc, "auc") 
perf@y.values[[1]]
#Conclusion