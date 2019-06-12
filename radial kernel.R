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
#Radial Kernel
model.svm=svm(Class~.,data=training_set)
summary(model.svm)
rad=tune(svm,Class~.,data=training_set,kernel='radial',ranges=list(cost= 10^(-5:2),gamma=10^(-5:2)))
rad
plot2=ggplot(rad$performances,aes(x=cost,y=gamma,color=factor(error)))+geom_jitter(size=3)  +labs(title='Hyperparamter Tuning for Radial Function') +theme_economist()
plot2$labels$colour='Error' 
plot2=ggplotly(plot2,tooltip=c('cost','gamma'))
plot2
fitted.svm.radial=svm(Class~.,data=training_set,cost=10,gamma=0.001,probability=TRUE)
#Predicition and ROC
pred_radial=predict(fitted.svm.radial,newdata=test_set,type='response',probability = TRUE) 
pred_radial
confuse=confusionMatrix(test_set$Class,pred_radial,positive='R')
confuse
svm.roc <- prediction(attributes(pred_radial)$probabilities[,2], test_set$Class) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
plot(svm.auc,type='o',main='Receiver Operator Curve for Radial Kernel SVM, cost=10, gamma=0.01') 
perf <- performance(svm.roc, "auc") 
perf@y.values[[1]]
  