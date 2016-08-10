#plotting ROC curves in R
require(ROCR)

#creating our ROC cruve plotting function
rocplot<-function(pred,truth,...){
      predob<-prediction(pred,truth)
      perf<-performance(predob,"tpr","fpr")
      plot(perf,...)
      abline(c(0,1),lty=2)
}

#creating sample data
set.seed(1)
x<-matrix(rnorm(200*2),ncol=2)
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
y<-c(rep(1,150),rep(2,50))
dat<-data.frame(x=x,y=as.factor(y))

#creating the two models we want to compare
#these are just two possible models, create any classification models you want
#IMPORTANT: you must add the argument 'decision.values=TRUE' in order to save the fitted values from the SVM
#these fitted values are for use in the predict() function later
require(e1071)
svm1<-svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=TRUE)
svm2<-svm(y~.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values=TRUE)

#creating predictions on the training and test data
predtrain1<-attributes(predict(svm1,dat[train,],decision.values=TRUE))$decision.values
predtrain2<-attributes(predict(svm2,dat[train,],decision.values=TRUE))$decision.values
predtest1<-attributes(predict(svm1,dat[-train,],decision.values=TRUE))$decision.values
predtest2<-attributes(predict(svm2,dat[-train,],decision.values=TRUE))$decision.values

#finally making our ROC curves
#use the argument 'add=TRUE' in the rocplot() function to tell it to add the curve to the existing plot
par(mfrow=c(1,2))
rocplot(predtrain1,dat[train,"y"],main="Training Data")
rocplot(predtrain2,dat[train,"y"],add=T,col="red")
rocplot(predtest1,dat[-train,"y"],main="Test Data")
rocplot(predtest2,dat[-train,"y"],add=T,col="red")