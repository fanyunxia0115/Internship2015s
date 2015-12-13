#logistic model of kddcup99 classification 
#this is the package we used
library("glmnet")
#import the data into the R studio
kddcup <- read.csv("C:/Users/Christina/Desktop/kddcup.data_10_percent_corrected", header=FALSE)
# we sample the data to get 5000 training data and 2000 test data
kddcup=as.matrix(kddcup)
data=function(kddcup){
kddcup_1=sample((nrow(kddcup)),size=5000,replace = FALSE, prob = NULL)
kddcup_2=sample((nrow(kddcup)),size=2000,replace = FALSE, prob = NULL)
kddcup_train=kddcup[kddcup_1,]
kddcup_test=kddcup[kddcup_2,]
#process the data in order to apply the logistic model
# process the last column into two parts
for (i in 1:2000){
  if (kddcup_test[i,42]=="normal."){
    kddcup_test[i,42]=1}
  else{
    kddcup_test[i,42]=2
  }
}
for (i in 1:5000){
  if (kddcup_train[i,42]=="normal."){
    kddcup_train[i,42]=1}
  else{
    kddcup_train[i,42]=2
  }
}
kddcup_train=as.data.frame(kddcup_train)
kddcup_test=as.data.frame(kddcup_test)
colnames(kddcup_train)=c(paste("x.",1:41,sep=""),"Y")
colnames(kddcup_test)=c(paste("x.",1:41,sep=""),"Y")
#process the data in order to apply the svm method
#process the x and through this way we can transfer the characters into numbers
for (i in 1:41){
  kddcup_train[,i]=as.numeric(kddcup_train[,i])
  kddcup_test[,i]=as.numeric(kddcup_test[,i])
}

kddcup_train[,42]=as.factor(kddcup_train[,42])
kddcup_train[,2]=as.factor(kddcup_train[,2])
kddcup_train[,3]=as.factor(kddcup_train[,3])
kddcup_train[,4]=as.factor(kddcup_train[,4])
kddcup_train[,7]=as.factor(kddcup_train[,7])
kddcup_train[,12]=as.factor(kddcup_train[,12])
kddcup_train[,21]=as.factor(kddcup_train[,21])
kddcup_train[,22]=as.factor(kddcup_train[,22])
kddcup_test[,42]=as.factor(kddcup_test[,42])
kddcup_test[,2]=as.factor(kddcup_test[,2])
kddcup_test[,3]=as.factor(kddcup_test[,3])
kddcup_test[,4]=as.factor(kddcup_test[,4])
kddcup_test[,7]=as.factor(kddcup_test[,7])
kddcup_test[,12]=as.factor(kddcup_test[,12])
kddcup_test[,21]=as.factor(kddcup_test[,21])
kddcup_test[,22]=as.factor(kddcup_test[,22])
kddcup_train=data.matrix(kddcup_train)
kddcup_test=data.matrix(kddcup_test)
kddcup_train_test=rbind(kddcup_train,kddcup_test)
return (kddcup_train_test)
}
# first we use the generalized linear model and cross validation to deternime the optimal lambda
#we run the algorithm 5 times
a1=data(kddcup)
a2=data(kddcup)
a3=data(kddcup)
a4=data(kddcup)
a5=data(kddcup)
cv_model1=cv.glmnet(a1[1:5000,-42],a1[1:5000,42],family="binomial")
cv_model2=cv.glmnet(a2[1:5000,-42],a2[1:5000,42],family="binomial")
cv_model3=cv.glmnet(a3[1:5000,-42],a3[1:5000,42],family="binomial")
cv_model4=cv.glmnet(a4[1:5000,-42],a4[1:5000,42],family="binomial")
cv_model5=cv.glmnet(a5[1:5000,-42],a5[1:5000,42],family="binomial")
#plot the figures to find optimal lambda
par(mfrow=c(2,3))
plot(cv_model1,pch=19,col="red")
legend("topleft",legend="Trial 1",col="red",pch=19)
plot(cv_model2,pch=19,col="green")
legend("topleft",legend="Trial 2",col="red",pch=19)
plot(cv_model3,pch=19,col="blue")
legend("topleft",legend="Trial 3",col="red",pch=19)
plot(cv_model4,pch=19,col="grey")
legend("topleft",legend="Trial 4",col="red",pch=19)
plot(cv_model5,pch=19,col="yellow")
legend("topleft",legend="Trial 5",col="red",pch=19)
plot(log(cv_model2$lambda),cv_model2$cvm,pch=19,col="red",cex=0.1)
points(log(cv_model2$lambda),cv_model2$cvm,pch=19,col="green",cex=0.1)
points(log(cv_model3$lambda),cv_model3$cvm,pch=19,col="blue",cex=0.1)
points(log(cv_model4$lambda),cv_model4$cvm,pch=19,col="grey",cex=0.1)
points(log(cv_model5$lambda),cv_model5$cvm,pch=19,col="yellow",cex=0.1)
legend("topleft",legend=c('Trial 1','Trial 2','Trial 3','Trial 4','Trial 5'),col=c('red','green','blue','grey','yellow'),pch=19)
abline(v=log((cv_model1$lambda.min+cv_model2$lambda.min+cv_model3$lambda.min+cv_model4$lambda.min+cv_model5$lambda.min)/5))
text(x=-6.5,y=0.4,label="optimal_lambda=0.000397")

# we train the model with a larger training data,say 20000 training data and 5000 testing data
kddcup_1=sample((nrow(kddcup)),size=20000,replace = FALSE, prob = NULL)
kddcup_2=sample((nrow(kddcup)),size=5000,replace = FALSE, prob = NULL)
kddcup_train=kddcup[kddcup_1,]
kddcup_test=kddcup[kddcup_2,]
#process the data in order to apply the logistic model
# process the last column into two parts
for (i in 1:5000){
  if (kddcup_test[i,42]=="normal."){
    kddcup_test[i,42]=1}
  else{
    kddcup_test[i,42]=2
  }
}
for (i in 1:20000){
  if (kddcup_train[i,42]=="normal."){
    kddcup_train[i,42]=1}
  else{
    kddcup_train[i,42]=2
  }
}
kddcup_train=as.data.frame(kddcup_train)
kddcup_test=as.data.frame(kddcup_test)
colnames(kddcup_train)=c(paste("x.",1:41,sep=""),"Y")
colnames(kddcup_test)=c(paste("x.",1:41,sep=""),"Y")
#process the data in order to apply the svm method
#process the x and through this way we can transfer the characters into numbers
for (i in 1:41){
  kddcup_train[,i]=as.numeric(kddcup_train[,i])
  kddcup_test[,i]=as.numeric(kddcup_test[,i])
}

kddcup_train[,42]=as.factor(kddcup_train[,42])
kddcup_train[,2]=as.factor(kddcup_train[,2])
kddcup_train[,3]=as.factor(kddcup_train[,3])
kddcup_train[,4]=as.factor(kddcup_train[,4])
kddcup_train[,7]=as.factor(kddcup_train[,7])
kddcup_train[,12]=as.factor(kddcup_train[,12])
kddcup_train[,21]=as.factor(kddcup_train[,21])
kddcup_train[,22]=as.factor(kddcup_train[,22])
kddcup_test[,42]=as.factor(kddcup_test[,42])
kddcup_test[,2]=as.factor(kddcup_test[,2])
kddcup_test[,3]=as.factor(kddcup_test[,3])
kddcup_test[,4]=as.factor(kddcup_test[,4])
kddcup_test[,7]=as.factor(kddcup_test[,7])
kddcup_test[,12]=as.factor(kddcup_test[,12])
kddcup_test[,21]=as.factor(kddcup_test[,21])
kddcup_test[,22]=as.factor(kddcup_test[,22])
kddcup_train=data.matrix(kddcup_train)
kddcup_test=data.matrix(kddcup_test)
kddcup_train_test=rbind(kddcup_train,kddcup_test)
#get the model
model=glmnet(kddcup_train[1:20000,-42],kddcup_train[1:20000,42],family="binomial")
#do test on test data
start_time=proc.time()
pre.test=predict(model,kddcup_test[1:5000,-42],0.000397,"class")
end_time=proc.time()
run_time=end_time-start_time
errorrate.test=sum((pre.test!=kddcup_test[,42]))/nrow(kddcup_test)
#
kddcup_train_x=data.matrix(subset(kddcup_train,select=-Y))
kddcup_train_y=subset(kddcup_train,select=Y)
kddcup_test_x=subset(kddcup_test,select=-Y)
kddcup_test_y=subset(kddcup_test,select=Y)
model=glmnet(kddcup_train_x,kddcup_train_y,"binomial")
> errorrate.test
[1] 0.0084
> run_time
user  system elapsed 
0.02    0.03    2.66 
