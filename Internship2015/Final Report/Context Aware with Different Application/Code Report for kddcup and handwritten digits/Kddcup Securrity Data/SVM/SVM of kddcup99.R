# SVM method to classify kddcup99 data
library("e1071")
kddcup <- read.csv("C:/Users/Christina/Desktop/kddcup.data_10_percent_corrected", header=FALSE)
kddcup=as.matrix(kddcup)
error_rate=matrix(0,nrow=5,ncol=20)
#we run the algorithm 5 times with the cost parameter changing from 1 to 100
for(n in (1:5)){
kddcup_1=sample((nrow(kddcup)),size=5000,replace = FALSE, prob = NULL)
kddcup_2=sample((nrow(kddcup)),size=2000,replace = FALSE, prob = NULL)
kddcup_train=kddcup[kddcup_1,]
kddcup_test=kddcup[kddcup_2,]
for (i in 1:2000){
  if (kddcup_test[i,42]=="normal."){
    kddcup_test[i,42]=0}
  else{
    kddcup_test[i,42]=1
  }
}
for (i in 1:5000){
  if (kddcup_train[i,42]=="normal."){
    kddcup_train[i,42]=0}
  else{
    kddcup_train[i,42]=1
  }
}
kddcup_train=as.data.frame(kddcup_train)
kddcup_test=as.data.frame(kddcup_test)
colnames(kddcup_train)=c(paste("x.",1:41,sep=""),"Y")
colnames(kddcup_test)=c(paste("x.",1:41,sep=""),"Y")
#process the data in order to apply the svm method
for (i in 1:41){
  kddcup_train[,i]=as.numeric(kddcup_train[,i])
  kddcup_test[,i]=as.numeric(kddcup_test[,i])
}
kddcup_test[,42]=as.factor(kddcup_test[,42])
kddcup_train[,42]=as.factor(kddcup_train[,42])
kddcup_train1=subset(kddcup_train,select=-c(x.7,x.21,x.9,x.11,x.15,x.22,x.20,x.14,x.18))
kddcup_test1=subset(kddcup_test,select=-c(x.7,x.21,x.9,x.11,x.15,x.22,x.20,x.14,x.18))
for (j in 1:20)
{#we choose cost from one to one hundred
  m=seq(1,100,by=5)
  c=m[j]
model_svm=svm(kddcup_train$Y~.,method="class",data=kddcup_train1,cost=c)
pre_svm=predict(model_svm,newdata=kddcup_test1,type="class")
error_rate[n,j]=sum(kddcup_test[,42]!=pre_svm)/nrow(kddcup_test)}
}
#plot the result to find
mean_error_rate=as.matrix(colMeans(error_rate))
cost.optimal=apply(mean_error_rate,2,which.min)
m=as.matrix(m)
matplot(m,cbind(t(error_rate),mean_error_rate),type='l',col=c('black','red','green','blue','yellow','purple'),ylab='error rate',xlab='cost',lty =1,cex=2)
title(main='error rate vs cost parameter')
legend(legend=c('Trial 1','Trial 2','Trial 3','Trial 4','Trial 5','Average'),col=c('black','red','green','blue','yellow','purple'),'topright',lty=1)
abline(v=cost.optimal)
text(x=11,y=0.7,label='optimal_cost=1')
# we find the optimal cost is 1.The we apply this to large training set and testing set( 20000 and 5000)

kddcup_1=sample((nrow(kddcup)),size=20000,replace = FALSE, prob = NULL)
kddcup_2=sample((nrow(kddcup)),size=5000,replace = FALSE, prob = NULL)
kddcup_train=kddcup[kddcup_1,]
kddcup_test=kddcup[kddcup_2,]
for (i in 1:5000){
  if (kddcup_test[i,42]=="normal."){
    kddcup_test[i,42]=0}
  else{
    kddcup_test[i,42]=1
  }
}
for (i in 1:20000){
  if (kddcup_train[i,42]=="normal."){
    kddcup_train[i,42]=0}
  else{
    kddcup_train[i,42]=1
  }
}
kddcup_train=as.data.frame(kddcup_train)
kddcup_test=as.data.frame(kddcup_test)
colnames(kddcup_train)=c(paste("x.",1:41,sep=""),"Y")
colnames(kddcup_test)=c(paste("x.",1:41,sep=""),"Y")
#process the data in order to apply the svm method
for (i in 1:41){
  kddcup_train[,i]=as.numeric(kddcup_train[,i])
  kddcup_test[,i]=as.numeric(kddcup_test[,i])
}
kddcup_test[,42]=as.factor(kddcup_test[,42])
kddcup_train[,42]=as.factor(kddcup_train[,42])
kddcup_train2=subset(kddcup_train,select=-c(x.7,x.21,x.9,x.11,x.15,x.22,x.20,x.14,x.18))
kddcup_test2=subset(kddcup_test,select=-c(x.7,x.21,x.9,x.11,x.15,x.22,x.20,x.14,x.18))
model_svm=svm(kddcup_train2$Y~.,method="class",data=kddcup_train2,cost=1)
start_time=proc.time()
pre_svm2=predict(model_svm,newdata=kddcup_test2,type="class")
end_time=proc.time()
run_time=end_time-start_time
error_rate=sum(kddcup_test[,42]!=pre_svm2)/nrow(kddcup_test)
> error_rate
[1] 0.0042
> run_time
user  system elapsed 
0.19    0.00    0.19 
