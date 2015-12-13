# random forest classification of the kddcup data

library("randomForest", lib.loc="~/R/win-library/3.2")
kddcup <- read.csv("C:/Users/Christina/Desktop/kddcup.data_10_percent_corrected", header=FALSE)
kddcup=as.matrix(kddcup)
kddcup_1=sample((nrow(kddcup)),size=20000,replace = FALSE, prob = NULL)
kddcup_2=sample((nrow(kddcup)),size=5000,replace = FALSE, prob = NULL)
#Process of the data
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
for (i in 1:41){
  kddcup_train[,i]=as.numeric(kddcup_train[,i])
  kddcup_test[,i]=as.numeric(kddcup_test[,i])
}
kddcup_test[,42]=as.factor(kddcup_test[,42])
kddcup_train[,42]=as.factor(kddcup_train[,42])
colnames(kddcup_train)=c(paste("x.",1:41,sep=""),"Y")
colnames(kddcup_test)=c(paste("x.",1:41,sep=""),"Y")
#build the model
rf=randomForest(kddcup_train$Y~.,kddcup_train,importance=TRUE)
plot(rf,type="p",cex=0.5)
print(rf)
imp=importance(rf)
varImpPlot(rf)
#find the optimal mtry paraeter
trf=tuneRF(kddcup_train[,-42],kddcup_train$Y,ntreeTry=500,improve=0.01)
#we find that the optimal mtry is 12
rf1=randomForest(kddcup_train$Y~.,kddcup_train,importance=TRUE,mtry=12)
rf1
varImpPlot(rf1)
start_time=proc.time()
pre_test=predict(rf1,kddcup_test[,-42])
end_time=proc.time()
run_time=end_time-start_time
error_rate=sum(pre_test!=kddcup_test$Y)/nrow(kddcup_test)









