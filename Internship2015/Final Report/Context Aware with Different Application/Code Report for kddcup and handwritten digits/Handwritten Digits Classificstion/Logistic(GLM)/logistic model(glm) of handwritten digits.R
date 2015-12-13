# use the logistic model to classify the handwritten digits
data=load("C:/Users/Christina/Desktop/digitsdata.RData")
image(t(1 - training.data[3,1,,])[,20:1],col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
num.class <- dim(training.data)[1]  # Number of classes
num.training <- dim(training.data)[2]  # Number of training data per class
d <- prod(dim(training.data)[3:4]) # Dimension of each training image (rowsxcolumns)
num.test <- dim(test.data)[2] # Number of test data
dim(training.data) <- c(num.class * num.training, d) # Reshape training data to 2-dim matrix
dim(test.data) <- c(num.class * num.test, d) # Same for test.
training.label <- rep(0:9, num.training)# Labels of training data.
test.label <- rep(0:9, num.test) # Labels of test data
#install glmnet packages
library("glmnet")
rownames(training.data)=training.label
#divide training data into 2 parts:training and validation to compute optinal lambda
error_rate=matrix(0,nrow=5,ncol=100)
lambda=matrix(0,nrow=5,ncol=100)
for(i in 1:5){
  # this is because we want to run the whole data 5 times
  # we sample rows aaccording to their labels and get the corresponding traing data and test data.
  #class 0
  class0=training.data[rownames(training.data)=="0",]
  train_0=sample((nrow(class0)),size=400,replace = FALSE, prob = NULL)
  train0=class0[train_0, ]
  test0=class0[-train_0, ]
  #class 1
  class1=training.data[rownames(training.data)=="1",]
  train_1=sample(nrow(class1),size=400,replace = FALSE, prob = NULL)
  train1=class1[train_1, ]
  test1=class1[-train_1, ]
  #class 2
  class2=training.data[rownames(training.data)=="2",]
  train_2=sample(nrow(class2), size=400,replace = FALSE, prob = NULL)
  train2=class2[train_2, ]
  test2=class2[-train_2, ]
  # class 3
  class3=training.data[rownames(training.data)=="3",]
  train_3=sample(nrow(class3), size=400,replace = FALSE, prob = NULL)
  train3=class3[train_3, ]
  test3=class3[-train_3, ]
  #class 4
  class4=training.data[rownames(training.data)=="4",]
  train_4=sample(nrow(class4), size=400,replace = FALSE, prob = NULL)
  train4=class4[train_4, ]
  test4=class4[-train_4, ]
  #class 5
  class5=training.data[rownames(training.data)=="5",]
  train_5=sample(nrow(class5), size=400,replace = FALSE, prob = NULL)
  train5=class5[train_5, ]
  test5=class5[-train_5, ]
  #class 6
  class6=training.data[rownames(training.data)=="6",]
  train_6=sample(nrow(class6), size=400,replace = FALSE, prob = NULL)
  train6=class6[train_6, ]
  test6=class6[-train_6, ]
  #class 7
  class7=training.data[rownames(training.data)=="7",]
  train_7=sample(nrow(class7), size=400,replace = FALSE, prob = NULL)
  train7=class7[train_7, ]
  test7=class7[-train_7, ]
  # class 8
  class8=training.data[rownames(training.data)=="8",]
  train_8=sample(nrow(class8), size=400,replace = FALSE, prob = NULL)
  train8=class8[train_8, ]
  test8=class8[-train_8, ]
  #class 9
  class9=training.data[rownames(training.data)=="9",]
  train_9=sample(nrow(class9), size=400,replace = FALSE, prob = NULL)
  train9=class9[train_9, ]
  test9=class9[-train_9, ]
  
  train=rbind(train0,train1,train2,train3,train4,train5,train6,train7,train8,train9)
  test=rbind(test0,test1,test2,test3,test4,test5,test6,test7,test8,test9)
  
  #do logistic regression on the sampled training set
  glmmodel=glmnet(train,rownames(train),"multinomial")
  lambda[i,]=t(glmmodel$lambda)
  for(j in 1:length(glmmodel$lambda)){
    test_pre=predict(glmmodel,test,glmmodel$lambda[j],"class")
    error_rate[i,j]=sum(test_pre!=rownames(test))/nrow(test)
  }
}
mean_error_rate=colMeans(error_rate)
lambda.optimal=mean(lambda[,apply(t(mean_error_rate),1,which.min)])
m=as.matrix(colMeans(lambda))
matplot(cbind(t(lambda),m),cbind(t(error_rate),mean_error_rate),type='l',col=c('black','red','green','blue','yellow','purple'),ylab='error rate',xlab='lambda',lty =1,cex=2)
title(main='error rate vs lambda parameter')
legend(legend=c('Trial 1','Trial 2','Trial 3','Trial 4','Trial 5','Average'),col=c('black','red','green','blue','yellow','purple'),'bottomright',lty=1)
abline(v=lambda.optimal)
text(x=0.03,y=0.6,label='optimal_lambda=0.00117')
plot(glmmodel,xvar="lambda",label=TRUE)
plot(glmmodel,label=TRUE)
plot(glmmodel,xvar="dev",label=TRUE)

# we get lambda.optimal=0.00117 

#Then we do logistic regression on the whole training data
model=glmnet(training.data,training.label,"multinomial")
#do test on test data
start_time=proc.time()
pre.test=predict(model,test.data,lambda.optimal,"class")
end_time=proc.time()
run_time=end_time-start_time
errorrate.test=sum((pre.test!=test.label))/nrow(test.data)
> errorrate.test
[1] 0.1308
> run_time
user  system elapsed 
0.72    0.25    1.81
