# use the LDA method to classify the handwritten digits

data=load("C:/Users/Christina/Desktop/digitsdata.RData")
image(t(1 - training.data[3,1,,])[,20:1],col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
num.class <- dim(training.data)[1]  # Number of classes
num.training <- dim(training.data)[2]  # Number of training data per class
d <- prod(dim(training.data)[3:4]) # Dimension of each training image (rowsxcolumns)
num.test <- dim(test.data)[2] # Number of test data
dim(training.data) <- c(num.class * num.training, d) # Reshape training data to 2-dim matrix
dim(test.data) <- c(num.class * num.test, d) # Same for test.
training.label <- rep(0:9, num.training) # Labels of training data.
test.label <- rep(0:9, num.test) # Labels of test data

# for the conveniece of dealing with data, we try to set training label as the rownames of the
#training data 
rownames(training.data)=training.label
#return the ax+b
lda=function (mean,sigma,x){
  Isigma=solve(sigma )
  b=-(0.5*((t(mean)%*%Isigma)%*%mean))[1,1]-1
  a=t(mean)%*%Isigma
  return (x%*%t(a)+b)
}
#divide the training set per class into 2 parts:400 training examples and 100 remaing ones to test
#compute the corresponding covariance matrix with different lambda:get the value from 0 to 1
#run the algorithm five times on a random subset of 400 training examples per class
#evaluate the error on the remaining 100 examples per class
#obtain the optimal lamda that minimize the average error of the 5 times
error_rate=matrix(NA,nrow=5,ncol=25)
start_time= proc.time()
for(i in 1:5){
  # this is because we want to run the whole data 5 times
  # we sample rows aaccording to their labels and get the corresponding traing data and test data.
  #then we get the mean and cov
  #class 0
  class0=training.data[rownames(training.data)=="0",]
  train_0=sample((nrow(class0)),size=400,replace = FALSE, prob = NULL)
  train0=class0[train_0, ]
  test0=class0[-train_0, ]
  cov0=cov(train0)
  #class 1
  class1=training.data[rownames(training.data)=="1",]
  train_1=sample(nrow(class1),size=400,replace = FALSE, prob = NULL)
  train1=class1[train_1, ]
  test1=class1[-train_1, ]
  cov1=cov(train1)
  mean1=colMeans(train1)
  #class 2
  class2=training.data[rownames(training.data)=="2",]
  train_2=sample(nrow(class2), size=400,replace = FALSE, prob = NULL)
  train2=class2[train_2, ]
  test2=class2[-train_2, ]
  cov2=cov(train2)
  mean2=colMeans(train2)
  # class 3
  class3=training.data[rownames(training.data)=="3",]
  train_3=sample(nrow(class3), size=400,replace = FALSE, prob = NULL)
  train3=class3[train_3, ]
  test3=class3[-train_3, ]
  cov3=cov(train3)
  mean3=colMeans(train3)
  #class 4
  class4=training.data[rownames(training.data)=="4",]
  train_4=sample(nrow(class4), size=400,replace = FALSE, prob = NULL)
  train4=class4[train_4, ]
  test4=class4[-train_4, ]
  cov4=cov(train4)
  mean4=colMeans(train4)
  #class 5
  class5=training.data[rownames(training.data)=="5",]
  train_5=sample(nrow(class5), size=400,replace = FALSE, prob = NULL)
  train5=class5[train_5, ]
  test5=class5[-train_5, ]
  cov5=cov(train5)
  mean5=colMeans(train5)
  #class 6
  class6=training.data[rownames(training.data)=="6",]
  train_6=sample(nrow(class6), size=400,replace = FALSE, prob = NULL)
  train6=class6[train_6, ]
  test6=class6[-train_6, ]
  cov6=cov(train6)
  mean6=colMeans(train6)
  #class 7
  class7=training.data[rownames(training.data)=="7",]
  train_7=sample(nrow(class7), size=400,replace = FALSE, prob = NULL)
  train7=class7[train_7, ]
  test7=class7[-train_7, ]
  cov7=cov(train7)
  mean7=colMeans(train7)
  # class 8
  class8=training.data[rownames(training.data)=="8",]
  train_8=sample(nrow(class8), size=400,replace = FALSE, prob = NULL)
  train8=class8[train_8, ]
  test8=class8[-train_8, ]
  cov8=cov(train8)
  mean8=colMeans(train8)
  #class 9
  class9=training.data[rownames(training.data)=="9",]
  train_9=sample(nrow(class9), size=400,replace = FALSE, prob = NULL)
  train9=class9[train_9, ]
  test9=class9[-train_9, ]
  cov9=cov(train9)
  mean9=colMeans(train9)
  # we use the covariance mean as the whole same covarice
  cov_mean=(cov0+cov1+cov2+cov3+cov4+cov5+cov6+cov7+cov8+cov9)/10
  test=rbind(test0,test1,test2,test3,test4,test5,test6,test7,test8,test9)
  #
  for(j in 1:25){
    m=seq(0.02,0.5,by=0.02)
    lamda=m[j]
    new_cov=(1-lamda)*cov_mean+lamda*diag(1/4,nrow=400, ncol=400)
    #predict the test data for every mean
    pre_test0=lda(mean0,new_cov,test)
    pre_test1=lda(mean1,new_cov,test)
    pre_test2=lda(mean2,new_cov,test)
    pre_test3=lda(mean3,new_cov,test)
    pre_test4=lda(mean4,new_cov,test)
    pre_test5=lda(mean5,new_cov,test)
    pre_test6=lda(mean6,new_cov,test)
    pre_test7=lda(mean7,new_cov,test)
    pre_test8=lda(mean8,new_cov,test)
    pre_test9=lda(mean9,new_cov,test)
    #For every row, return the largest column index that has largest a*x+b
    pre_test00=cbind(pre_test0,pre_test1,pre_test2,pre_test3,pre_test4,pre_test5,pre_test6,pre_test7,pre_test8,pre_test9)
    pre_test=apply(pre_test00,1,which.max)
    error_rate[i,j]=sum((pre_test-1)!=rownames(test))/nrow(test)
  }
}
end_time= proc.time()
run_time=end_time-start_time
#draw a picture to find the optimal lambda
mean_error_rate=as.matrix(colMeans(error_rate))
lamda_optimal=0.02*(apply(mean_error_rate,2,which.min))
m=as.matrix(m)
matplot(m,cbind(t(error_rate),mean_error_rate),type='l',col=c('black','red','green','blue','yellow','purple'),ylab='error rate',xlab='lambda',lty =1,cex=2)
title(main='error rate vs lambda')
legend(legend=c('Trial 1','Trial 2','Trial 3','Trial 4','Trial 5','Average'),col=c('black','red','green','blue','yellow','purple'),'bottomright',lty=1)
abline(v=lamda_optimal)
text(x=0.08,y=0.135,label='optimal_lambda=0.06')
# the optimal lambda we get is 0.06 and the error rate we get of the training data is 0.125

#######################################################
# we use theoptimal lambda we get to train the whole data and do test on the test data
rownames(test.data)=test.label
#get the variance and mean
Cov02=cov(training.data[rownames(training.data)=="0",])
Cov12=cov(training.data[rownames(training.data)=="1",])
Cov22=cov(training.data[rownames(training.data)=="2",])
Cov32=cov(training.data[rownames(training.data)=="3",])
Cov42=cov(training.data[rownames(training.data)=="4",])
Cov52=cov(training.data[rownames(training.data)=="5",])
Cov62=cov(training.data[rownames(training.data)=="6",])
Cov72=cov(training.data[rownames(training.data)=="7",])
Cov82=cov(training.data[rownames(training.data)=="8",])
Cov92=cov(training.data[rownames(training.data)=="9",])
cov.training=(Cov02+Cov12+Cov22+Cov32+Cov42+Cov52+Cov62+Cov72+Cov82+Cov92)/10
cov_new=(1-lamda_optimal)*cov.training+lamda_optimal*diag(1/4,nrow=400, ncol=400)
mean02=colMeans(training.data[rownames(training.data)=="0",])
mean12=colMeans(training.data[rownames(training.data)=="1",])
mean22=colMeans(training.data[rownames(training.data)=="2",])
mean32=colMeans(training.data[rownames(training.data)=="3",])
mean42=colMeans(training.data[rownames(training.data)=="4",])
mean52=colMeans(training.data[rownames(training.data)=="5",])
mean62=colMeans(training.data[rownames(training.data)=="6",])
mean72=colMeans(training.data[rownames(training.data)=="7",])
mean82=colMeans(training.data[rownames(training.data)=="8",])
mean92=colMeans(training.data[rownames(training.data)=="9",])

# use the similar method we calculate the errorrate
start_time= proc.time()
pre_test02=lda(mean02,cov_new,test.data)
pre_test12=lda(mean12,cov_new,test.data)
pre_test22=lda(mean22,cov_new,test.data)
pre_test32=lda(mean32,cov_new,test.data)
pre_test42=lda(mean42,cov_new,test.data)
pre_test52=lda(mean52,cov_new,test.data)
pre_test62=lda(mean62,cov_new,test.data)
pre_test72=lda(mean72,cov_new,test.data)
pre_test82=lda(mean82,cov_new,test.data)
pre_test92=lda(mean92,cov_new,test.data)
#For every row, return the largest column index that has largest a*x+b
pre_test=apply(cbind(pre_test02,pre_test12,pre_test22,pre_test32,pre_test42,pre_test52,
                     pre_test62,pre_test72,pre_test82,pre_test92),1,which.max)
end_time= proc.time()
run_time=end_time-start_time
error_rate=sum((pre_test-1)!=rownames(test.data))/nrow(test.data)
#the errorrate we get is 0.1451

