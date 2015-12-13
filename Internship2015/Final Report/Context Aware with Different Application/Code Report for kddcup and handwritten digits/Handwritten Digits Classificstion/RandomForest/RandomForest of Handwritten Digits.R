
# classification of handwritten digits(Random Forest)

#Process the data
library("randomForest", lib.loc="~/R/win-library/3.2")
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
#transfer the data matix into data frame
training.data=as.data.frame(cbind(training.label,training.data))
training.data[,1]=as.factor(training.data[,1])
test.data=as.data.frame(cbind(test.label,test.data))
test.data[,1]=as.factor(test.data[,1])
colnames(training.data)=c("Y",paste("x.",1:400,sep=""))
colnames(test.data)=c("Y",paste("x.",1:400,sep=""))

# train the whole training data with random forest

rf=randomForest(training.data$Y~.,training.data,proximity = TRUE,importance=TRUE)
plot(rf,type="p",cex=0.5)
print(rf)
imp=importance(rf)
varImpPlot(rf)

#find the optimal mtry paraeter

trf=tuneRF(training.data[,-1],training.data$Y,ntreeTry=500)
#note we can not use the following code to find the optimal mtry
cv=rfcv(training.data[,-1],training.data$Y,cv.fold=5)
with(cv, plot(n.var, error.cv, log="x", type="b", lwd=2),col="green")

# Test on the test data
start_time=proc.time()
pre_test=predict(rf,test.data[,-1])
end_time=proc.time()
run_time=start_time-end_time
error_rate=sum(pre_test!=test.label)/nrow(test.data)
> run_time
user  system elapsed 
1.61    0.08    1.78 
> error_rate
[1] 0.0655