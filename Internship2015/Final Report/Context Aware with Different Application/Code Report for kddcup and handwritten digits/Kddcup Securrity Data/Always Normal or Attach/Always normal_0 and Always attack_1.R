#always 1 and 0 to classify the kddcup99 data
kddcup <- read.csv("C:/Users/Christina/Desktop/kddcup.data_10_percent_corrected", header=FALSE)
kddcup=as.matrix(kddcup)
kddcup_1=sample((nrow(kddcup)),size=20000,replace = FALSE, prob = NULL)
kddcup_test=kddcup[kddcup_1,]
for (i in 1:20000){
  if (kddcup_test[i,42]=="normal."){
    kddcup_test[i,42]=0}
  else{
    kddcup_test[i,42]=1
  }
}
kddcup_test=as.data.frame(kddcup_test)
colnames(kddcup_test)=c(paste("x.",1:41,sep=""),"Y")
pre_0=matrix(0,ncol=1,nrow=20000)#always 0
pre_1=matrix(1,ncol=1,nrow=20000)#always 1
error_rate_0=sum(kddcup_test[,42]!=pre_0)/nrow(kddcup_test)
error_rate_1=sum(kddcup_test[,42]!=pre_1)/nrow(kddcup_test)






