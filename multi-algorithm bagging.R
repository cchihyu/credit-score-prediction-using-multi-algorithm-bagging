data_org=read.csv("C:/Users/qaz12/Desktop/statistical learning/credit_score.csv",head=T)
data=data_org[,c(-1,-2)]

# pre-processing
age_out=which(data$Age%%1!=0)
card_out=which(data$Num_Credit_Card%%1!=0)
account_out=which(data$Num_Bank_Accounts%%1!=0| data$Num_Bank_Accounts<0)
inquire_out=which(data$Num_Credit_Inquiries%%1!=0)
delay_out=which(data$Num_of_Delayed_Payment%%1!=0 | data$Num_of_Delayed_Payment<0)
date_out=which(data$Delay_from_due_date<0)
data_out=Reduce(union,list(age_out,card_out,account_out,inquire_out,delay_out,date_out))
data=data[-data_out,]

# facorize categorical data
data$Occupation=factor(data$Occupation)
data$Type_of_Loan=factor(data$Type_of_Loan)
data$Interest_Rate=factor(data$Interest_Rate)
data$Credit_Score=factor(data$Credit_Score)
data$Payment_of_Min_Amount=factor(data$Payment_of_Min_Amount)
data$Payment_Behaviour=factor(data$Payment_Behaviour)
summary(data)

#eda and summary of data
v0=data[which(data$Credit_Score==0),]
v1=data[which(data$Credit_Score==1),]
v2=data[which(data$Credit_Score==2),]
summary(v0)
summary(v1)
summary(v2)

boxplot(v0$Num_of_Loan,v1$Num_of_Loan,v2$Num_of_Loan,ylab='number of loan',xlab='credict score')
boxplot(v0$Num_Credit_Card,v1$Num_Credit_Card,v2$Num_Credit_Card,ylab='number of credit card',xlab='credict score')

#seperate data into train and test set
set.seed(108021186)
train_idx=c(sample(which(data$Credit_Score==0),800),sample(which(data$Credit_Score==1),800),sample(which(data$Credit_Score==2),800))

train=data[sample(train_idx),-c(2,7,9)]
test=data[-train_idx,-c(2,7,9)]

# svm model

# choosing cost
library(e1071)
set.seed(108021186)
tune.out <- tune(svm, Credit_Score ~ ., data = train, kernel = "linear", 
    ranges = list(cost=c(0.1,1,10,100,1000,10000,100000)))
summary(tune.out)

# model fitting
library(e1071)
set.seed(108021186)
svm_result=1:61003
for (i in 1:20){
    boot=train[sample(1:2400,2400,replace=T),]
    svmfit<-svm(Credit_Score~.,data=boot,kernel="linear", 
    cost=0.1,scale = FALSE)
    svm_result=cbind(svm_result,predict(svmfit,test[,-20]))
    print(i)
}
svm_result_mod=svm_result[,-1]-1

# sub-model2: tree model

# choosing split
library(rpart)
meuse=train[,-c(2,7,9)]
k <- 10; (size <- floor(dim(train_valid)[1]/k))
max.split <- 20 
split.vs.xval <- as.data.frame(matrix(0, nrow=max.split, ncol=2))
names(split.vs.xval) <- c("nsplit", "xerr")
records.permute <- sample(1:dim(train_valid)[1])

for (i.split in 1:max.split) {
  rmse <- rep(0,k)
  for (i in 0:(k-1)) {
    ix <- records.permute[(i*size+1):(i*size+size)]
    # training set to build the tree, test set to evaluate
    meuse.cal <- meuse[-ix,]; meuse.val <- meuse[ix,]
    m.cal <- rpart(Credit_Score~.,data = meuse.cal, maxdepth=i.split, cp=0)
    val <- sum(meuse.val$Credit_Score!=predict(m.cal, meuse.val[,-20]))
    rmse[i+1] <- val/nrow(test)
    }
rmse.m <- mean(rmse)
split.vs.xval[i.split,] = c(i.split, rmse.m)
} # for minsplit
plot(split.vs.xval)

# model fitting
library(rpart)
set.seed(108021186)
tree_result=1:61003
for (i in 1:20){
    boot=train[sample(1:2400,2400,replace=T),]
    treefit<-rpart(Credit_Score~.,data = boot, maxdepth=13, cp=0)
    tree_result=cbind(tree_result,predict(treefit,test[,-20],type='class'))
    print(i)
}
tree_result_mod=tree_result[,-1]-1

# logistic regression

# model fitting
library(nnet)
set.seed(108021186)
lr_result=1:61003
for (i in 1:20){
    boot=train[sample(1:2400,2400,replace=T),]
    lrfit<-nnet::multinom(Credit_Score ~., data = boot)
    lr_result=cbind(lr_result,predict(lrfit,test[,-20],type='class'))
    print(i)
}
lr_result_mod=lr_result[,-1]-1

# QDA classifier

# model fitting
library(MASS)
set.seed(108021186)
qda_result=1:61003
for (i in 1:20){
    boot=train[sample(1:2400,2400,replace=T),]
    qdafit<-qda(Credit_Score~.,data = boot)
    qda_result=cbind(qda_result,predict(qdafit,test[,-20])$class)
    print(i)
}
qda_result_mod=qda_result[,-1]-1


#compute the result using voting
result=function(mat){
  resul=NULL
  for(i in 1:nrow(mat)){
    a0=sum(mat[i,]==0)
    a1=sum(mat[i,]==1)
    a2=sum(mat[i,]==2)
    res=which.max(c(a0,a1,a2))
    resul[i]=res-1
  }
  return(resul)
}

agg_svm=result(svm_result_mod)
table(agg_svm,test$Credit_Score)
mean(agg_svm==test$Credit_Score)

agg_tree=result(tree_result_mod)
table(agg_tree,test$Credit_Score)
mean(agg_tree==test$Credit_Score)

agg_lr=result(lr_result_mod)
table(agg_lr,test$Credit_Score)
mean(agg_lr==test$Credit_Score)

agg_qda=result(qda_result_mod)
table(agg_qda,test$Credit_Score)
mean(agg_qda==test$Credit_Score)

# single accuracy

t1=proc.time()
svmfit<-svm(Credit_Score~.,data=train,kernel="linear", cost=0.1,scale = FALSE)
t2=proc.time()
svm_result=predict(svmfit,test[,-20])

t3=proc.time()
treefit<-rpart(Credit_Score~.,data = train, maxdepth=13, cp=0)
t4=proc.time()
tree_result=predict(treefit,test[,-20],type='class')

t5=proc.time()
lrfit<-nnet::multinom(Credit_Score ~., data = train)
t6=proc.time()
lr_result=predict(lrfit,test[,-20],type='class')

t7=proc.time()
qdafit<-qda(Credit_Score~.,data = train)
t8=proc.time()
qda_result=predict(qdafit,test[,-20])$class

# time cost and accuracy
t2-t1
t4-t3
t6-t5
t8-t7
mean(svm_result==test$Credit_Score)
mean(tree_result==test$Credit_Score)
mean(lr_result==test$Credit_Score)
mean(qda_result==test$Credit_Score)

#correlation define
cor_dist<-function(mat,true){
  cor=matrix(0,ncol(mat),ncol(mat))
  for(i in 1:ncol(mat)){
    for(j in 1:ncol(mat)){
      n11=(mat[,i]==true)%*%(mat[,j]==true)
      n00=(mat[,i]!=true)%*%(mat[,j]!=true) 
      n01=(mat[,i]!=true)%*%(mat[,j]==true) 
      n10=(mat[,i]==true)%*%(mat[,j]!=true)
      cor[i,j]=(n11*n00-n10*n01)/sqrt((n11+n10)*(n00+n01)*(n11+n01)*(n10+n00))
    }
  }
  return(cor)
}

#correlation matrix of each model
round(cor_dist(svm_result_mod[,1:10],test$Credit_Score),3)
round(cor_dist(tree_result_mod[,1:10],test$Credit_Score),3)
round(cor_dist(lr_result_mod[,1:10],test$Credit_Score),3)
round(cor_dist(qda_result_mod[,1:10],test$Credit_Score),3)


# final aggregation and performance
set.seed(108021186)
setting=sample(1:80.20)
mult_result=cbind(svm_result_mod,tree_result_mod,lr_result_mod,qda_result_mod)[,setting]
agg_mult=result(mult_result)
table(agg_mult,test$Credit_Score)
mean(agg_mult==test$Credit_Score)
round(cor_dist(mult_result[,1:10],test$Credit_Score),3)