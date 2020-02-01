
data=read.csv("C:/Users/yingy/Desktop/11 data mining/hw4/data.csv")
colnames(data)[1] <- "Class"
attach(data)

#for continuous features kept in RDS:compute and display their mean and standard deviation within each class
c2=data[which(data$Class=="2"),]
c3=data[which(data$Class=="3"),]
c5=data[which(data$Class=="5"),]
#compute mean and sd in c2
for (i in 3:26){
  print(mean(c2[,i]))
}
for (i in 3:26){
  print(sd(c2[,i]))
}

#compute mean and sd in c3
for (i in 3:26){
  print(mean(c3[,i]))
}
for (i in 3:26){
  print(sd(c3[,i]))
}

#compute mean and sd in c5
for (i in 3:26){
  print(mean(c5[,i]))
}
for (i in 3:26){
  print(sd(c5[,i]))
}
#Step3 : Center and Rescale the whole RDS so that each feature will then have global mean = 0 and global stand. dev. =1
library(scales)
cen_data <- scale(data[,3:26])
cen_data <- data.frame(cen_data)
new_data <- cbind(data[,1:2],cen_data)
new_data = data.frame(new_data)
c2=new_data[which(new_data$Class=="2"),]
c3=new_data[which(new_data$Class=="3"),]
c5=new_data[which(new_data$Class=="5"),]


##add more case in test
addtest=read.csv("C:/Users/yingy/Desktop/11 data mining/hw4/test.csv")
colnames(addtest)[1] <- "Class"
attach(addtest)
cen_addtest <- scale(addtest[,3:26])
cen_addtest <- data.frame(cen_addtest)
new_addtest <- cbind(addtest[,1:2],cen_addtest)
new_addtest = data.frame(new_addtest)
c2add=new_addtest[which(new_addtest$Class=="2"),]
c3add=new_addtest[which(new_addtest$Class=="3"),]
c5add=new_addtest[which(new_addtest$Class=="5"),]



#Split each class into a training set and a test set , using the proportions 80% and 20% 
n <- 2000  # Number of observations
ntrain <- round(n*0.8)  # 80% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_c2 <- c2[tindex,]   # Create c2 training set
test_c2 <- c2[-tindex,]   # Create c2 test set
test_c2 <- rbind(test_c2,c2add)
train_c3 <- c3[tindex,]   # Create c3 training set
test_c3 <- c3[-tindex,]   # Create c3 test set
test_c3 <- rbind(test_c3,c3add)
train_c5 <- c5[tindex,]   # Create c5 training set
test_c5 <- c5[-tindex,]   # Create c5 test set
test_c5 <- rbind(test_c5,c5add)

TRAIN <- rbind(train_c2,train_c3,train_c5)
TEST <- rbind(test_c2,test_c3,test_c5)

#Question 2: SVM classification by radial kernel 
#Step1: optimize the parameters "cost" and "gamma" 
x1=rbind(train_c2,train_c3)

data1=data.frame(x=x1[3:26],y=as.factor(x1$Class))

set.seed (1)
library(e1071)
tune.out1=tune(svm ,y~ .,data=data1,kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100),gamma=c(0.1,1,10,100) ))
summary (tune.out1)


#Step 2: Re-evaluation of tuning : 
x2=rbind(train_c2,train_c5)
data2=data.frame(x=x2[3:26],y=as.factor(x2$Class))

set.seed (1)
tune.out2=tune(svm ,y~ .,data=data2,kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100),gamma=c(0.1,1,10,100) ))
summary (tune.out2)

#Question 3 : for the largest 3 classes CL1 CL2 CL3 , compute 3 SVMs 
#SVM1 to classify CL2 vs (not CL2) 
c35=rbind(train_c3,train_c5)
c35[,1] <- "-2"
TRAIN1=rbind(train_c2,c35)
library(e1071)
TRAIN1$y=as.factor(TRAIN1$Class)
svmfit1=svm(TRAIN1$y~ .,data=TRAIN1[3:26],kernel="radial",gamma =0.1,cost=10,scale=FALSE)  
summary(svmfit1)

c35_test=rbind(test_c3,test_c5)
c35_test[,1] <- "-2"
TEST1=rbind(test_c2,c35_test)
TEST1$y=as.factor(TEST1$Class)

#the percentages of correct predictions PredTrain and PredTest and the two confusion matrices
train_pred1 = predict(svmfit1, TRAIN1[3:26]) 
test_pred1 = predict(svmfit1, TEST1[3:26]) 

#confusion matrices for TRAIN
TRAIN_confus.matrix1 = table(real=TRAIN1$y, predict=train_pred1)
TRAIN_confus.matrix1
#confusion matrices for TEST
TEST_confus.matrix1 = table(real=TEST1$y, predict=test_pred1)
TEST_confus.matrix1
#compute the errors of estimation on PredTRAIN, PredTEST, and on the terms of the confusion matrices 
sum(diag(TRAIN_confus.matrix1))/sum(TRAIN_confus.matrix1)    
sum(diag(TEST_confus.matrix1))/sum(TEST_confus.matrix1)

#SVM2 to classify CL3 vs (not CL3) 
c25=rbind(train_c2,train_c5)
c25[,1] <- "-3"
TRAIN2=rbind(train_c3,c25)
TRAIN2$y=as.factor(TRAIN2$Class)
svmfit2=svm(TRAIN2$y~ .,data=TRAIN2[3:26],kernel="radial",gamma =0.1,cost=10,scale=FALSE)  
summary(svmfit2)

c25_test=rbind(test_c2,test_c5)
c25_test[,1] <- "-3"
TEST2=rbind(test_c3,c25_test)
TEST2$y=as.factor(TEST2$Class)

#the percentages of correct predictions PredTrain and PredTest and the two confusion matrices
train_pred2 = predict(svmfit2, TRAIN2[3:26]) 
test_pred2 = predict(svmfit2, TEST2[3:26]) 

#confusion matrices for TRAIN
TRAIN_confus.matrix2 = table(real=TRAIN2$y, predict=train_pred2)
TRAIN_confus.matrix2
#confusion matrices for TEST
TEST_confus.matrix2 = table(real=TEST2$y, predict=test_pred2)
TEST_confus.matrix2

#SVM3 to classify CL5 vs (not CL5) 
c23=rbind(train_c2,train_c3)
c23[,1] <- "-5"
TRAIN3=rbind(train_c5,c23)
TRAIN3$y=as.factor(TRAIN3$Class)
svmfit3=svm(TRAIN3$y~ .,data=TRAIN3[3:26],kernel="radial",gamma =0.1,cost=10,scale=FALSE)  
summary(svmfit3)

c23_test=rbind(test_c2,test_c3)
c23_test[,1] <- "-5"
TEST3=rbind(test_c5,c23_test)
TEST3$y=as.factor(TEST3$Class)

#the percentages of correct predictions PredTrain and PredTest and the two confusion matrices
train_pred3 = predict(svmfit3, TRAIN3[3:26]) 
test_pred3 = predict(svmfit3, TEST3[3:26]) 

#confusion matrices for TRAIN
TRAIN_confus.matrix3 = table(real=TRAIN3$y, predict=train_pred3)
TRAIN_confus.matrix3
#confusion matrices for TEST
TEST_confus.matrix3 = table(real=TEST3$y, predict=test_pred3)
TEST_confus.matrix3


#Q4:for the largest 3 classes CL1 CL2 CL3 , combine the three SVMs to classify all cases
train_pred1= data.frame(train_pred1)
train_pred2= data.frame(train_pred2)
train_pred3= data.frame(train_pred3)
train_pred1=train_pred1[order(as.numeric(rownames(train_pred1))),,drop=FALSE]
train_pred2=train_pred2[order(as.numeric(rownames(train_pred2))),,drop=FALSE]
train_pred3=train_pred3[order(as.numeric(rownames(train_pred3))),,drop=FALSE]
SVM_train_pred <-  cbind(train_pred1,train_pred2,train_pred3)
#head(SVM_train_pred)
#tail(SVM_train_pred)
SVM_train_pred$SVM1_rel2 <- ifelse(SVM_train_pred$train_pred1 =='2', 1,0)
SVM_train_pred$SVM1_rel3 <- ifelse(SVM_train_pred$train_pred1 =='2',0, 1/2)
SVM_train_pred$SVM1_rel5 <- ifelse(SVM_train_pred$train_pred1 =='2',0, 1/2)

SVM_train_pred$SVM2_rel2 <- ifelse(SVM_train_pred$train_pred2 =='3', 0, 1/2)
SVM_train_pred$SVM2_rel3 <- ifelse(SVM_train_pred$train_pred2 =='3', 1, 0)
SVM_train_pred$SVM2_rel5 <- ifelse(SVM_train_pred$train_pred2 =='3', 0, 1/2)

SVM_train_pred$SVM3_rel2 <- ifelse(SVM_train_pred$train_pred3 =='5', 0, 1/2)
SVM_train_pred$SVM3_rel3 <- ifelse(SVM_train_pred$train_pred3 =='5', 0, 1/2)
SVM_train_pred$SVM3_rel5 <- ifelse(SVM_train_pred$train_pred3 =='5', 1, 0)


SVM_train_pred$score2 <- SVM_train_pred$SVM1_rel2+SVM_train_pred$SVM2_rel2+SVM_train_pred$SVM3_rel2
SVM_train_pred$score3 <- SVM_train_pred$SVM1_rel3+SVM_train_pred$SVM2_rel3+SVM_train_pred$SVM3_rel3
SVM_train_pred$score5 <- SVM_train_pred$SVM1_rel5+SVM_train_pred$SVM2_rel5+SVM_train_pred$SVM3_rel5

SVM_train_pred$sum <- SVM_train_pred$score2+SVM_train_pred$score3+SVM_train_pred$score5

SVM_train_pred$pred.CL <- ifelse(SVM_train_pred$score2 > SVM_train_pred$score3 & SVM_train_pred$score2 > SVM_train_pred$score5,"CL2",
                                 ifelse(SVM_train_pred$score3 > SVM_train_pred$score2 & SVM_train_pred$score3 > SVM_train_pred$score5,"CL3" ,
                                        ifelse(SVM_train_pred$score5 > SVM_train_pred$score2 & SVM_train_pred$score5 > SVM_train_pred$score3,"CL5", NA)))

SVM_train_pred$reliability <- ifelse(SVM_train_pred$pred.CL == "CL2", SVM_train_pred$score2/SVM_train_pred$sum,
                                     ifelse(SVM_train_pred$pred.CL == "CL3", SVM_train_pred$score3/SVM_train_pred$sum,
                                            ifelse(SVM_train_pred$pred.CL == "CL5", SVM_train_pred$score5/SVM_train_pred$sum,NA)))

TRAIN=TRAIN[order(as.numeric(rownames(TRAIN))),,drop=FALSE]
train_labels <- TRAIN[,1]
SVM_train_pred$true.CL <- train_labels
  
tail(SVM_train_pred)
table(predict=SVM_train_pred$pred.CL, real=SVM_train_pred$true.CL)



##test
test_pred1 <- data.frame(test_pred1)
test_pred2 <- data.frame(test_pred2)
test_pred3 <- data.frame(test_pred3)
test_pred1=test_pred1[order(as.numeric(rownames(test_pred1))),,drop=FALSE]
test_pred2=test_pred2[order(as.numeric(rownames(test_pred2))),,drop=FALSE]
test_pred3=test_pred3[order(as.numeric(rownames(test_pred3))),,drop=FALSE]

SVM_test_pred <-  cbind(test_pred1,test_pred2,test_pred3)

SVM_test_pred$SVM1_rel2 <- ifelse(SVM_test_pred$test_pred1 =='2', 1*0.9617,0)
SVM_test_pred$SVM1_rel3 <- ifelse(SVM_test_pred$test_pred1 =='2',0, 0.5*0.9822)
SVM_test_pred$SVM1_rel5 <- ifelse(SVM_test_pred$test_pred1 =='2',0, 0.5*0.9822)

SVM_test_pred$SVM2_rel2 <- ifelse(SVM_test_pred$test_pred2 =='3', 0, 0.5*0.9754)
SVM_test_pred$SVM2_rel3 <- ifelse(SVM_test_pred$test_pred2 =='3', 1*0.9946, 0)
SVM_test_pred$SVM2_rel5 <- ifelse(SVM_test_pred$test_pred2 =='3', 0, 0.5*0.9754)

SVM_test_pred$SVM3_rel2 <- ifelse(SVM_test_pred$test_pred3 =='5', 0, 0.5*0.953)
SVM_test_pred$SVM3_rel3 <- ifelse(SVM_test_pred$test_pred3 =='5', 0, 0.5*0.953)
SVM_test_pred$SVM3_rel5 <- ifelse(SVM_test_pred$test_pred3 =='5', 1*0.9973, 0)

SVM_test_pred$score2 <- SVM_test_pred$SVM1_rel2+SVM_test_pred$SVM2_rel2+SVM_test_pred$SVM3_rel2
SVM_test_pred$score3 <- SVM_test_pred$SVM1_rel3+SVM_test_pred$SVM2_rel3+SVM_test_pred$SVM3_rel3
SVM_test_pred$score5 <- SVM_test_pred$SVM1_rel5+SVM_test_pred$SVM2_rel5+SVM_test_pred$SVM3_rel5

SVM_test_pred$sum <- SVM_test_pred$score2+SVM_test_pred$score3+SVM_test_pred$score5

SVM_test_pred$pred.CL <- ifelse(SVM_test_pred$score2 > SVM_test_pred$score3 & SVM_test_pred$score2 > SVM_test_pred$score5,"CL2",
                                ifelse(SVM_test_pred$score3 > SVM_test_pred$score2 & SVM_test_pred$score3 > SVM_test_pred$score5,"CL3" ,
                                       ifelse(SVM_test_pred$score5 > SVM_test_pred$score2 & SVM_test_pred$score5 > SVM_test_pred$score3,"CL5", NA)))

SVM_test_pred$reliability <- ifelse(SVM_test_pred$pred.CL == "CL2", SVM_test_pred$score2/SVM_test_pred$sum,
                                    ifelse(SVM_test_pred$pred.CL == "CL3", SVM_test_pred$score3/SVM_test_pred$sum,
                                           ifelse(SVM_test_pred$pred.CL == "CL5", SVM_test_pred$score5/SVM_test_pred$sum,NA)))

TEST=TEST[order(as.numeric(rownames(TEST))),,drop=FALSE]
test_labels <- TEST[,1]
SVM_test_pred$true.CL <- test_labels

tail(SVM_test_pred)
table(predict=SVM_test_pred$pred.CL, real=SVM_test_pred$true.CL)

#Q5:using the polynomial kernel (K(x,y) = (1+<x,y>)2 
#optimize the parameters "cost"
set.seed (1)
tune.out1.1=tune(svm ,y~ .,data=data1,kernel ="polynomial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000)))
summary (tune.out1.1)


set.seed (1)
tune.out2.1=tune(svm ,y~ .,data=data2,kernel ="polynomial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000)))
summary (tune.out2.1)

#SVM1 to classify CL2 vs (not CL2) 
svmfit1.1=svm(TRAIN1$y~ .,data=TRAIN1[3:26],kernel="polynomial",coef0=1,degree=2,cost=10,scale=FALSE)  
summary(svmfit1.1)

#the percentages of correct predictions PredTrain and PredTest and the two confusion matrices
train_pred1.1 = predict(svmfit1.1, TRAIN1[3:26]) 
test_pred1.1 = predict(svmfit1.1, TEST1[3:26]) 

#confusion matrices for TRAIN
TRAIN_confus.matrix1.1 = table(real=TRAIN1$y, predict=train_pred1.1)
TRAIN_confus.matrix1.1
#confusion matrices for TEST
TEST_confus.matrix1.1 = table(real=TEST1$y, predict=test_pred1.1)
TEST_confus.matrix1.1

#SVM2 to classify CL3 vs (not CL3)
svmfit2.1=svm(TRAIN2$y~ .,data=TRAIN2[3:26],kernel="polynomial",coef0=1,degree=2,cost=10,scale=FALSE)  
summary(svmfit2.1)

#the percentages of correct predictions PredTrain and PredTest and the two confusion matrices
train_pred2.1 = predict(svmfit2.1, TRAIN2[3:26]) 
test_pred2.1 = predict(svmfit2.1, TEST2[3:26]) 

#confusion matrices for TRAIN
TRAIN_confus.matrix2.1 = table(real=TRAIN2$y, predict=train_pred2.1)
TRAIN_confus.matrix2.1
#confusion matrices for TEST
TEST_confus.matrix2.1 = table(real=TEST2$y, predict=test_pred2.1)
TEST_confus.matrix2.1

#SVM3 to classify CL5 vs (not CL5) 
svmfit3.1=svm(TRAIN3$y~ .,data=TRAIN3[3:26],kernel="polynomial",coef0=1,degree=2,cost=10,scale=FALSE)  
summary(svmfit3.1)

#the percentages of correct predictions PredTrain and PredTest and the two confusion matrices
train_pred3.1 = predict(svmfit3.1, TRAIN3[3:26]) 
test_pred3.1 = predict(svmfit3.1, TEST3[3:26]) 

#confusion matrices for TRAIN
TRAIN_confus.matrix3.1 = table(real=TRAIN3$y, predict=train_pred3.1)
TRAIN_confus.matrix3.1
#confusion matrices for TEST
TEST_confus.matrix3.1 = table(real=TEST3$y, predict=test_pred3.1)
TEST_confus.matrix3.1




#train
train_pred1.1= data.frame(train_pred1.1)
train_pred2.1= data.frame(train_pred2.1)
train_pred3.1= data.frame(train_pred3.1)
train_pred1.1=train_pred1.1[order(as.numeric(rownames(train_pred1.1))),,drop=FALSE]
train_pred2.1=train_pred2.1[order(as.numeric(rownames(train_pred2.1))),,drop=FALSE]
train_pred3.1=train_pred3.1[order(as.numeric(rownames(train_pred3.1))),,drop=FALSE]
SVM_train_pred.1 <-  cbind(train_pred1.1,train_pred2.1,train_pred3.1)
#head(SVM_train_pred.1)
#tail(SVM_train_pred.1)
SVM_train_pred.1$SVM1_rel2 <- ifelse(SVM_train_pred.1$train_pred1.1 =='2', 1*0.9997,0)
SVM_train_pred.1$SVM1_rel3 <- ifelse(SVM_train_pred.1$train_pred1.1 =='2',0, 0.5*0.9988)
SVM_train_pred.1$SVM1_rel5 <- ifelse(SVM_train_pred.1$train_pred1.1 =='2',0, 0.5*0.9988)

SVM_train_pred.1$SVM2_rel2 <- ifelse(SVM_train_pred.1$train_pred2.1 =='3', 0, 0.5*0.9975)
SVM_train_pred.1$SVM2_rel3 <- ifelse(SVM_train_pred.1$train_pred2.1 =='3', 1*0.9997, 0)
SVM_train_pred.1$SVM2_rel5 <- ifelse(SVM_train_pred.1$train_pred2.1 =='3', 0, 0.5*0.9975)

SVM_train_pred.1$SVM3_rel2 <- ifelse(SVM_train_pred.1$train_pred3.1 =='5', 0, 1/2)
SVM_train_pred.1$SVM3_rel3 <- ifelse(SVM_train_pred.1$train_pred3.1 =='5', 0, 1/2)
SVM_train_pred.1$SVM3_rel5 <- ifelse(SVM_train_pred.1$train_pred3.1 =='5', 1, 0)


SVM_train_pred.1$score2 <- SVM_train_pred.1$SVM1_rel2+SVM_train_pred.1$SVM2_rel2+SVM_train_pred.1$SVM3_rel2
SVM_train_pred.1$score3 <- SVM_train_pred.1$SVM1_rel3+SVM_train_pred.1$SVM2_rel3+SVM_train_pred.1$SVM3_rel3
SVM_train_pred.1$score5 <- SVM_train_pred.1$SVM1_rel5+SVM_train_pred.1$SVM2_rel5+SVM_train_pred.1$SVM3_rel5

SVM_train_pred.1$sum <- SVM_train_pred.1$score2+SVM_train_pred.1$score3+SVM_train_pred.1$score5

SVM_train_pred.1$pred.CL <- ifelse(SVM_train_pred.1$score2 > SVM_train_pred.1$score3 & SVM_train_pred.1$score2 > SVM_train_pred.1$score5,"CL2",
                                   ifelse(SVM_train_pred.1$score3 > SVM_train_pred.1$score2 & SVM_train_pred.1$score3 > SVM_train_pred.1$score5,"CL3" ,
                                          ifelse(SVM_train_pred.1$score5 > SVM_train_pred.1$score2 & SVM_train_pred.1$score5 > SVM_train_pred.1$score3,"CL5", NA)))

SVM_train_pred.1$reliability <- ifelse(SVM_train_pred.1$pred.CL == "CL2", SVM_train_pred.1$score2/SVM_train_pred.1$sum,
                                       ifelse(SVM_train_pred.1$pred.CL == "CL3", SVM_train_pred.1$score3/SVM_train_pred.1$sum,
                                              ifelse(SVM_train_pred.1$pred.CL == "CL5", SVM_train_pred.1$score5/SVM_train_pred.1$sum,NA)))

SVM_train_pred.1$true.CL <- train_labels

tail(SVM_train_pred.1)
table(predict=SVM_train_pred.1$pred.CL, real=SVM_train_pred.1$true.CL)



#test
test_pred1.1= data.frame(test_pred1.1)
test_pred2.1= data.frame(test_pred2.1)
test_pred3.1= data.frame(test_pred3.1)
test_pred1.1=test_pred1.1[order(as.numeric(rownames(test_pred1.1))),,drop=FALSE]
test_pred2.1=test_pred2.1[order(as.numeric(rownames(test_pred2.1))),,drop=FALSE]
test_pred3.1=test_pred3.1[order(as.numeric(rownames(test_pred3.1))),,drop=FALSE]
SVM_test_pred.1 <-  cbind(test_pred1.1,test_pred2.1,test_pred3.1)
#head(SVM_test_pred.1)
#tail(SVM_test_pred.1)
SVM_test_pred.1$SVM1_rel2 <- ifelse(SVM_test_pred.1$test_pred1.1 =='2', 1*0.9492,0)
SVM_test_pred.1$SVM1_rel3 <- ifelse(SVM_test_pred.1$test_pred1.1 =='2',0, 0.5*0.9093)
SVM_test_pred.1$SVM1_rel5 <- ifelse(SVM_test_pred.1$test_pred1.1 =='2',0, 0.5*0.9093)

SVM_test_pred.1$SVM2_rel2 <- ifelse(SVM_test_pred.1$test_pred2.1 =='3', 0, 0.5*0.9859)
SVM_test_pred.1$SVM2_rel3 <- ifelse(SVM_test_pred.1$test_pred2.1 =='3', 1*0.9955, 0)
SVM_test_pred.1$SVM2_rel5 <- ifelse(SVM_test_pred.1$test_pred2.1 =='3', 0, 0.5*0.9859)

SVM_test_pred.1$SVM3_rel2 <- ifelse(SVM_test_pred.1$test_pred3.1 =='5', 0, 0.5*0.9964)
SVM_test_pred.1$SVM3_rel3 <- ifelse(SVM_test_pred.1$test_pred3.1 =='5', 0, 0.5*0.9964)
SVM_test_pred.1$SVM3_rel5 <- ifelse(SVM_test_pred.1$test_pred3.1 =='5', 1*0.9973, 0)


SVM_test_pred.1$score2 <- SVM_test_pred.1$SVM1_rel2+SVM_test_pred.1$SVM2_rel2+SVM_test_pred.1$SVM3_rel2
SVM_test_pred.1$score3 <- SVM_test_pred.1$SVM1_rel3+SVM_test_pred.1$SVM2_rel3+SVM_test_pred.1$SVM3_rel3
SVM_test_pred.1$score5 <- SVM_test_pred.1$SVM1_rel5+SVM_test_pred.1$SVM2_rel5+SVM_test_pred.1$SVM3_rel5

SVM_test_pred.1$sum <- SVM_test_pred.1$score2+SVM_test_pred.1$score3+SVM_test_pred.1$score5

SVM_test_pred.1$pred.CL <- ifelse(SVM_test_pred.1$score2 > SVM_test_pred.1$score3 & SVM_test_pred.1$score2 > SVM_test_pred.1$score5,"CL2",
                                  ifelse(SVM_test_pred.1$score3 > SVM_test_pred.1$score2 & SVM_test_pred.1$score3 > SVM_test_pred.1$score5,"CL3" ,
                                         ifelse(SVM_test_pred.1$score5 > SVM_test_pred.1$score2 & SVM_test_pred.1$score5 > SVM_test_pred.1$score3,"CL5", NA)))

SVM_test_pred.1$reliability <- ifelse(SVM_test_pred.1$pred.CL == "CL2", SVM_test_pred.1$score2/SVM_test_pred.1$sum,
                                      ifelse(SVM_test_pred.1$pred.CL == "CL3", SVM_test_pred.1$score3/SVM_test_pred.1$sum,
                                             ifelse(SVM_test_pred.1$pred.CL == "CL5", SVM_test_pred.1$score5/SVM_test_pred.1$sum,NA)))

SVM_test_pred.1$true.CL <- test_labels

tail(SVM_test_pred.1)
table(predict=SVM_test_pred.1$pred.CL, real=SVM_test_pred.1$true.CL)
