#Q1
#step1
A = matrix(runif(16,-2,2), 4, 4)
A
B = runif(4,-2,2)
B
C = runif(1,-2,2)
C
#Pol(x) = £Ui £Uj Aij xi xj + £U i Bi xi + c/20
pol <- function(x){
  sum_A = 0
  sum_B = 0
  for (i in 1:4){
    for (j in 1:4){
      sum_A = sum_A + A[i,j] * x[i] * x[j]
    }
  }
  for (i in 1:4){
    sum_B = sum_B + B[i] * x[i]
  }
  result = sum_A + sum_B + C/20
  return(result)
}

#step2
#10, 000 vectors, each vector has 4 randomly chosen coordinates with values in [-2, 2]
row=10000
col=4
x = matrix(runif(row*col,-2,2), row, col)
#U(n) = Pol(xn)
U <- function(n){
  U_result = pol(x[n,])
  return(U_result)
}

#y(n) = sign[U(n)]
y <- function(n){
  y_result= sign(U(n))
  return(y_result)
}

#keep only 2500 cases in CL(1) and 2500 cases in CL(-1)
select=2500
select_up=0
select_down=0
data = matrix(NA,select*2, col+2)
for(i in 1:row){
  select_total = select_down + select_up + 1
  if(y(i)==1 && select_up<select) {
    data[select_total,1:col]=x[i,]
    data[select_total,col+1]=U(i)
    data[select_total,col+2]=y(i)
    select_up = select_up+1;
  }else if(y(i)==-1 && select_down<select) {
    data[select_total,1:col]=x[i,]
    data[select_total,col+1]=U(i)
    data[select_total,col+2]=y(i)
    select_down=select_down+1
  }
  if(select_up==select && select_down==select) break
}


dimnames(data) <- list(c(1:(select*2)),c("X1","X2","X3","X4","U","y"))
data = na.omit(data)     
data=data.frame(data)
head(data)
#Center and Rescale this data set of size 5000 so that the standardized data set will have mean = 0 and dispersion =1
library(scales)
cen_data <- scale(data[,1:4])
cen_data <- data.frame(cen_data)
#cen_data <- rescale(data[,1:4], mean = 0, sd = 1)
y = as.factor(data[,6])
new_data <- cbind(cen_data,y)
new_data = data.frame(new_data)
head(new_data)
# Split training and test set using an 80:20 ratio
#defines a training set TRAIN and a test set TEST of resp. sizes 4000 and 1000
CL_PO = subset(new_data,y==1)
CL_NE = subset(new_data,y==-1)
train = sample(2500,2000)

newtrain=merge(CL_PO[train,],CL_NE[train,],all=T)
TRAIN_data= newtrain[c(1:4)]
TRAIN_labels=newtrain[5]

newtest = merge(CL_PO[-train,],CL_NE[-train,],all=T)
TEST_data = newtest[c(1:4)]
TEST_labels=newtest[5]

TRAIN = cbind(TRAIN_data,TRAIN_labels)
Summary(TRAIN)
TEST = cbind(TEST_data,TEST_labels)

#Question 2: SVM classification by linear kernel
#Run the svm() function on the set TRAIN,kernel = "linear ", cost = 5
library(e1071)
TRAIN_labels$y=as.factor(TRAIN_labels$y)
svmfit=svm(TRAIN_labels$y~ .,data=TRAIN,kernel="linear",cost=5,scale=FALSE)  

#compute the number S of support vectors and the ratio s = S/4000
summary(svmfit)  

#compute the percentages of correct prediction PredTrain and PredTest on the sets TRAIN and TEST
train_pred = predict(svmfit, TRAIN) 
test_pred = predict(svmfit, TEST) 

#confusion matrices must be converted in terms of frequencies of correct predictions within each class
#confusion matrices for TRAIN
TRAIN_confus.matrix = table(predict=train_pred,real=TRAIN$y)
TRAIN_confus.matrix
#confusion matrices for TEST
TEST_confus.matrix = table(real=TEST$y, predict=test_pred)
TEST_confus.matrix

#compute the errors of estimation on PredTRAIN, PredTEST, and on the terms of the confusion matrices 
sum(diag(TRAIN_confus.matrix))/sum(TRAIN_confus.matrix)     #0.65975

sum(diag(TEST_confus.matrix))/sum(TEST_confus.matrix)      # 0.646
#interpret your results
#there has xxxx support vectors, and when the cost is 5, the confusion matrix fir train is xx, and for test is xx, 


#Q3 : optimize the parameter "cost"
#Select a list of 6 values for the "cost " parameter
#Run the tuning function tune() for the linear svm() to identify the best value of "cost"

set.seed (1)
tune.out=tune(svm,y~.,data=TRAIN,kernel ="linear",ranges =list(cost=c(0.001,0.01,0.1,1,5,10)))
summary (tune.out)  #best value of "cost"= 0.01

#Evaluate the performance characteristics of the "best" linear svm as in question 2
svmfit_1=svm(TRAIN_labels$y~ .,data=TRAIN,kernel="linear",cost=0.01,scale=FALSE)
summary(svmfit_1)
train_pred_1 = predict(svmfit_1, TRAIN) 
test_pred_1 = predict(svmfit_1, TEST) 

#confusion matrices must be converted in terms of frequencies of correct predictions within each class
#confusion matrices for TRAIN
TRAIN_confus.matrix_1 = table(predict=train_pred_1,real=TRAIN$y)
TRAIN_confus.matrix_1
#confusion matrices for TEST
TEST_confus.matrix_1 = table(real=TEST$y, predict=test_pred_1)
TEST_confus.matrix_1

#compute the errors of estimation on PredTRAIN, PredTEST, and on the terms of the confusion matrices 
sum(diag(TRAIN_confus.matrix_1))/sum(TRAIN_confus.matrix_1)  

sum(diag(TEST_confus.matrix_1))/sum(TEST_confus.matrix_1)  






#Q4: SVM classification by radial kernel
#Fix the "cost" parameter in the svm() function to the best cost value identified in question 3
#Select the kernel parameter kernel = "radial " which means that the kernel ks given by the formula
#Select arbitrarily the gamma parameter "gamma" = 1
#Run the svm() function on the set TRAIN
svmfit1=svm(TRAIN_labels$y~ .,data=TRAIN,kernel="radial",gamma =1,cost=0.01,scale=FALSE)
summary(svmfit1)
#as in question 2 compute the number S and the ratio s = S/4000
S1=3994
s1=S1/4000
s1         #0.9985
#the percentages of correct predictions PredTrain and PredTest and the two confusion matrices
train_pred1 = predict(svmfit1, TRAIN) 
test_pred1 = predict(svmfit1, TEST) 

#confusion matrices for TRAIN
TRAIN_confus.matrix1 = table(real=TRAIN$y, predict=train_pred1)
TRAIN_confus.matrix1
#confusion matrices for TEST
TEST_confus.matrix1 = table(real=TEST$y, predict=test_pred1)
TEST_confus.matrix1

sum(diag(TRAIN_confus.matrix1))/sum(TRAIN_confus.matrix1) # 0.94575
sum(diag(TEST_confus.matrix1))/sum(TEST_confus.matrix1) #0.931
#interpret your results



#Q5 : optimize the parameter "cost"and "gamma"
#Select a list of 5 values for the "cost " parameter and a list of 5 values for the parameter "gamma"
#On the TRAIN set , run the tuning function tune() for the radial svm() to identify the best value of the pair ("cost", "gamma") among the 25 values you have listed
set.seed (1)
tune.out1=tune(svm ,y~ .,data=TRAIN,kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.01,0.1,1,10,100) ))
summary (tune.out1)


#Evaluate the performance characteristics of the "best" radial svm as in question 2
#Interpret your results
svmfit1_1=svm(TRAIN_labels$y~ .,data=TRAIN,kernel="radial",gamma =0.1,cost=1000,scale=FALSE)
summary(svmfit1_1)
train_pred1_1 = predict(svmfit1_1, TRAIN) 
test_pred1_1 = predict(svmfit1_1, TEST) 

#confusion matrices for TRAIN
TRAIN_confus.matrix1_1 = table(real=TRAIN$y, predict=train_pred1_1)
TRAIN_confus.matrix1_1
#confusion matrices for TEST
TEST_confus.matrix1_1 = table(real=TEST$y, predict=test_pred1_1)
TEST_confus.matrix1_1

sum(diag(TRAIN_confus.matrix1_1))/sum(TRAIN_confus.matrix1_1) 
sum(diag(TEST_confus.matrix1_1))/sum(TEST_confus.matrix1_1) 

#Q6 : SVM classification using a polynomial kernel
#Implement the steps of question 4 and 5 for the svm() function based on the polynomial kernel
#K(x,y) = (a + <x,y>)^4
#You will have to optimize the choice of the two parameters "a" >0 and ''cost"
svmfit2=svm(TRAIN_labels$y~ .,data=TRAIN,kernel="polynomial",coef0=1,degree=4,cost=0.01,scale=FALSE)
summary(svmfit2)

train_pred2 = predict(svmfit2, TRAIN) 
test_pred2 = predict(svmfit2, TEST) 

#confusion matrices for TRAIN
TRAIN_confus.matrix2 = table(real=TRAIN$y, predict=train_pred2)
TRAIN_confus.matrix2
#confusion matrices for TEST
TEST_confus.matrix2 = table(real=TEST$y, predict=test_pred2)
TEST_confus.matrix2

sum(diag(TRAIN_confus.matrix2))/sum(TRAIN_confus.matrix2) 
sum(diag(TEST_confus.matrix2))/sum(TEST_confus.matrix2) 

set.seed (1)
tune.out2=tune(svm ,y~ .,data=TRAIN,kernel ="polynomial",ranges =list(coef0=c(0.01,0.1,1,10,100),cost=c(0.1 ,1 ,10 ,100 ,1000)))
summary (tune.out2)




svmfit2_1=svm(TRAIN_labels$y~ .,data=TRAIN,kernel="polynomial",coef0=100,degree=4,cost=10,scale=FALSE)
summary(svmfit2_1)

train_pred2_1 = predict(svmfit2_1, TRAIN) 
test_pred2_1 = predict(svmfit2_1, TEST) 

#confusion matrices for TRAIN
TRAIN_confus.matrix2_1 = table(real=TRAIN$y, predict=train_pred2_1)
TRAIN_confus.matrix2_1
#confusion matrices for TEST
TEST_confus.matrix2_1 = table(real=TEST$y, predict=test_pred2_1)
TEST_confus.matrix2_1

sum(diag(TRAIN_confus.matrix2_1))/sum(TRAIN_confus.matrix2_1) 
sum(diag(TEST_confus.matrix2_1))/sum(TEST_confus.matrix2_1) 

