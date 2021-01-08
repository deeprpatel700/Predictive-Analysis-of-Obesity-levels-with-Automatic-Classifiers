'''
Name: Deep Patel 
Project: Factors affecting Obesity and Classification of Obesity levels
Project Section: KNN (Supervised learning)

Subsections:
1. Importing files and balancing classes
2. KNN for differnt k-values
3. Visualization Plot for Training vs. Test set to select best k-value
4. Using Confidence Intervals
5. Using weighted KNN to improve accuracy

'''

setwd("C:\\Users\\deepp\\Google Drive\\MSDS\\MATH 6350 Data Mining")
getwd()
SDATA<- read.csv('Obesity_SDATA.csv') #reading the files
SDATA_OB<- read.csv('OBESITY_Standardized_Pre-clone.csv')
SDATA[1]<- NULL
SDATA_OB[1]<- NULL
class_r<- case_when(SDATA_OB$Class=="Non-Obese"~"C1",
                    SDATA_OB$Class=="Overweight"~"C2",
                    SDATA_OB$Class=="Obese"~"C3")
SDATA_OB$class<- class_r
SDATA_OB$Class<- NULL

C1<- filter(SDATA_OB, class =='C1')
C2<- filter(SDATA_OB, class =='C2')
C3<- filter(SDATA_OB, class =='C3')


n1 = nrow(C1)
n2 = nrow(C2)
n3 = nrow(C3)

head(SDATA_OB[,1:5])
dim(SDATA_OB)
SDATA_OB$class<- as.factor(SDATA_OB$class)


SDATA_CL1 <- filter(SDATA_OB, class == 'C1')
train1 <- sample(1:n1, 0.8*n1)
trainCL1 <- SDATA_CL1[train1,]
testCL1 <- SDATA_CL1[-train1,]

SDATA_CL2 <- filter(SDATA_OB, class == 'C2')
train2 <- sample(1:n2, 0.8*n2)
trainCL2 <- SDATA_CL2[train2,]
testCL2 <- SDATA_CL2[-train2,]

SDATA_CL3 <- filter(SDATA_OB, class == 'C3')
train3 <- sample(1:n3, 0.8*n3)
trainCL3 <- SDATA_CL3[train3,]
testCL3 <- SDATA_CL3[-train3,]

TRAINSET <- rbind(trainCL1,trainCL1,trainCL2,trainCL2,trainCL3)
x.TRAINSET <- TRAINSET[-30]
y.TRAINSET <- as.factor(TRAINSET[,30])
TESTSET <- rbind(testCL1,testCL1,testCL2,testCL2,testCL3)
x.TESTSET <- TESTSET[-30]
y.TESTSET <- as.factor(TESTSET[,30])

table(y.TRAINSET)
table(y.TESTSET)


library(class)
k_values <- 1:30
testperfk <- numeric(length(k_values))
trainperfk <- numeric(length(k_values))
error_rate_train<- numeric(length(k_values))
error_rate_test<- numeric(length(k_values))

set.seed(1)
for (i in 1:length(k_values)) {
  knn.pred <- knn(x.TRAINSET, x.TESTSET, y.TRAINSET, k=k_values[i])
  testperfk[i] <-  (mean(y.TESTSET == knn.pred))*100
  error_rate_test[i]<- 1-(testperfk[i]/100)
  
  knn.train.pred <- knn(x.TRAINSET, x.TRAINSET, y.TRAINSET, k=k_values[i])
  trainperfk[i] <-  (mean(y.TRAINSET == knn.train.pred))*100
  error_rate_train[i]<- 1-(trainperfk[i]/100)
  
  cat('For k=',k_values[i], ', Test Accuracy is',round(testperfk[i],2), '%   
        & Train Accuracy is', round(trainperfk[i],2), "%", '\n')
}


# To see the plot of test accuracy vs. K-value compared with train accuracy 
plot(k_values, trainperfk, ylim=range(c(trainperfk,testperfk)),type='l',col='blue',
     xlab = "K values", ylab = "Accuracy (%)", main = 'Testing(Red) & Training(Blue) Accuracy')
par(new = TRUE)
plot(k_values, testperfk, ylim=range(c(trainperfk,testperfk)),type='l',
     axes = FALSE, xlab = "", ylab = "", col='red')


#To see the plot of test error rate vs. K-value compared with train error rate 
plot(k_values,error_rate_test,type = 'l',xlab = 'K values', ylab = 'Error rate', 
     ylim=range(c(error_rate_test,error_rate_train)), col='red',
     main = 'Testing Error Rate (Red) & Training Error Rate (Blue) Accuracy vs. K-values')
par(new = TRUE)
plot(k_values,error_rate_train, type="l", 
     ylim=range(c(error_rate_test,error_rate_train)),axes=FALSE,
     xlab="",ylab="", col="blue")



max(testperfk) # Highest accuracy.
which.max(testperfk) # The index of best K.
kbest <- k_values[which.max(testperfk)]; kbest # The best K value.
#kbest<- 2
# Training set confusion matrix
set.seed(1)
knn.train.pred_kbest <- knn(train = x.TRAINSET,test = x.TRAINSET,cl = y.TRAINSET, k=kbest)
trainperf_kbest = mean(y.TRAINSET == knn.train.pred_kbest)

trainconf = table('Prediction (Train)'=knn.train.pred_kbest, 'True (Train)'=y.TRAINSET)
trainconf   #Train confusion matrix in number of classes

confmat.train<- as.matrix(trainconf, rownames=TRUE, colnames=TRUE)
trainconf_percent<- scale(confmat.train, center=FALSE, scale=rowSums(confmat.train))*100
round(trainconf_percent,2)     #Training confusion matrix in percentage


N1<-sum(trainconf); N1
diag_train_sum<-sum(diag(trainconf)); diag_train_sum  #N of cases correctly classified
sum(trainconf)-sum(diag(trainconf))  #N of cases incorrectly classified
p1 <- (diag_train_sum/N1); 
trainconf_acc<- p1*100    #global accuracy of train confusion matrix
trainconf_acc

# Testing set confusion matrix
set.seed(1)
knn.test.pred_kbest <- knn(x.TRAINSET, x.TESTSET, y.TRAINSET, k=kbest)
testperf_kbest = mean(y.TESTSET == knn.test.pred_kbest) 

testconf = table('Prediction (Test)'=knn.test.pred_kbest, 'True (Test)'=y.TESTSET)
testconf    #Test confusion matrix in number of classes

confmat.test<- as.matrix(testconf, rownames=TRUE, colnames=TRUE)
testconf_percent<- scale(confmat.test, center=FALSE, scale=rowSums(confmat.test))*100
round(testconf_percent,2)       #Testing confusion matrix in percentage

N2<- as.numeric(sum(testconf)); N2
diag_test_sum<-sum(diag(testconf))    #N of cases correctly classified
sum(testconf)-sum(diag(testconf))  #N of cases incorrectly classified
p2 <- (diag_test_sum/N2)   
testconf_acc<- p2*100 #global accuracy of test confusion matrix
testconf_acc

# To check confidence intervals for best k-value
#95% Confidence interval of each diagonal term individually
#(see below for 90% confidence interval for sum of 3 diagonal terms)
trainconf
sum(trainconf)   #sum of the trainconf matrix
round(trainconf_percent,2) #accuracies of trainconf matrix
rowSums(confmat.train)   #row sums of trainconf matrix
Ntrain.CL1<- sum(confmat.train[1,])    #sum of train CL1 values
Ntrain.CL2<- sum(confmat.train[2,])   #sum of train CL2 values
Ntrain.CL3<- sum(confmat.train[3,])   #sum of train CL3 values
ptrain.CL1<- confmat.train[1,1]/Ntrain.CL1  #observed frequency of correct train CL1 classes
ptrain.CL2<- confmat.train[2,2]/Ntrain.CL2  #observed frequency of correct train CL2 classes
ptrain.CL3<- confmat.train[3,3]/Ntrain.CL3  #observed frequency of correct train CL3 classes

testconf
sum(testconf)     #sum of testconf matrix
round(testconf_percent,2) #accurancies % of testconf matrix
rowSums(confmat.test)   #row sums of testconf matrix

Ntest.CL1<- sum(confmat.test[1,])   #sum of test CL1 values
Ntest.CL2<- sum(confmat.test[2,])    #sum of test CL2 values
Ntest.CL3<- sum(confmat.test[3,])    #sum of test CL3 values
ptest.CL1<- confmat.test[1,1]/Ntest.CL1
ptest.CL2<- confmat.test[2,2]/Ntest.CL2
ptest.CL3<- confmat.test[3,3]/Ntest.CL3

#95% Confidence interval of each diagonal term individually
#TRAINSET
#CI for CL1 diagonal term
sigma_train.CL1<- sqrt(ptrain.CL1*(1-ptrain.CL1)/Ntrain.CL1) 
lower_train.CL1<- ptrain.CL1-(1.96*sigma_train.CL1); lower_train.CL1
upper_train.CL1<- ptrain.CL1 + (1.96*sigma_train.CL1); upper_train.CL1

#CI for CL2 diagonal term
sigma_train.CL2<- sqrt(ptrain.CL2*(1-ptrain.CL2)/Ntrain.CL2) 
lower_train.CL2<- ptrain.CL2-(1.96*sigma_train.CL2); lower_train.CL2
upper_train.CL2<- ptrain.CL2 + (1.96*sigma_train.CL2); upper_train.CL2

#CI for CL3 diagonal term
sigma_train.CL3<- sqrt(ptrain.CL3*(1-ptrain.CL3)/Ntrain.CL3) 
lower_train.CL3<- ptrain.CL3-(1.96*sigma_train.CL3); lower_train.CL3
upper_train.CL3<- ptrain.CL3 + (1.96*sigma_train.CL3); upper_train.CL3

#TESTSET
#CI for CL1 diagonal term
sigma_test.CL1<- sqrt(ptest.CL1*(1-ptest.CL1)/Ntest.CL1) 
lower_test.CL1<- ptest.CL1-(1.96*sigma_test.CL1); lower_test.CL1
upper_test.CL1<- ptest.CL1 + (1.96*sigma_test.CL1); upper_test.CL1

#CI for CL2 diagonal term
sigma_test.CL2<- sqrt(ptest.CL2*(1-ptest.CL2)/Ntest.CL2) 
lower_test.CL2<- ptest.CL2-(1.96*sigma_test.CL2); lower_test.CL2
upper_test.CL2<- ptest.CL2 + (1.96*sigma_test.CL2); upper_test.CL2

#CI for CL3 diagonal term
sigma_test.CL3<- sqrt(ptest.CL3*(1-ptest.CL3)/Ntest.CL3) 
lower_test.CL3<- ptest.CL3-(1.96*sigma_test.CL3); lower_test.CL3
upper_test.CL3<- ptest.CL3 + (1.96*sigma_test.CL3); upper_test.CL3

#---------

#Confidence interval for sum of 3 diagonal terms (TRAINSET(p1) & TESTSET(p2))

N1<-as.numeric(sum(trainconf)); N1
diag_train_sum<-sum(diag(trainconf)); diag_train_sum  #N of cases correctly classified
p1 <- (diag_train_sum/N1); 

N2<- as.numeric(sum(testconf)); N2
diag_test_sum<-sum(diag(testconf))    #N of cases correctly classified
p2 <- (diag_test_sum/N2)   
sigma_p1<- sqrt(p1*((1-p1)/N1))   
sigma_p2<- sqrt(p2*(1-p2)/N2)

#Trainset 
lower_limit.p1<- p1-(1.96*sigma_p1); lower_limit.p1
upper_limit.p1<- p1+(1.96*sigma_p1); upper_limit.p1

#Testset
lower_limit.p2<- p2-(1.96*sigma_p2); lower_limit.p2
upper_limit.p2<- p2+(1.96*sigma_p2); upper_limit.p2

#If there is no overlap, disjoint confidence intervals exists and q1>q2
# If there is overlap? Try below

std<- sqrt((sigma_p1)^2 +(sigma_p2)^2); std
lower_limit.q1subq2<- (p1-p2)-(1.96*std); lower_limit.q1subq2
upper_limit.q1subq2<- (p1-p2)+(1.96*std); upper_limit.q1subq2

lower_limit.q2subq1<- (p2-p1)-(1.96*std); lower_limit.q2subq1
upper_limit.q2subq1<- (p2-p1)+(1.96*std); upper_limit.q2subq1

###################################################################################
########################## STEP 3 DONE #########################################
################################################################################

#-----------------------------------------------------------------------------
kbest <- k_values[which.max(testperfk)]; kbest # The best K value.
#kbest<- 2
colnames(TRAINSET)
colnames(TESTSET)
# Step 4

PACK1_test <- TESTSET[c(c(1:3,9,10),30)]
PACK1_train <- TRAINSET[c(c(1:3,9,10),30)]
set.seed(1)
knn.test.PACK1 <- knn(train = PACK1_train[-6],test = PACK1_test[-6],cl = PACK1_train[,6], k=kbest)
perf.PACK1 = mean(PACK1_test[,6] == knn.test.PACK1);perf.PACK1
w1<- perf.PACK1


PACK2_test <- TESTSET[c(c(6,11:14),30)]
PACK2_train <- TRAINSET[c(c(6,11:14),30)]
knn.test.PACK2 <- knn(train = PACK2_train[-6],test = PACK2_test[-6],cl = PACK2_train[,6], k=kbest)
perf.PACK2 = mean(PACK2_test[,6] == knn.test.PACK2); perf.PACK2
w2 <- perf.PACK2

PACK3_test <- TESTSET[c(c(7,25:29),30)]
PACK3_train <- TRAINSET[c(c(7,25:29),30)]
knn.test.PACK3 <- knn(train = PACK3_train[-7],test = PACK3_test[-7],cl = PACK3_train[,7], k=kbest)
perf.PACK3 = mean(PACK3_test[,7] == knn.test.PACK3); perf.PACK3
w3<- perf.PACK3

PACK4_test <- TESTSET[c(c(8,21:24),30)]
PACK4_train <- TRAINSET[c(c(8,21:24),30)]
knn.test.PACK4 <- knn(train = PACK4_train[-6],test = PACK4_test[-6],cl = PACK4_train[,6], k=kbest)
perf.PACK4 = mean(PACK4_test[,6] == knn.test.PACK4); perf.PACK4
w4<- perf.PACK4


PACK5_test <- TESTSET[c(c(4,5,15,16),30)]
PACK5_train <- TRAINSET[c(c(4,5,15,16),30)]
knn.test.PACK5 <- knn(train = PACK5_train[-5],test = PACK5_test[-5],cl = PACK5_train[,5], k=kbest)
perf.PACK5 = mean(PACK5_test[,5] == knn.test.PACK5); perf.PACK5
w5<- perf.PACK5

PACK6_test <- TESTSET[c(c(17:20),30)]
PACK6_train <- TRAINSET[c(c(17:20),30)]
knn.test.PACK6 <- knn(train = PACK6_train[-5],test = PACK6_test[-5],cl = PACK6_train[,5], k=kbest)
perf.PACK6 = mean(PACK6_test[,5] == knn.test.PACK6); perf.PACK6
w6<- perf.PACK6


library(LICORS) # normalize function gotten from here
wts<-c(w1,w2,w3,w4,w5,w6)
weights<- normalize(wts)

w1=weights[1];w2=weights[2];w3=weights[3];
w4=weights[4];w5=weights[5]; w6=weights[6]

sum(weights)
sum(wts)

# Multiplying by sqrt(w)
PACK1_test_w <- cbind(PACK1_test[0:5]*sqrt(w1),PACK1_test[6])
PACK1_train_w <- cbind(PACK1_train[0:5]*sqrt(w1),PACK1_train[6])

PACK2_test_w <- cbind(PACK2_test[0:5]*sqrt(w2),PACK2_test[6])
PACK2_train_w <- cbind(PACK2_train[0:5]*sqrt(w2),PACK2_train[6])

PACK3_test_w <- cbind(PACK3_test[0:6]*sqrt(w3),PACK3_test[7])
PACK3_train_w <- cbind(PACK3_train[0:6]*sqrt(w3),PACK3_train[7])

PACK4_test_w <- cbind(PACK4_test[0:5]*sqrt(w4),PACK4_test[6])
PACK4_train_w <- cbind(PACK4_train[0:5]*sqrt(w4),PACK4_train[6])

PACK5_test_w <- cbind(PACK5_test[0:4]*sqrt(w5),PACK4_test[5])
PACK5_train_w <- cbind(PACK5_train[0:4]*sqrt(w5),PACK4_train[5])

PACK6_test_w <- cbind(PACK6_test[0:4]*sqrt(w6),PACK6_test[5])
PACK6_train_w <- cbind(PACK6_train[0:4]*sqrt(w6),PACK6_train[5])

library(plyr)
TESTSET_w <- data.frame(matrix(ncol=30,nrow=0, dimnames=list(NULL, names(TESTSET))))
TESTSET_w <- rbind.fill(TESTSET_w,cbind(PACK1_test_w,PACK2_test_w,PACK3_test_w,
                                        PACK4_test_w,PACK5_test_w,PACK6_test_w))
TRAINSET_w <- data.frame(matrix(ncol=30,nrow=0, dimnames=list(NULL, names(TRAINSET))))
TRAINSET_w <- rbind.fill(TRAINSET_w,cbind(PACK1_train_w,PACK2_train_w,PACK3_train_w,
                                          PACK4_train_w,PACK5_train_w,PACK6_train_w))


# KNN on the TEST data with normalized weights
set.seed(1)
knn.test.w <- knn(train = TRAINSET_w[-30],test = TESTSET_w[-30],cl = TRAINSET_w[,30], k=kbest)
testperfw = mean(TESTSET_w[,30] == knn.test.w); testperfw

# Confusion matrix of weighted data on TESTSET
conftest.weight<- table('Prediction'=knn.test.w, 'True'=TESTSET_w[,30])
conftest.weight

confmat.testweight<- as.matrix(conftest.weight, rownames=TRUE, colnames=TRUE)
conftest_weightpercent<- scale(confmat.testweight, center=FALSE, 
                               scale=rowSums(confmat.testweight))*100
round(conftest_weightpercent,2)

global_acc.testw<- (sum(diag(conftest.weight))/sum(conftest.weight))*100 ;
global_acc.testw

testconf_acc
