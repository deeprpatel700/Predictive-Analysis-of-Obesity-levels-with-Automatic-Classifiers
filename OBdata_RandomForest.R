'''
Name: Deep Patel 
Project: Factors affecting Obesity and Classification of Obesity levels
Project Section: RandomForest (Supervised learning)

Subsections:
1. Importing files and balancing classes
2. Random Forest for differnt number of ntrees
3. Visualization Plot for Training vs. Test set
4. Visualization Plot for Each Class in testing set
5. Random Forest for best number of ntrees and Importance
6. Statisical Analysis of Most Important and Least Important feature

'''

setwd("C:\\Users\\deepp\\Google Drive")
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

library(randomForest)
colnames(TRAINSET)
RF_func<- function(ntree){
  train_RF<- randomForest(class~., data=TRAINSET, 
                          ntree=ntree, mtry=round(sqrt(29),0), importance=TRUE)
  test_RF<- predict(train_RF,x.TESTSET)
  test_conf<- table('TRUE'=y.TESTSET, 'PREDICTION'=test_RF)
  train_conf<-train_RF$confusion[,-4]
  result<- list('train_conf'=train_conf,'test_conf'=test_conf)
  return(result)
}


# function for confusion matrix in percentage
conf_percentage<- function(conf_name){
  confmat<- as.matrix(conf_name, rownames=TRUE, colnames=TRUE)
  conf_percent<- (prop.table(confmat,1))*100
  round(conf_percent,2)     
}

# function for Global accuracies
accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))*100
}

# Confusion matrices in train & test cases from random Forest function
RF1<- RF_func(1)
RF10<- RF_func(10)
RF50<- RF_func(50)
RF100<- RF_func(100)
RF200<- RF_func(200)
RF300<- RF_func(300)
RF400<- RF_func(400)
RF500<- RF_func(500)
RF750<- RF_func(750)
RF1000<- RF_func(1000)

# Confusion Matrices of Testing data for different number of ntrees
conf10<- conf_percentage(RF10$test_conf); conf10
conf50<- conf_percentage(RF50$test_conf); conf50
conf100<- conf_percentage(RF100$test_conf); conf100
conf200<- conf_percentage(RF200$test_conf); conf200
conf300<- conf_percentage(RF300$test_conf); conf300
conf400<- conf_percentage(RF400$test_conf); conf400
conf500<- conf_percentage(RF500$test_conf); conf500
conf750<- conf_percentage(RF750$test_conf); conf750
conf1000<- conf_percentage(RF1000$test_conf); conf1000

# Global accuracy of training data obtained from randomForest algorithm
a10_train= accuracy(RF10$train_conf)
a50_train= accuracy(RF50$train_conf)
a100_train= accuracy(RF100$train_conf)
a200_train= accuracy(RF200$train_conf)
a300_train= accuracy(RF300$train_conf)
a400_train= accuracy(RF400$train_conf)
a500_train= accuracy(RF500$train_conf)
a750_train= accuracy(RF750$train_conf)
a1000_train= accuracy(RF1000$train_conf)

# Global accuracy of testing data obtained from randomForest algorithm
a10_test= accuracy(RF10$test_conf)
a50_test= accuracy(RF50$test_conf)
a100_test= accuracy(RF100$test_conf)
a200_test= accuracy(RF200$test_conf)
a300_test= accuracy(RF300$test_conf)
a400_test= accuracy(RF400$test_conf)
a500_test= accuracy(RF500$test_conf)
a750_test= accuracy(RF750$test_conf)
a1000_test= accuracy(RF1000$test_conf)


ntrees<- c(10,50,100,200,300,400,500,750,1000)
acc_train<- c(a10_train,a50_train,a100_train,a200_train,a300_train,a400_train,a500_train,a750_train,a1000_train)
acc_test<- c(a10_test,a50_test,a100_test,a200_test,a300_test,a400_test,a500_test,a750_test,a1000_test)

acc_train
acc_test

# Accuracy vs. ntrees plot
plot(ntrees,acc_train,
     ylim=c(75,100),#range(c(acc_train,acc_test)),
     type='b', col='red',
     main='Accuracy vs. ntrees', ylab='accuracy')
par(new=TRUE)
plot(ntrees,acc_test, type='b', 
     ylim=c(75,100),#range(c(acc_train,acc_test)),
     col='blue',xlab="",ylab="")
legend("bottomright", legend=c("Train",'Test'),
       col=c("red","blue"),lwd=1, inset=c(0.02,0.03))

conf_C1<- c(conf10[1,1],conf50[1,1],conf100[1,1],conf200[1,1],conf300[1,1],conf400[1,1] 
            ,conf500[1,1],conf750[1,1],conf1000[1,1])
conf_C2<- c(conf10[2,2],conf50[2,2],conf100[2,2],conf200[2,2],conf300[2,2],conf400[2,2]
            ,conf500[2,2],conf750[2,2],conf1000[2,2])
conf_C3<- c(conf10[3,3],conf50[3,3],conf100[3,3],conf200[3,3],conf300[3,3],conf400[3,3]
            ,conf500[3,3],conf750[3,3],conf1000[3,3])


# 3 diagonal coefficients of confusion matrices (Each Class accuracy) vs. ntrees plot
plot(ntrees,conf_C1,type='b', ylim=range(c(conf_C1,conf_C2,conf_C3)),
     col='blue', xlab='Number of trees', ylab='Class Accuracy', 
     main='Class Accuracy vs. Number of trees')
par(new=TRUE)
plot(ntrees,conf_C2, type='b', ylim=range(c(conf_C1,conf_C2,conf_C3)),
     col='red',xlab="",ylab="")
par(new=TRUE)
plot(ntrees,conf_C3, type='b', ylim=range(c(conf_C1,conf_C2,conf_C3)),
     col='darkgreen',xlab="",ylab="")
legend("bottomright", legend=c("C1","C2","C3"),
       col=c("blue","red","darkgreen"),lwd=1, inset=c(0.02,0.03))
#----------------------------------------------------------------------

bntr=500 #chosen after evaluating accuracy plots and confusion matrices


bestRF<- randomForest(class~., data=TRAINSET, 
                      ntree=bntr, mtry=round(sqrt(29),0), importance=TRUE)
bestRF.test<- predict(bestRF,x.TESTSET)
bestRF.test_conf<- table('TRUE'=y.TESTSET,'PREDICTION'=bestRF.test)
bestRF_conf<- conf_percentage(bestRF.test_conf)  #Confusion matrix of bestRF
bestRF_conf
accuracy(bestRF.test_conf)

head(CORR[,1:6])
L  #eigenvalues obtained from HW3 for post-PCA

imp<- as.data.frame(importance(bestRF))
imp_eigen<- data.frame(imp$MeanDecreaseGini,imp$MeanDecreaseAccuracy, L)
head(imp_eigen)
varImpPlot(bestRF) #shows that X1 is the most important feature followed by X3

# Mean Decrease Accuracy - How much the model accuracy decreases if we drop that variable.
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index 
# used for the calculation of splits in trees.
# Higher the value of mean decrease accuracy or mean decrease gini score , 
# higher the importance of the feature in the model. 

#-----------------------------------------------------------
#most important feature Histogram Comparison
par(mfrow=c(2,2))
hist(C1$Age, breaks=10, xlim=c(-2,5), ylim=c(0,500), col='goldenrod')
hist(C2$Age, breaks=10,  xlim=c(-2,5), ylim=c(0,500), col='goldenrod')
hist(C3$Age, breaks=10, xlim=c(-2,5), ylim=c(0,500), col='goldenrod')

#least important feature Histogram Comparison
par(mfrow=c(2,2))
hist(C1$CALCAlways, ylim=c(0,1000), breaks=2,col='lightpink')  
hist(C2$CALCAlways, ylim=c(0,1000), breaks=2, col='lightpink')
hist(C3$CALCAlways, ylim=c(0,1000), breaks=2, col='lightpink')


# KS Test for most and least important feature in each class

ks.test(C1$Age,C2$Age,alternative = 'two.sided')
ks.test(C1$Age,C3$Age,alternative = 'two.sided')
ks.test(C2$Age,C3$Age,alternative = 'two.sided')

ks.test(C1$CALCAlways,C2$CALCAlways,alternative = 'two.sided')
ks.test(C1$CALCAlways,C3$CALCAlways,alternative = 'two.sided')
ks.test(C2$CALCAlways,C3$CALCAlways,alternative = 'two.sided')
