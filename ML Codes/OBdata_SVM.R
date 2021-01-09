''' 
Name: Deep Patel 
Project: Factors affecting Obesity and Classification of Obesity levels
Project Section: Support Vector Machine- SVM (Supervised learning)

Subsections
1. Balancing two chosen classes in Training and Testing set
2. SVM analysis using e1071 library
3. SVM analysis using caret package

'''
# Use the same Train/Test split used in RandomForest 
svm.trainCL1<- trainCL1
svm.trainCL3<- trainCL3
svm.testCL1<- testCL1
svm.testCL3<- testCL3

SVM.TRAINSET <- rbind(svm.trainCL1,svm.trainCL1,svm.trainCL3)
SVM.TESTSET <- rbind(svm.testCL1,svm.testCL1,svm.testCL3)
SVM.TRAINSET$class<- factor(SVM.TRAINSET$class)
SVM.TESTSET$class<- factor(SVM.TESTSET$class)

x.SVM.TRAINSET <- SVM.TRAINSET[-30]
y.SVM.TRAINSET <- SVM.TRAINSET[,30]
x.SVM.TESTSET <- SVM.TESTSET[-30]
y.SVM.TESTSET <- SVM.TESTSET[,30]

table(y.SVM.TRAINSET)
table(y.SVM.TESTSET)

library(e1071)
library(caret)


tmodel<- tune.svm(class~., data=SVM.TRAINSET, kernel='linear', 
                  cost=c(seq(0.1,2,0.3),2^(2:7)))
tmodel$best.model

train_perf<- tmodel$performances
train_perf$accuracy<- (1-(train_perf$error))*100
plot(train_perf$cost,train_perf$accuracy, type='l', col='blue') #best cost= 7
summary(tmodel)
plot(tmodel)

tmodel3<- tune.svm(class~., data=SVM.TESTSET, kernel='linear',
                   cost=c(seq(0.1,2,0.3),2^(2:7)))
test_perf<- tmodel3$performances
test_perf$accuracy<- (1- (test_perf$error))*100
summary(tmodel3)
plot(tmodel3)

plot(train_perf$cost,train_perf$accuracy, type='l', 
     ylim=range(c(train_perf$accuracy,test_perf$accuracy)),col='red',
     main='Cost vs. Accuracy', xlab='Cost', ylab='Accuracy') #best cost= 7
par(new=TRUE)
plot(test_perf$cost,test_perf$accuracy, type='l',
     ylim=range(c(train_perf$accuracy,test_perf$accuracy)), col='blue',
     main='',xlab='',ylab='')
legend("bottomright", legend=c("Train",'Test'),
       col=c("red","blue"),lwd=1, inset=c(0.02,0.03))

mymodel<- tmodel$best.model
plot(mymodel, data=SVM.TRAINSET)

bestc<- 1.9
svm.trial1<- svm(class~., data= SVM.TRAINSET,
                 type='C-classification', kernel='linear', 
                 cost=128, scale = FALSE)
summary(svm.trial1)

tot_train<- 1671
nsv<- c(403,386,386,379,379,379,379,379,379,380,379,380,379) #obtained from single launchings
nsv_ratio<- nsv/tot_train
cost_c<- c(seq(0.1,2,0.3),2^(2:7))
plot(cost_c,nsv_ratio, type='l', col='blue', ylab='NSV RATIO', xlab='Cost',
     main='Cost vs. Number of Support Vectors Ratio')

#Plot of first two important features classified from RF
plot(mymodel, data=SVM.TRAINSET, Age~FCVC) 
plot(mymodel, data=SVM.TESTSET, Age~FCVC)


pred_best<- predict(mymodel,SVM.TESTSET)
best_tab<- table('TRUE'=y.SVM.TESTSET, 'Prediction'=pred_best)
conf_percentage(best_tab)
accuracy(best_tab)


#USING CARET PACKAGE
trctrl<- trainControl(method='repeatedcv',number=10,repeats = 3)
svm.trial2<- train(class~., data= SVM.TRAINSET, method= 'svmLinear',
                   trControl=trctrl,
                   preProcess=c('center','scale'), tuneLength=10)
svm.trial2
test_pred<- predict(svm.trial2, newdata = SVM.TESTSET)
svm.predict<- table('TRUE'=y.SVM.TESTSET, 'PREDICTION'=test_pred)
conf_percentage(svm.predict)
accuracy(svm.predict)
grid= expand.grid(C = c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5,7))
svm.trial2c<- train(class~., data= SVM.TRAINSET, method= 'svmLinear',
                    tuneGrid=grid,
                    trControl=trctrl,
                    preProcess=c('center','scale'), tuneLength=10)
svm.trial2
plot(svm.trial2)
