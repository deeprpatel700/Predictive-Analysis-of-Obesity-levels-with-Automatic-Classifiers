'''
Name: Deep Patel 
Project: Factors affecting Obesity and Classification of Obesity levels
Project Section: Principal Component Analysis (PCA) 

Subsections
1. Small Random Perturbations
2. Standardization
3. Principal Component Analysis (PCA)
4. Visualization of Principal Components

'''

setwd("C:\\Users\\deepp\\Google Drive\\MSDS\\MATH 6350 Data Mining")
getwd()
obesity_clean<- read.csv('Obesity_Post-Binary.csv') #reading the file
head(obesity_clean)   #Checking the dataset


# Adding Small Random Perturbations to Binary data to avoid too many ties error in K-means
obesity_features<- (obesity_clean[1:29]) 
randnum<-function(){(1+((runif((2111*29), min=0, max=1)-0.5)/1000))}
dim(obesity_features)
RDATA<- matrix(data=randnum(),nrow=2111,ncol=29)
head(RDATA[,1:6])
dim(RDATA)
OB_SRP<- as.data.frame((RDATA))
head(OB_SRP[,1:6])
head(obesity_features[,1:6])
obesity_num<- obesity_features*OB_SRP
head(obesity_num[,25:29])

#-------------------------------------------------------------------------------
# Standardization of the dataset / Rescaled data matrix

OBESITY_mean_std <- sapply(obesity_num, function(obesity_num) 
  c( "Stand dev" = sd(obesity_num,na.rm=TRUE), 
     "Mean"= mean(obesity_num,na.rm=TRUE)))

count = 0
for (Xj in obesity_features) {
  count = count + 1
  mean = mean(Xj)
  sd = sd(Xj)
  Yj = (Xj-mean)/sd
  
  if (count==1) {SDATA <- data.frame('r0c0'=Yj); next}
  SDATA <- cbind(SDATA,Yj)
}
colnames(SDATA) <- colnames(obesity_features)
#write.csv(SDATA,file='Obesity_SDATA.csv')

SDATA_OB<- cbind(SDATA,'Class'= obesity_clean$Classes)
#write.csv(SDATA_OB, file='OBESITY_Standardized_Pre-clone.csv')

#------------------------------------------------------------------------
# Principal Component Analysis
CORR <- cor(SDATA)
# write.csv(CORR, file="SDATA_CORR.csv")

ev<- eigen(CORR)
L<- ev$values  #eigenvalues of the standardized dataset
W<- ev$vectors #eigenvectors of the standaridzed dataset

# write.csv(L, file="SDATA_CORR_eigenvalues.csv")  #Uncomment if need to save file
# write.csv(W, file="SDATA_CORR_eigenvectors.csv") #Uncomment if need to save file

# Plot of Eigenvalues vs. number of features
plot(1:29, L, type='l', main="Eigenvalues(Lr) vs. r",
     xlab="r", ylab="eigenvalues(L)", col="red")

# BUILDING A TABLE FOR first 3 eigenvalues, PVE% for each, PVE% accumulating
ev_data<- data.frame(L)
ev_data$new<- cumsum(L)
head(ev_data)
PVE = ((ev_data$new)/sum(L))
head(PVE)
ev_data$PVE<- PVE
ev_data$r<- 1:29
ev_data$PVE_percent<- ev_data$PVE*100
pve_tab<- ev_data[,c(4,1,5)]
View(round((pve_tab),2))
r95 = which(ev_data$PVE >= .95)[1]   #directly finds r95 which shows features with 95% of variance
r95

# Plot of Perventage of variance vs. number of features
plot(PVE*100, xlab="r",
     ylab="PVE(%)",
     main="Percentage of Variance Explained vs. r",
     ylim=c(0,100),type='l',col='blue')


coeff_newY<- t(W); head(coeff_newY, n=2)  #matrix of coefficients of new features

new_features<- t(W) %*% t(SDATA)   
new_features = t(new_features)
preNFDATA = as.data.frame(new_features) # created dataframe version of new features
dim(preNFDATA)
names(preNFDATA)[1:29]<- paste("Y(", seq(1, 29), sep="",")")

########## RENAME SDATA_OB$Class to C1 C2 C3 for machine learning algorithms #######

library(dplyr)
class_r<- case_when(SDATA_OB$Class=="Non-Obese"~"C1",
                    SDATA_OB$Class=="Overweight"~"C2",
                    SDATA_OB$Class=="Obese"~"C3")
SDATA_OB$class<- class_r
SDATA_OB$Class<- NULL

C1<- filter(SDATA_OB, class =='C1')
C2<- filter(SDATA_OB, class =='C2')
C3<- filter(SDATA_OB, class =='C3')
#------------------------------------------------------------------------

# This is the new feature data with only the principal components for visualization
NFDATA = preNFDATA[1:r95]
colnames(NFDATA)

# Adding class to NFDATA
NFDATA$class = as.factor(SDATA_OB$class)

NFDATA_12<- filter(NFDATA, class== 'C1' | class=='C2')
NFDATA_12$class= factor(NFDATA_12$class)
NFDATA_13<- filter(NFDATA, class== 'C1' | class=='C3')
NFDATA_13$class= factor(NFDATA_13$class)
NFDATA_23<- filter(NFDATA, class== 'C2' | class=='C3')
NFDATA_23$class= factor(NFDATA_23$class)

levels(NFDATA_13$class)
library(scatterplot3d)

# 3D Plot - C1 vs. C2
colors=c('darkgreen','red')
scatterplot3d(NFDATA_12[,1], NFDATA_12[,2], NFDATA_12[,3],
              angle=55, pch=19, cex.symbols=I(.9),
              color=colors[NFDATA_12[,20]],
              main="3D Scatter Plot of PCs in C1 vs C2",
              xlab = "PC1",ylab = "PC2",zlab = "PC3")
legend("topright", legend = c('C1','C2'),
       col = colors, 
       pch = 16, 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.85, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# 3D plot= C1 vs. C3
colors=c('darkgreen','blue')
scatterplot3d(NFDATA_13[,1], NFDATA_13[,2], NFDATA_13[,3],
              angle=55, pch=16, cex.symbols=I(0.9)
              , color=colors[NFDATA_13[,20]],
              main="3D Scatter Plot of PCs in C1 vs C3",
              xlab = "PC1",ylab = "PC2",zlab = "PC3")
legend("topright", legend = c('C1','C3'),
       col = colors, 
       pch = 16, 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.85, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# 3D plot= C2 vs. C3
colors=c('red','blue')
scatterplot3d(NFDATA_23[,1], NFDATA_23[,2], NFDATA_23[,3],
              angle=55, pch=16, color=colors[NFDATA_23[,18]],
              cex.symbols = I(0.9),
              main="3D Scatter Plot of PCs in C2 vs C3",
              xlab = "PC1",ylab = "PC2",zlab = "PC3")
legend("topright", legend = c('C2','C3'),
       col = colors, 
       pch = 16, 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.85, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


library(lattice)
p3DATA = NFDATA[1:3]
p3DATA2 = NFDATA[1:3]
p3DATA2$class = as.factor(NFDATA$class)

# 3D Plot- Option 1
cloud(V3 ~ V1 + V2, p3DATA, pch=1, groups=p3DATA2$class, 
      scales = list(arrows = FALSE), main = "3D plot, Y1 vs Y2 vs Y3", 
      xlab = "Y1", ylab = "Y2", zlab = "Y3", 
      auto.key = list(title="Class", corner=c(.98, 1)))

colors <- c('darkgreen','red','purple')

# 3D Plot - Option 2
scatterplot3d(p3DATA2[,1], p3DATA2[,2], p3DATA2[,3],
              angle=25, pch=16, color=colors[p3DATA2[,4]],
              main="3D Scatter Plot of PCs",
              xlab = "PC1",ylab = "PC2",zlab = "PC3")

legend("topright", legend = c('C1','C2','C3'),
       col = colors, 
       pch = 16, 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.85, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


