# MATH 6350 FINAL 
# Name: Deep Patel

setwd("C:\\Users\\deepp\\Google Drive\\MSDS\\MATH 6350 Data Mining")
getwd()
obesityOR<- read.csv('ObesityDataSet.csv') # Obesity Data set

library(dplyr) 


obesity<- obesityOR
colnames(obesity)
colnames(obesity)[17]<- "Type"
colnames(obesity)

#Regrouping Classes
filter_nonob<- ifelse(obesity$Type=="Insufficient_Weight"|obesity$Type=="Normal_Weight","Non-Obese", NA)
filter_over<- ifelse(obesity$Type=="Overweight_Level_I" | obesity$Type=="Overweight_Level_II","Overweight", NA)
filter_obesity<-ifelse((obesity$Type=="Obesity_Type_I" | obesity$Type=="Obesity_Type_II"|obesity$Type=="Obesity_Type_III"), "Obese", NA)

obesity$Nonob<-filter_nonob
obesity$over<- filter_over
obesity$obese<- filter_obesity
obesity$Classes<- coalesce(obesity$Nonob,obesity$over,obesity$obese)
obesity$Nonob<-NULL
obesity$over<- NULL
obesity$obese<- NULL

obesity$Type[250:260] #Just to check
obesity$Classes[250:260] #Just to check

#obesity$Type<- NULL

#Separating classes
CL1<- filter(obesity, Classes=="Non-Obese")
CL2<- filter(obesity, Classes=="Overweight")
CL3<- filter(obesity, Classes=="Obese")

# Checks & Omits Missing values if any
CL1<- CL1[complete.cases(CL1),]
CL2<- CL2[complete.cases(CL2),]
CL3<- CL2[complete.cases(CL3),]


#Turning Categorical Variables into Numerical
#var 1- Gender
Gender_Male<- ifelse(obesity$Gender=="Male",1,0)
Gender_Female<- ifelse(obesity$Gender=="Female",1,0)
obesity$Gender_Male<- Gender_Male
obesity$Gender_Female<- Gender_Female

#var 2- Age (numerical- normally distributed between 14 and 61)

#var 3- Family history with overweight
fam_history_Yes<- ifelse(obesity$family_history_with_overweight=="yes",1,0)
fam_history_No<-ifelse(obesity$family_history_with_overweight=="no",1,0)
obesity$Family_History_YES<- fam_history_Yes
obesity$Family_History_NO<- fam_history_No

#var 4- FAVC (Frequent consumption of high caloric food)
FAVC_yes<- ifelse(obesity$FAVC=="yes",1,0)
FAVC_no<- ifelse(obesity$FAVC=="no",1,0)
obesity$FAVC_YES<- FAVC_yes
obesity$FAVC_NO<- FAVC_no

#var 5- numerical
#var 6- numerical

#var 7- below var 10

#var 8- SMOKE 
smoke_yes<- ifelse(obesity$SMOKE=="yes",1,0)
smoke_no<- ifelse(obesity$SMOKE=="no",1,0)
obesity$Smoke_YES<- smoke_yes
obesity$Smoke_NO<- smoke_no


#var 10- SCC (Calorie consumption monitoring)
scc_yes<- ifelse(obesity$SCC=="yes",1,0)
scc_no<- ifelse(obesity$SCC=="no",1,0)
obesity$SCC_YES<- scc_yes
obesity$SCC_NO<- scc_no

#var 7- CAEC (Consumption of food between meals)- 4 categories
CAEC_binary<- cbind(with(obesity, model.matrix(~ CAEC + 0)))
obesity1<- data.frame(obesity,CAEC_binary)


#var 13- 
CALC_binary<- cbind(with(obesity, model.matrix(~ CALC + 0)))
obesity2<- data.frame(obesity1,CALC_binary)

#var 14-
MTRANS_binary<- cbind(with(obesity, model.matrix(~ MTRANS + 0)))
obesity3<- data.frame(obesity2,MTRANS_binary)
colnames(obesity3)
obesity_clean<- obesity3[,c(2,7,8,11,13,14,19,20,21,22,23:41,18)]
#obesity_clean<- obesity3[,c(19,20,2,21,22,7,8,11,13,14,23:41,18)]
colnames(obesity_clean)

#write.csv(obesity_clean, file='Obesity_Post-Binary.csv')

obesity_clean<- read.csv('Obesity_Post-Binary.csv')

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

#--- Normalization using LICORS--------------#
library(LICORS)
SDATA<- normalize(data.matrix(obesity_num))
head(S_obesity)
SDATA<- as.data.frame((SDATA))
SDATA_OB<- cbind(SDATA,'Class'= obesity_clean$Classes)
head(S_OBESITY)

# write.csv(S_OBESITY, file='OBESITY_LICORS_Standardized_Pre-clone.csv')
#------------------------------------------------------------------------------------------------------

OBESITY_mean_std <- sapply(obesity_num, function(obesity_num) 
                    c( "Stand dev" = sd(obesity_num,na.rm=TRUE), 
                        "Mean"= mean(obesity_num,na.rm=TRUE)))

# Creating "SDATA" - standardized data set / Rescaled data matrix
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
head(SDATA)

SDATA_OB<- cbind(SDATA,'Class'= obesity_clean$Classes)
head(SDATA_OB)



# write.csv(SDATA_OB, file='OBESITY_Standardized_Pre-clone.csv')

#--------------------------------------------------------------------------------------------
CORR <- cor(SDATA)
View(round(CORR[1:5,1:5],2))
# write.csv(CORR, file="SDATA_CORR.csv")

ev<- eigen(CORR)
L<- ev$values; View(round(L[1:3],2))
W<- ev$vectors; View(round((W[,1:3]),2))

# library(psych)
# tr(CORR)     #checking sum of trace of CORR matrix- should equal 29

# write.csv(L, file="SDATA_CORR_eigenvalues.csv")
# write.csv(W, file="SDATA_CORR_eigenvectors.csv")

plot(1:29, L, type='l', main="Eigenvalues(Lr) vs. r",
     xlab="r", ylab="eigenvalues(L)", col="red")

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
#BUILD A TABLE FOR first 3 eigenvalues, PVE% for each, PVE% accumulating
r95 = which(ev_data$PVE >= .90)[1]   #directly finds r95
r95
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
View(round((preNFDATA[,1:6]),2))
write.csv(new_features, file="NEW_FEATURES_Pre-PCA.csv")

########## RENAME SDATA_OB$Class to C1 C2 C3 #######


class_r<- case_when(SDATA_OB$Class=="Non-Obese"~"C1",
                    SDATA_OB$Class=="Overweight"~"C2",
                    SDATA_OB$Class=="Obese"~"C3")
SDATA_OB$class<- class_r
SDATA_OB[250:260,26:30]
SDATA_OB$Class<- NULL


C1<- filter(SDATA_OB, class =='C1')
C2<- filter(SDATA_OB, class =='C2')
C3<- filter(SDATA_OB, class =='C3')



#------------------------------------------------------------------------

# This is the new feature data with only the principal components
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
              color=colors[NFDATA_12[,18]],
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
              , color=colors[NFDATA_13[,18]],
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

############################ QUESTION 1 DONE ################################
# Use SDATA

### Q2 Kmeans
#pick_k = 200
k_values = 17   #c(seq(1,9,3),seq(10,48,4),seq(50,100,10))
start_time <- Sys.time() # Start timer
GINI_indexes_per = numeric(length(k_values))#pick_k
GINI_indexes_full= numeric(length(k_values))
prob_mat = numeric(length(k_values))
Qm <- numeric(length(k_values))
Perf.m <- numeric(length(k_values))
index = 0
for (k in k_values){ #seq(1,pick_k)
  index = index+1
  out <- kmeans(SDATA, k, nstart=50, iter.max = 50000) 
  Q <- out$tot.withinss/out$totss
  Perf <- 1-(out$tot.withinss/out$totss)
  Qm[index] = Q
  Perf.m[index] = Perf
  
  # Calculating the GINI index for each K value
  clusters_size = out$size
  var1 = character(0)
  for(i in 1:k){
    var2 = paste('CLU', as.character(i), sep = "")
    var1 = c(var1, var2)
  }
  prob_dist_matrix = matrix(nrow = k, ncol = 3, dimnames = list(var1, c('C1%','C2%','C3%'))) #,'gini_index'
  cluster_num = 0
  for (i in clusters_size){
    cluster_num = cluster_num + 1 #which(clusters_size == i)
    cluster_index <- out$cluster == cluster_num # Indexes of cases belonging to a cluster
    cluster_cases <- SDATA[cluster_index,]
    cluster <- cbind(cluster_cases, 'class'= SDATA_OB[cluster_index,]$class) # Adding column of class names
    n_total_cases = i #dim(cluster['Position'])[1]
    dist_c1 = sum(cluster['class'] == 'C1')/n_total_cases
    dist_c2 = sum(cluster['class'] == 'C2')/n_total_cases
    dist_c3 = sum(cluster['class'] == 'C3')/n_total_cases
    #gini_index = (dist_CB*(1-dist_CB))+(dist_FB*(1-dist_FB))+(dist_CM*(1-dist_CM))+(dist_ST*(1-dist_ST)) # Gini Index for a cluster. High purity when close to zero
    prob_dist_cluster <- c(dist_c1,dist_c2,dist_c3) #,gini_index
    prob_dist_matrix[cluster_num,] = prob_dist_cluster
  }
  ginis = numeric(k)
  for (i in c(1:k)){
    gini = prob_dist_matrix[i,] %*% (1-prob_dist_matrix[i,])
    ginis[i] = gini}
  prob_dist_matrix <- cbind(prob_dist_matrix,'gini_index'=ginis)
  impurity_clustering = sum(prob_dist_matrix[,'gini_index'])
  percent_impurity = impurity_clustering/((2/3)*k)
  
  prob_mat[index]<- list(prob_dist_matrix)
  GINI_indexes_full[index] = impurity_clustering
  GINI_indexes_per[index] = percent_impurity 
}
Qm

round(prob_dist_matrix,3)
clusters_size
out$withinss
impurity_clustering
Qm
end_time <- Sys.time() # End timer

kmeans_time = end_time - start_time
print(kmeans_time)

par(mar=c( 5.1,4.1,4.1,3.1))
par(mfrow=c(2,2))
# Q(m) = sum(withinss)/total_disp
plot(k_values,Qm,xlab = 'k Clusters', ylab = 'Q(m)dasdasd',
     main = 'Elbow Method for Clustering Quality',col='red', type='l',
     xaxt = 'n')
axis(1,at = seq(1, 1000, by = 2), las=2)

plot(k_values,Perf.m,xlab = 'k Clusters', ylab = 'Perf(k)',
     main = 'Clustering Performance vs Number of Clusters',col='red', type='l',
     xaxt = 'n')
axis(1,at = seq(1, 1000, by = 2), las=2)

plot(k_values,GINI_indexes_full,xlab = 'k Clusters', ylab = 'GINI(k) full',
     main = 'GINI Full vs Number of Clusters',col='red', type='l',
     xaxt='n')
axis(1,at = seq(1, 1000, by = 2), las=2)

plot(k_values,GINI_indexes_per,xlab = 'k Clusters', ylab = 'GINI(k) in %',
     main = 'GINI % vs Number of Clusters',col='red', type='l',
     xaxt='n')
axis(1,at = seq(1, 1000, by = 2), las=2)
par(mfrow=c(1,1))






#---------------------

#From elbow method, the best k=40
k = 17
out <- kmeans(SDATA, k, nstart=50, iter.max=50000)
centers <-  out$centers
out$size

CORR_center <- cor(centers)
Z_c = eigen(CORR_center)
W_c <- Z_c$vectors # matrix W -> eigenvectors

View(W_c)

vectors_3D <- t(t(W_c[,1:3]) %*% t(centers))
View(round(t(vectors_3D),2))
colnames(vectors_3D) <- c('V1','V2','V3')
rownames(vectors_3D) <- c('CEN1','CEN2','CEN3','CEN4','CEN5','CEN6','CEN7','CEN8','CEN9','CEN10',
                          'CEN11','CEN12','CEN13','CEN14','CEN15','CEN16','CEN17','CEN18','CEN19','CEN20',
                          'CEN21','CEN22','CEN23','CEN24','CEN25','CEN26','CEN27','CEN28','CEN29','CEN30',
                          'CEN31','CEN32','CEN33','CEN34','CEN35','CEN36','CEN37','CEN38','CEN39','CEN40')
vectors_3D

# 3D plot of vectors
library(scatterplot3d)
colors = c('cyan', 'red', 'darkorange','black','blue','azure2', 'beige','bisque2','cornflowerblue','blue4',
           'cornsilk2','brown','brown1','darkgoldenrod','darkgoldenrod1','darkgoldenrod3','darkgreen','thistle1','deepskyblue1','darkmagenta',
           'dodgerblue3','firebrick','firebrick1','darkorchid1','darksalmon','gold','gold3','deeppink','deeppink3','gray48',
           'gray44','gray84','green','green3','greenyellow','lightpink','orchid1','slategray1','yellow','tan')
colors = colors[1:k]
legend.k = character(0)
for(i in 1:k){
  var3 = paste('CEN', as.character(i), sep = "")
  legend.k = c(legend.k, var3)
}

scatterplot3d(vectors_3D[,1], vectors_3D[,2], vectors_3D[,3], angle=55, pch=16, 
              color=colors,
              main="3D Scatter Plot of Centroids",
              xlab = "V1",ylab = "V2",zlab = "V3")
legend("topright", legend = legend.k,
       col = colors, 
       pch = 16, 
       bty = "n", 
       pt.cex = .7, 
       cex = 0.50, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

clusters_size <- out$size

clusters_size
bigCLU_index <- out$cluster == which(clusters_size == 372)  #TRY w/ 140 as well
bigCLU_cases <- SDATA[bigCLU_index,]
Smallgini_cases <- cbind(bigCLU_cases,'class'=SDATA_OB[bigCLU_index,]$class) 
table(Smallgini_cases$class)

write.csv(Smallgini_cases,file='SmallGINI_CASES.csv')

randrum2<- matrix(data=(1+((runif((nrow(bigCLU_cases)*29), min=0, max=1)-0.5)/5000))
                  ,nrow=nrow(bigCLU_cases),ncol=29)
bigCLU<- bigCLU_cases*randrum2
dim(bigCLU_cases)
dim(bigCLU)

# Uncomment below lines if using bigCLU CORR w/ randrum
CORR_bigCLU <- cor(bigCLU)
Z_bigCLU = eigen(CORR_bigCLU)
W_bigCLU <- Z_bigCLU$vectors # matrix W -> eigenvectors
t.vectors_3D <- t(t(W_bigCLU[,1:3]) %*% t(bigCLU))

# t.vectors_3D <- t(t(W[,1:3]) %*% t(bigCLU_cases))

t.vectors_3D.df <- data.frame(t.vectors_3D)

# Attaching class names to the matrix
t.vectors_3D.df <- cbind(t.vectors_3D.df,'class'=SDATA_OB[bigCLU_index,]$class) 
table(t.vectors_3D.df$class)

# write.csv(t.vectors_3D.df,'bigCLU_times_corr.csv' )
colors <- c('green','red','purple')

scatterplot3d(t.vectors_3D[,1], t.vectors_3D[,2], t.vectors_3D[,3], angle=55, 
              pch=16, color=colors[factor(t.vectors_3D.df$class)],
              main="3D Scatter Plot of Cluster with Smallest GINI",
              xlab = "V1",
              ylab = "V2",
              zlab = "V3")
table(colors[factor(t.vectors_3D.df$class)])
table(factor(t.vectors_3D.df$class))
legend("topright", legend = c('C1','C2','C3'),
       col = colors, 
       pch = 16, 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.85, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# Matrix for probability distribution of classes.
var1 = character(0)
for(i in 1:k){
  var2 = paste('CLU', as.character(i), sep = "")
  var1 = c(var1, var2)
}

prob_dist_matrix = matrix(nrow = k_values, ncol = 4, 
                          dimnames = list(var1, c('C1%','C2%','C3%','gini_index')))
for (i in clusters_size){
  cluster_num = which(clusters_size == i)
  cluster_index <- out$cluster == cluster_num # Indexes of cases belonging to a cluster
  cluster_cases <- SDATA[cluster_index,]
  # Adding column of class names
  cluster <- cbind(cluster_cases,'class'=SDATA_OB[cluster_index,]$class) 
  n_total_cases = dim(cluster['class'])[1]
  dist_c1 = sum(cluster['class'] == 'C1')/n_total_cases
  dist_c2 = sum(cluster['class'] == 'C2')/n_total_cases
  dist_c3 = sum(cluster['class'] == 'C3')/n_total_cases
  # Gini Index for a cluster. High purity when close to zero
  gini_index = dist_c1*(1-dist_c1) + dist_c2*(1-dist_c2) + dist_c3*(1-dist_c3) 
  prob_dist_cluster <- c(dist_c1,dist_c2,dist_c3,gini_index)
  prob_dist_matrix[cluster_num,] = prob_dist_cluster
}
prob_dist_matrix
impurity_clustering = sum(prob_dist_matrix[,'gini_index']) 
clusters_size

Ultimate_mat<- as.data.frame(prob_dist_matrix)
size_dat<- as.data.frame(clusters_size)
disp<- as.data.frame(out$withinss)
Ultimate_mat$Size<- size_dat$clusters_size
Ultimate_mat$`C1%`<- Ultimate_mat$`C1%`*100
Ultimate_mat$`C2%`<- Ultimate_mat$`C2%`*100
Ultimate_mat$`C3%`<- Ultimate_mat$`C3%`*100
Ultimate_mat$Dispersion<- round(disp$`out$withinss`,0)
Ultimate_mat$Topj<- FREQ[4,]
head(Ultimate_mat)
head(out$centers)
colnames(Ultimate_mat)
Ultimate_mat_ordered<- Ultimate_mat[,c(5,4,6,1,2,3,7)]
head(Ultimate_mat_ordered)
colnames(Ultimate_mat_ordered)[4]= 'C1% FREQ'
colnames(Ultimate_mat_ordered)[5]= 'C2% FREQ'
colnames(Ultimate_mat_ordered)[6]= 'C3% FREQ'
colnames(Ultimate_mat_ordered)[7]= 'Top Class'
View(round(Ultimate_mat_ordered,2))

# Matrix of frequencies
FREQ = matrix(nrow = 3, ncol = k_values, dimnames = list(c('C1','C2','C3'),
                                                  var1))
clusters_size
x=seq(1,k_values,by=1)
plot(x,prob_dist_matrix[,'gini_index'],type='b') #GINI vs. K PLOT?


for (j in seq(1:k_values)){
  cluster_index <- out$cluster == j # Indexes of cases belonging to a cluster
  cluster_cases <- SDATA[cluster_index,]
  # Adding column of class names
  cluster <- cbind(cluster_cases,'class'=SDATA_OB[cluster_index,]$class) 
  Sj = dim(cluster['class'])[1] # Cluster size
  A1j = sum(cluster$class == 'C1') # Class 1 intersection cluster
  A2j = sum(cluster$class == 'C2') # Class 2 intersection cluster
  A3j = sum(cluster$class == 'C3') # Class 3 intersection cluster
  column <- c(A1j,A2j,A3j)/Sj
  FREQ[,j] <- column
}

# Getting the class index TOPj (most frequent class per cluster)
Topj = apply(FREQ,2,which.max) 
FREQ <- rbind(FREQ,Topj)
head(FREQ)
nrow(cluster)
#---------------
Pred_n <- out$cluster
# Use the Topj calculated to assign each cluster to a class. 
for(i in seq(1:k_values)){
  Pred_n[Pred_n == i] = paste("C", as.character(FREQ[4,i]), sep = "")
}

predicted_classes <- Pred_n
true_classes <- SDATA_OB$class

#install.packages('caret')
library(caret)
conf_mat <- confusionMatrix(factor(predicted_classes),factor(true_classes),
                            dnn = c("Prediction", "True"))
CONF <- t(conf_mat$table) # Rows are true classes and columns are predicted
CONF


CONF_percent = CONF
table(true_classes)
table(predicted_classes)
global_acc = conf_mat$overall[1]*100


# Confusion matrix in %
for(i in seq(1:3)){
  for(j in seq(1:3)){
    CONF_percent[j, i] = round(CONF[j, i]/sum(CONF[j, 1:3]), 4)*100
  }
}

CONF_percent
###################  Q2 & Q3 DONE ############################################
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

##################### QUESTION 4 DONE ###########################################


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

RF100

conf10<- conf_percentage(RF10$test_conf); conf10
conf50<- conf_percentage(RF50$test_conf); conf50
conf100<- conf_percentage(RF100$test_conf); conf100
conf200<- conf_percentage(RF200$test_conf); conf200
conf300<- conf_percentage(RF300$test_conf); conf300
conf400<- conf_percentage(RF400$test_conf); conf400
conf500<- conf_percentage(RF500$test_conf); conf500
conf750<- conf_percentage(RF750$test_conf); conf750
conf1000<- conf_percentage(RF1000$test_conf); conf1000

a10_train= accuracy(RF10$train_conf)
a50_train= accuracy(RF50$train_conf)
a100_train= accuracy(RF100$train_conf)
a200_train= accuracy(RF200$train_conf)
a300_train= accuracy(RF300$train_conf)
a400_train= accuracy(RF400$train_conf)
a500_train= accuracy(RF500$train_conf)
a750_train= accuracy(RF750$train_conf)
a1000_train= accuracy(RF1000$train_conf)


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


############################ QUESTION 5 & 6 DONE ########################################

bntr=400 #chosen after evaluating accuracy plots and confusion matrices


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
plot(imp_eigen$L, imp_eigen$imp.MeanDecreaseGini, 
     xlab='Eigenvalues (L)', ylab='Importance by Mean Decrease Gini',
     main="Importance of Features (PC's)", col='red', pch=3)
plot(imp_eigen$L, imp_eigen$imp.MeanDecreaseAccuracy, 
     xlab='Eigenvalues (L)', ylab='Importance by Mean Decrease Accuracy',
     main="Importance of Features (PC's)", col='blue', pch=8, cex=1.5)

# Mean Decrease Accuracy - How much the model accuracy decreases if we drop that variable.
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index 
# used for the calculation of splits in trees.
# Higher the value of mean decrease accuracy or mean decrease gini score , 
# higher the importance of the feature in the model. 

################# QUESTION 7 DONE #######################################

#most important features across different classes
par(mfrow=c(2,2))
hist(C1$Age, breaks=10, xlim=c(-2,5), ylim=c(0,500), col='goldenrod')
hist(C2$Age, breaks=10,  xlim=c(-2,5), ylim=c(0,500), col='goldenrod')
hist(C3$Age, breaks=10, xlim=c(-2,5), ylim=c(0,500), col='goldenrod')

#Plot NCP, FCVC if time

#least important features
par(mfrow=c(2,2))
hist(C1$CALCAlways, ylim=c(0,1000), breaks=2,col='lightpink')  
hist(C2$CALCAlways, ylim=c(0,1000), breaks=2, col='lightpink')
hist(C3$CALCAlways, ylim=c(0,1000), breaks=2, col='lightpink')

# Plot MTRANSMotorbike and MTRANSBike if time

#Dataframes for KS-test
S_OBESITY12<- rbind(C1,C2)
S_OBESITY13<- rbind(C1,C3)
S_OBESITY23<- rbind(C2,C3)
nrow(S_OBESITY12)
nrow(S_OBESITY13)
nrow(S_OBESITY23)

ks.test(C1$Age,C2$Age,alternative = 'two.sided')
ks.test(C1$Age,C3$Age,alternative = 'two.sided')
ks.test(C2$Age,C3$Age,alternative = 'two.sided')

ks.test(C1$CALCAlways,C2$CALCAlways,alternative = 'two.sided')
ks.test(C1$CALCAlways,C3$CALCAlways,alternative = 'two.sided')
ks.test(C2$CALCAlways,C3$CALCAlways,alternative = 'two.sided')


###################### Come back to QUESTION 8 ##############################
clusters_size
bigCLU_index <- out$cluster == which(clusters_size == 372)  #TRY w/ 372 as well
bigCLU_cases <- SDATA[bigCLU_index,]
Smallgini_cases <- cbind(bigCLU_cases,'class'=SDATA_OB[bigCLU_index,]$class) 
table(Smallgini_cases$class)



dim(Smallgini_cases)
table(Smallgini_cases$class)

Smallgini_cases$class<- as.factor(Smallgini_cases$class)


smGINI_CL1 <- filter(Smallgini_cases, class == 'C1')
sm.n1<- nrow(smGINI_CL1)
sm.train1 <- sample(1:sm.n1, 0.8*sm.n1)
sm.trainCL1 <- smGINI_CL1[sm.train1,]
sm.testCL1 <- smGINI_CL1[-sm.train1,]

subtrainCL1<- sample(1:nrow(sm.trainCL1), (1/8)*nrow(sm.trainCL1))
subtrain_CL1<- sm.trainCL1[subtrainCL1,]
subtestCL1<- sample(1:nrow(sm.testCL1), (1/4)*nrow(sm.testCL1))
subtest_CL1<- sm.testCL1[subtestCL1,]


smGINI_CL2 <- filter(Smallgini_cases, class == 'C2')
sm.n2<- nrow(smGINI_CL2)
sm.train2 <- sample(1:sm.n2, 0.8*sm.n2)
sm.trainCL2 <- smGINI_CL2[sm.train2,]
sm.testCL2 <- smGINI_CL2[-sm.train2,]

subtrain_CL2<- sm.trainCL2[rep(1:nrow(sm.trainCL2),3),]
subtest_CL2<- sm.testCL2[rep(1:nrow(sm.testCL2),3),]

# randrum3<- matrix(data=(1+((runif((nrow(sm.trainCL2)*29), min=0, max=1)-0.5)/5000))
#                   ,nrow=nrow(sm.trainCL2),ncol=29)
# sm.train2_num<- sm.trainCL2[-30]
# train2clone<- randrum3*sm.train2_num
# train2clone2<- randrum3*sm.train2_num

smGINI_CL3 <- filter(Smallgini_cases, class == 'C3')
sm.n3<- nrow(smGINI_CL3)
sm.train3 <- sample(1:sm.n3, 0.8*sm.n3)
sm.trainCL3 <- smGINI_CL3[sm.train3,]
sm.testCL3 <- smGINI_CL3[-sm.train3,]

subtrainCL3<- sample(1:nrow(sm.trainCL3), (1/4)*nrow(sm.trainCL3))
subtrain_CL3<- sm.trainCL3[subtrainCL3,]
subtestCL3<- sample(1:nrow(sm.testCL3), (1/4)*nrow(sm.testCL3))
subtest_CL3<- sm.testCL3[subtestCL3,]
# subtrain_CL3<- sm.trainCL3[rep(1:nrow(sm.trainCL3),3),]
# subtest_CL3<- sm.testCL3[rep(1:nrow(sm.testCL3),3),]


sm.TRAINSET <- rbind(subtrain_CL2,subtrain_CL3)
sm.TESTSET <- rbind(subtest_CL2,subtest_CL3)

x.smTRAINSET <- sm.TRAINSET[-30]
y.smTRAINSET <- as.factor(sm.TRAINSET[,30])
x.smTESTSET <- sm.TESTSET[-30]
y.smTESTSET <- as.factor(sm.TESTSET[,30])

sm.TRAINSET$class<- factor(sm.TRAINSET$class)
sm.TESTSET$class<- factor(sm.TESTSET$class)

table(y.smTRAINSET)
table(y.smTESTSET)

minGCL<-randomForest(class~., data=sm.TRAINSET, 
                     ntree=bntr, mtry=sqrt(29), importance=TRUE)
minGCL.test<- predict(minGCL,x.smTESTSET)
minGCL
minGCLtest_conf<- table('TRUE'=y.smTESTSET, 'PREDICTION'=minGCL.test)
minGCLtest_conf
conf_percentage(minGCLtest_conf)
accuracy(minGCLtest_conf)
##################################################################
#Question 10
clusters_size
Ultimate_mat_ordered
CLU_index <- out$cluster == which(clusters_size == Ultimate_mat_ordered$Size[8]) 
#size was changed for each cluster in above line
CLU_cases <- SDATA[CLU_index,]
CLUGini_cases <- cbind(CLU_cases,'class'=SDATA_OB[CLU_index,]$class) 
table(CLUGini_cases$class)
SepRF<-randomForest(class~., data=CLUGini_cases, 
                    ntree=bntr, mtry=sqrt(29), importance=TRUE)
SepRF.test<- predict(SepRF,x.TESTSET)
SepRF
SepRFtest_conf<- table('TRUE'=y.TESTSET, 'PREDICTION'=SepRF.test)
SepRFtest_conf
conf_percentage(SepRFtest_conf)
accuracy(SepRFtest_conf)


#################################################################################
# Question 11- SVM
# Use SDATA_CL1 & SDATACL2

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





