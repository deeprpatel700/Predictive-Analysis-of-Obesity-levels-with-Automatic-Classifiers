'''
Name: Deep Patel 
Project: Factors affecting Obesity and Classification of Obesity levels
Project Section: K-means 

Subsections:
1. Function to collect data for various k-values using k-means algorithm
2. Different plots to decide optimal k clusters
3. Visualization of Centers for best k
4. Summary table for best k clusters

'''
setwd("C:\\Users\\deepp\\Google Drive")
getwd()
SDATA<- read.csv('Obesity_SDATA.csv') #reading the file

#Testing broad range of k-values to determine best k-value from elbow method and dispersion rate
k_values = c(seq(1,9,3),seq(10,48,4),seq(50,100,10))  
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
end_time <- Sys.time() # End timer
kmeans_time = end_time - start_time
print(kmeans_time)

par(mar=c(5.1,4.1,4.1,3.1))
par(mfrow=c(2,2))

# Plot for Elbow method to determine optimal k clusters
plot(k_values,Qm,xlab = 'k Clusters', ylab = 'Q(m)dasdasd',
     main = 'Elbow Method for Clustering Quality',col='red', type='l',
     xaxt = 'n')
axis(1,at = seq(1, 1000, by = 2), las=2)

# Plot for Clustering performance to determine optimal k clusters
plot(k_values,Perf.m,xlab = 'k Clusters', ylab = 'Perf(k)',
     main = 'Clustering Performance vs Number of Clusters',col='red', type='l',
     xaxt = 'n')
axis(1,at = seq(1, 1000, by = 2), las=2)

# Plot for GINI full impurity for each k run to determine optimal k clusters
plot(k_values,GINI_indexes_full,xlab = 'k Clusters', ylab = 'GINI(k) full',
     main = 'GINI Full vs Number of Clusters',col='red', type='l',
     xaxt='n')
axis(1,at = seq(1, 1000, by = 2), las=2)

# Plot for GINI full impurity percentage for each k run to determine optimal k clusters
plot(k_values,GINI_indexes_per,xlab = 'k Clusters', ylab = 'GINI(k) in %',
     main = 'GINI % vs Number of Clusters',col='red', type='l',
     xaxt='n')
axis(1,at = seq(1, 1000, by = 2), las=2)
par(mfrow=c(1,1))




#---------------------------------------------------------------------------------
# Visualization of Centers for best k clusters
#From elbow method, the best k=17
k = 17
out <- kmeans(SDATA, k, nstart=50, iter.max=5000000)
centers <-  out$centers

CORR_center <- cor(centers)  #Correlation Matrix of centers
Z_c = eigen(CORR_center)     #eigenvalues of centers
W_c <- Z_c$vectors           #eigenvectors of centers

vectors_3D <- t(t(W_c[,1:3]) %*% t(centers))
View(round(t(vectors_3D),2))
colnames(vectors_3D) <- c('V1','V2','V3')
rownames(vectors_3D) <- c('CEN1','CEN2','CEN3','CEN4','CEN5','CEN6','CEN7','CEN8','CEN9','CEN10',
                          'CEN11','CEN12','CEN13','CEN14','CEN15','CEN16','CEN17')
vectors_3D

# 3D plot of centroids
library(scatterplot3d)
colors = c('cyan', 'red', 'darkorange','black','blue','azure2', 'beige','bisque2','cornflowerblue','blue4',
           'cornsilk2','brown','brown1','darkgoldenrod','darkgoldenrod1','darkgoldenrod3','darkgreen','thistle1','deepskyblue1','darkmagenta')
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

#----------------------------------------------------------------------------------
# Re-run to create summary table 
k_values = 17 
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

# Matrix of frequencies
FREQ = matrix(nrow = 3, ncol = k_values, dimnames = list(c('C1','C2','C3'),
                                                         var1))
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

# Matrix of best k clusters, size of each cluster, dispersion, 
# class frequencies within each cluster and top class in each cluster.
Ultimate_mat<- as.data.frame(prob_dist_matrix)
size_dat<- as.data.frame(clusters_size)
disp<- as.data.frame(out$withinss)
Ultimate_mat$Size<- size_dat$clusters_size
Ultimate_mat$`C1%`<- Ultimate_mat$`C1%`*100
Ultimate_mat$`C2%`<- Ultimate_mat$`C2%`*100
Ultimate_mat$`C3%`<- Ultimate_mat$`C3%`*100
Ultimate_mat$Dispersion<- round(disp$`out$withinss`,0)
Ultimate_mat$Topj<- FREQ[4,]
colnames(Ultimate_mat)
Ultimate_mat_ordered<- Ultimate_mat[,c(5,4,6,1,2,3,7)]
head(Ultimate_mat_ordered)
colnames(Ultimate_mat_ordered)[4]= 'C1% FREQ'
colnames(Ultimate_mat_ordered)[5]= 'C2% FREQ'
colnames(Ultimate_mat_ordered)[6]= 'C3% FREQ'
colnames(Ultimate_mat_ordered)[7]= 'Top Class'
View(round(Ultimate_mat_ordered,2)) #Displays the summary table

