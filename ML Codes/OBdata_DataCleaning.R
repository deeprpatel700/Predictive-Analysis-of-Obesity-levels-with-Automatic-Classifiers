# Name: Deep Patel

setwd("C:\\Users\\deepp\\Google Drive")
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
