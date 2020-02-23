#########################################
# File Name:-Assignment14.R
# Subject:- Random Forest
# Author:- Debadutta Dey
#########################################
install.packages("randomForest")
library(randomForest)

library(XML)
library(kselection)
library(dummies)
library(C50)
library(caTools)
library(doParallel)
library(caret)
library(gmodels)

###################################################################################################
# Company_Data.csv
###################################################################################################
##Loading .csv into R
## File name "Company_Data.csv"
companyData <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(companyData))

str(companyData)

View(companyData)

summary(companyData)

companyData1<-companyData
str(companyData1)

companyData4<-companyData1
companyData4$Advertising<-as.factor(companyData4$Advertising)
companyData4$ShelveLoc<-as.factor(companyData4$ShelveLoc)

companyData4$Education<-as.factor(companyData4$Education)
companyData4$Urban <-as.factor(companyData4$Urban)
companyData4$US<-as.factor(companyData4$US)

str(companyData4)

#companyData4<-companyData4[,-7]
companyData4$ShelveLoc<-as.factor(companyData4$ShelveLoc)

companyData4$Advertising<-as.factor(companyData4$Advertising)
companyData4$Education<-as.factor(companyData4$Education)
companyData4$Urban <-as.factor(companyData4$Urban)
companyData4$US<-as.factor(companyData4$US)
##############################################################################################################

companyData4$SalesB<-0
sqldf("select count(1) from companyData where sales<=5.390")
sqldf("select count(1) from companyData where sales between 5.3901 and 9.320")
sqldf("select count(1) from companyData where sales  >9.3201")
companyData4<-sqldf(c("update companyData4 set SalesB=1 where Sales<=5","select * from companyData4"))
companyData4<-sqldf(c("update companyData4 set SalesB=2 where Sales between 5 and 10","select * from companyData4"))
companyData4<-sqldf(c("update companyData4 set SalesB=3 where Sales >10","select * from companyData4"))

sqldf("select * from companyData4 where SalesB=0")
companyData4$SalesB<-as.factor(companyData4$SalesB)

companyData4<-companyData4[,-1]

companyData4_low<-companyData4[companyData4$SalesB=="1",] # 77
companyData4_medium<-companyData4[companyData4$SalesB=="2",] # 245
companyData4_high<-companyData4[companyData4$SalesB=="3",] # 78


sqldf("select count(1), SalesB from companyData4 group by SalesB ")
sqldf("select count(1), SalesB from companyData4_low group by SalesB ")
sqldf("select count(1), SalesB from companyData4_medium group by SalesB ")
sqldf("select count(1), SalesB from companyData4_high group by SalesB ")




set.seed(132)
samp<-sample.split(companyData4$SalesB,SplitRatio = 0.8)
compTrain<-companyData4[samp,]
compTest<-companyData4[-samp,]

sqldf("select count(1),SalesB from companyData4 group by SalesB")
sqldf("select count(1),SalesB from compTrain group by SalesB")
sqldf("select count(1),SalesB from compTest group by SalesB")
#######################################################################################################################

set.seed(123)
i=63
j=214
k=64
companyData4Train <- rbind(companyData4_low[1:i,],companyData4_medium[1:j,],companyData4_high[1:j,])
companyData4Test <- rbind(companyData4_low[i+1:77,],companyData4_medium[j+1:245,],companyData4_high[j+1:78,])
#summary(companyData4Test)

companyData4Test <-sqldf(c("delete from companyData4Test where SalesB is null","select * from companyData4Test"))
#str(companyData4Test)
#sqldf("select distinct SalesB from companyData4Test")


##################################################################################
## building model
#################################################################################
set.seed(423)
mod1<-randomForest(SalesB ~.,data=companyData4Train, na.action=na.roughfix,importance=TRUE,proximity= TRUE)
print(mod1)

mean(companyData4Train$SalesB==predict(mod1,companyData4Train),na.rm = TRUE) 

predtrain <- predict(mod1,companyData4Train)


CrossTable(companyData4Train$SalesB,predtrain)

predtest <- predict(mod1,newdata=companyData4Test)
mean(predtest==companyData4Test$SalesB)

CrossTable(companyData4Test$SalesB,predtest)


###################################################################################################
# Fraud_check.csv
###################################################################################################
##Loading .csv into R
## File name "Fraud_check.csv"
fraudCheckData <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""),col.names =c("Undergrad","MaritalStatus","TaxableIncome","CityPopulation","WorkExperience","Urban"))

sum(is.na(fraudCheckData))

str(fraudCheckData)

View(fraudCheckData)

summary(fraudCheckData)

sqldf("select distinct TaxableIncome from fraudCheckData where TaxableIncome <=30000")

fraudCheckData$IncomeB<-1

fraudCheckData1<-sqldf(c("update fraudCheckData set IncomeB=case when TaxableIncome <=30000 then 1 else 0 end","select * from fraudCheckData"))

fraudCheckData1$IncomeB<-as.factor(fraudCheckData1$IncomeB)
fraudCheckData1<-sqldf(c("update fraudCheckData1 set IncomeB='Risky' where IncomeB='1'","select * from fraudCheckData1"))
fraudCheckData1<-sqldf(c("update fraudCheckData1 set IncomeB='Good' where IncomeB='0'","select * from fraudCheckData1"))
fraudCheckData1$IncomeB<-as.factor(fraudCheckData1$IncomeB)

str(fraudCheckData1)
fraudCheckData2<-fraudCheckData1[,-3]
str(fraudCheckData2)

#######################################################################################################
## Implementing Stratified sampling
#######################################################################################################

fraudCheckData2_Risky<-fraudCheckData2[fraudCheckData2$IncomeB=="Risky",] # 124
fraudCheckData2_Good<-fraudCheckData2[fraudCheckData2$IncomeB=="Good",] # 476

sqldf("select count(1) from fraudCheckData2 where IncomeB='Good'")


sqldf("select count(1), IncomeB from fraudCheckData2 group by IncomeB ")

set.seed(135)
i=118
j=118
k=i+1
l=j+1

fraudCheckData2Train <- rbind(fraudCheckData2_Risky[1:i,],fraudCheckData2_Good[1:j,],row.names= TRUE)
#Print(k)
#Print(l)
fraudCheckData2Test <- as.data.frame(rbind(fraudCheckData2_Risky[k:124,],fraudCheckData2_Good[l:476,],row.names= TRUE),na.rm=TRUE)
#str(fraudCheckData2Test)
#summary(fraudCheckData2Test)
#sqldf("select * from fraudCheckData2Test where IncomeB is null")


##################################################################################
## building model
#################################################################################
set.seed(455)

mod2<-randomForest(IncomeB ~.,data=fraudCheckData2Train, na.action=na.roughfix,importance=TRUE,proximity= TRUE)
print(mod2)

##Although OOB error rate is 55.7%. Also balanced good and risky.

mean(fraudCheckData2Train$IncomeB==predict(mod2,fraudCheckData2Train),na.rm = TRUE) 

predtrain <- predict(mod2,fraudCheckData2Train)


CrossTable(fraudCheckData2Train$IncomeB,predtrain)

predtest <- predict(mod2,newdata=fraudCheckData2Test)
mean(predtest==fraudCheckData2Test$IncomeB,na.rm = TRUE)

CrossTable(fraudCheckData2Test$IncomeB,predtest)


###################################################################################################
# iris Dataset
###################################################################################################
?iris
data("iris")
Iris<-iris

str(Iris)

sqldf("select * from Iris")

set.seed(967)
samp = sample.split(Iris$Species, SplitRatio=0.8)
train = Iris[ samp,]
test  = Iris[!samp,]


set.seed(9670)
samp<-sample(2,nrow(Iris),replace = TRUE,prob = c(0.8,0.2))
IrisTrain<-Iris[samp==1,]
IrisTest<-Iris[samp==2,]

##################################################################################
## building model
#################################################################################
set.seed(455)

mod2<-randomForest(Species ~.,data=train, na.action=na.roughfix,importance=TRUE,proximity= TRUE)
print(mod2)


mean(train$Species==predict(mod2,train),na.rm = TRUE) 

predtrain <- predict(mod2,train)


CrossTable(train$Species,predtrain)

predtest <- predict(mod2,newdata=test)
mean(predtest==test$Species,na.rm = TRUE)

CrossTable(test$Species,predtest)






