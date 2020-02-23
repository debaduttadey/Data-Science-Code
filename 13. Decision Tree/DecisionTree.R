#########################################
# File Name:-Assignment13.R
# Subject:- Decision Tree
# Author:- Debadutta Dey
#########################################

install.packages("kselection")
install.packages("dummies")
install.packages("C50") 
install.packages("doParallel")

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

norma <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

companyData1<-sqldf("select * from companyData1 order by Sales")

###########################################################################################################
companyData2<-companyData1


sqldf("select distinct Advertising from companyData2 order by Advertising")
#companyData2$Education <- as.factor(companyData2$Education)
#companyData2$Advertising <- as.factor(companyData2$Advertising)

str(companyData2)
#companyData2$Urban <- as.factor(companyData2$Urban)
#companyData2$US <- as.factor(companyData2$US)

companyData2$Sales<-norma(companyData2$Sales)
companyData2$CompPrice<-norma(companyData2$CompPrice)
companyData2$Population<-norma(companyData2$Population)

companyData2$Income<-norma(companyData2$Income)
companyData2$Price<-norma(companyData2$Price)
companyData2$Age<-norma(companyData2$Age)

summary(companyData2)


str(companyData2)

companyData2$Advertising<-as.factor(companyData2$Advertising)
companyData2$ShelveLoc<-as.factor(companyData2$ShelveLoc)

companyData2$Education<-as.factor(companyData2$Education)
companyData2$Urban <-as.factor(companyData2$Urban)
companyData2$US<-as.factor(companyData2$US)

###########################################################################################################
## Using clusters divided the sales into 4 categories
###########################################################################################################
###################################################################
## kmetorid Clustering
####################################################################

set.seed(1447)
companyData3<-companyData2
library(Rtsne)
library(cluster) 
#install.packages("Rtsne")
#install.packages("cluster")

dist <- daisy(companyData3, metric = "gower")
mat <- as.matrix(dist)


sil_width <- c(NA)
for(i in 2:11){  
  pam_fit <- pam(dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:11, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:11, sil_width)

sil_width

k <- 2
pam_fit <- pam(dist, diss = TRUE, k)
pam_results <- companyData3 %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

companyData4<-cbind(companyData3,pam_fit$clustering)
aggHist<-aggregate(companyData4,list(companyData4$pam),mean)

sqldf("select * from aggHist order by sales asc")




colnames(companyData4)[which(names(companyData4) == "pam_fit$clustering")] <- "SalesB"
str(companyData4)
companyData4$SalesB<-as.factor(companyData4$SalesB)

companyData4<-companyData4[,-1]

#############################################################################################################
## Stratified Sampling
############################################################################################################

companyData4_low<-companyData4[companyData4$SalesB=="2",] # 235
companyData4_high<-companyData4[companyData4$SalesB=="1",] # 165


sqldf("select count(1), SalesB from companyData4 group by SalesB ")



set.seed(132)


i=83
j=83
companyData4Train <- rbind(companyData4_low[1:i,],companyData4_high[1:j,])
companyData4Test <- rbind(companyData4_low[i+1:235,],companyData4_high[j+1:165,])
summary(companyData4Test)



##################################################################################
## building model
#################################################################################
set.seed(453)
mod1 <- C5.0(companyData4Train[,-11],companyData4Train$SalesB)
plot(mod1)
predTrain <- predict(mod1,companyData4Train[,-11])

mean(companyData4Train$SalesB==predTrain)

CrossTable(companyData4Train$SalesB,predTrain)


predTest<-Predict(mod1,companyData4Test[,-12])
mean(companyData4Test$SalesB==predTest,na.rm = TRUE)
CrossTable(companyData4Test$SalesB,predTest)

###################################################################################################
## Implementing decision tree without clustering
###################################################################################################
summary(companyData1)
str(companyData4)

companyData4<-companyData1
companyData4$Advertising<-as.factor(companyData4$Advertising)
companyData4$ShelveLoc<-as.factor(companyData4$ShelveLoc)

companyData4$Education<-as.factor(companyData4$Education)
companyData4$Urban <-as.factor(companyData4$Urban)
companyData4$US<-as.factor(companyData4$US)

companyData4<-sqldf(c("update companyData4 set Urban=1 where Urban ='Yes'","select * from companyData4"))
companyData4<-sqldf(c("update companyData4 set Urban=0 where Urban ='No'","select * from companyData4"))

companyData4<-sqldf(c("update companyData4 set US=1 where US ='Yes'","select * from companyData4"))
companyData4<-sqldf(c("update companyData4 set US=0 where US ='No'","select * from companyData4"))

sqldf("select distinct Advertising from companyData4 order by Advertising")
s<- dummy(companyData4$ShelveLoc,sep="")
s<-as.data.frame(s)
s$ShelveLocGood



companyData4$ShelveLocGood<-s$ShelveLocGood
companyData4$ShelveLocMedium<-s$ShelveLocMedium

str(companyData4)

#companyData4<-companyData4[,-7]
companyData4$ShelveLoc<-as.factor(companyData4$ShelveLoc)

companyData4$Advertising<-as.factor(companyData4$Advertising)
companyData4$ShelveLocGood<-as.factor(companyData4$ShelveLocGood)
companyData4$ShelveLocMedium<-as.factor(companyData4$ShelveLocMedium)
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


i=60
j=70
k=60
companyData4Train <- rbind(companyData4_low[1:i,],companyData4_medium[1:j,],companyData4_high[1:j,])
companyData4Test <- rbind(companyData4_low[i+1:100,],companyData4_medium[j+1:201,],companyData4_high[j+1:99,])
summary(companyData4Test)

companyData4Test <-sqldf(c("delete from companyData4Test where SalesB is null","select * from companyData4Test"))
str(companyData4Test)
sqldf("select distinct SalesB from companyData4Test")

##################################################################################
## building model
#################################################################################
set.seed(453)
mod1 <- C5.0(companyData4Train[,-11],companyData4Train$SalesB)
plot(mod1)
predTrain <- predict(mod1,companyData4Train[,-11])

mean(companyData4Train$SalesB==predTrain)

CrossTable(companyData4Train$SalesB,predTrain)


predTest<-Predict(mod1,companyData4Test[,-11])
mean(companyData4Test$SalesB==predTest,na.rm = TRUE)
CrossTable(companyData4Test$SalesB,predTest)

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
i=100
j=100
k=i+1
l=j+1

fraudCheckData2Train <- rbind(fraudCheckData2_Risky[1:i,],fraudCheckData2_Good[1:j,],row.names= TRUE)
#Print(k)
#Print(l)
fraudCheckData2Test <- as.data.frame(rbind(fraudCheckData2_Risky[k:124,],fraudCheckData2_Good[k:476,],row.names= TRUE),na.rm=TRUE)
#str(fraudCheckData2Test)
#summary(fraudCheckData2Test)
#sqldf("select * from fraudCheckData2Test where IncomeB is null")


##################################################################################
## building model
#################################################################################
set.seed(455)
mod1 <- C5.0(fraudCheckData2Train[,-6],fraudCheckData2Train$IncomeB)
plot(mod1)
predTrain <- predict(mod1,fraudCheckData2Train[,-6])

mean(fraudCheckData2Train$IncomeB==predTrain,na.rm = TRUE)

CrossTable(predTrain,fraudCheckData2Train$IncomeB)


predTest<-Predict(mod1,fraudCheckData2Test[,-6])
mean(fraudCheckData2Test$IncomeB==predTest,na.rm = TRUE)
CrossTable(fraudCheckData2Test$IncomeB,predTest)

###################################################################################################
# iris Dataset
###################################################################################################
?iris
data("iris")
Iris<-iris

str(Iris)

sqldf("select * from Iris")

install.packages("party")
library(party)
?ctree

set.seed(9670)
samp<-sample(2,nrow(Iris),replace = TRUE,prob = c(0.8,0.2))
IrisTrain<-Iris[samp==1,]
IrisTest<-Iris[samp==2,]



ITree<-ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = IrisTrain)
plot(ITree)
IPredTrain<-predict(ITree, IrisTrain, type="response")

table(IPredTrain,IrisTrain$Species)

mean(IPredTrain == IrisTrain$Species) * 100

IPredTest <- predict(ITree, newdata= IrisTest,type="response")

table(IPredTest,IrisTest$Species)

mean(IPredTest == IrisTest$Species) * 100


