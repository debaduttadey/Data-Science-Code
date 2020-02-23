#########################################
# File Name:-Assignment6.R
# Subject:- Logistic Regression
# Author:- Debadutta Dey
# Date:- 08/08/2019
#########################################

install.packages("dummies")
install.packages("irr")
install.packages("caret")
install.packages("ROCR")
install.packages("dplyr")
install.packages("InformationValue")
install.packages("pROC")
install.packages('AER')



# Importing required packages
library(XML)
library(moments)
library(sqldf)
library(InformationValue)
library(caret)
library(MASS)
library(dummies)

library(caTools)

library(irr)

library(ROCR)
library(dplyr)
library(pROC)
library(AER)



###################################################################################################
# bank-full.csv
###################################################################################################
##Loading .csv into R
## File name "bank-full.csv"
BankData <- read.csv(file.choose(),header = TRUE,sep = ";",na.strings = c("NA","Na","nA","na")) 

View(BankData)

?read.csv

str(BankData)

sum(is.na(BankData))

summary(BankData)

BankData1 <-BankData

#BankData1$day<-as.factor(BankData1$day)
#BankData1$campaign<-as.factor(BankData1$campaign)
#BankData1$pdays<-as.factor(BankData1$pdays)
#BankData1$previous<-as.factor(BankData1$previous)
str(BankData1)



BankData1<-sqldf(c("update bankdata1 set job='admin' where job='admin.'","select * from BankData1"))
BankData1$job<-as.factor(BankData1$job)
#BankData1$day<-as.factor(BankData1$day)
#BankData1$campaign<-as.factor(BankData1$campaign)
#BankData1$pdays<-as.factor(BankData1$pdays)
#BankData1$previous<-as.factor(BankData1$previous)

BankData1<-sqldf(c("update BankData1 set y=1 where y='yes'","select * from BankData1"))
BankData1<-sqldf(c("update BankData1 set y=0 where y='no'","select * from BankData1"))
str(BankData1)

BankData1$y<-as.factor(BankData1$y)

###########################################################################################
## Feature selection method
###########################################################################################

factor_vars <- c ("job", "marital", "education", "default", "housing", "loan", "contact", "month","poutcome")  # get all categorical variables

all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)  # init output dataframe

for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=BankData1[, factor_var], Y=BankData1$y)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=BankData1[, factor_var], Y=BankData1$y), "howgood")
}

all_iv <- all_iv[order(-all_iv$IV), ] 

all_iv

WOETable(X=BankData1[, 'poutcome'], Y=BankData1$y)
WOETable(X=BankData1[, 'month'], Y=BankData1$y)
WOETable(X=BankData1[, 'contact'], Y=BankData1$y)
WOETable(X=BankData1[, 'housing'], Y=BankData1$y)
WOETable(X=BankData1[, 'job'], Y=BankData1$y)
WOETable(X=BankData1[, 'loan'], Y=BankData1$y)
WOETable(X=BankData1[, 'education'], Y=BankData1$y)
WOETable(X=BankData1[, 'marital'], Y=BankData1$y)

##default is not contributing and hence deleting the feature
str(BankData1)
BankData2<-BankData1[,-5]

str(BankData2)

set.seed(1221)
mod1<-glm(y~.,data = BankData2, family = 'binomial')
summary(mod1)



 

str(BankData2)

## age,jobunemployed,jobunknown,maritalsingle,pdays,previous,poutcomeunknown are insignificant
BankData3<-BankData2[,-c(1,13,14)]


str(BankData3)
str(BankData)
BankData4<- dummy.data.frame(BankData3,sep="")
View(BankData4)

str(BankData4)

dd<-c("jobunemployed","jobunknown","maritalsingle","educationprimary","housingno","loanno","contactcellular","monthapr","poutcomeunknown","y0")

BankData5 <- BankData4[,!(names(BankData4) %in% dd )] 
str(BankData5)

BankData5<-data.frame(lapply(BankData5, as.factor))

BankData5$balance<-BankData$balance
BankData5$duration<-BankData$duration
BankData5$campaign<-BankData$campaign
BankData5$day<-BankData$day


set.seed(1234)   
sample <- createDataPartition(y = BankData5$y1, 
                              p = 0.8, list = F)
BankDataTrain <- BankData5[sample,] 
BankDataTest <- BankData5[-sample,]

mod2<-glm(y1~.,data = BankDataTrain, family = 'binomial')
summary(mod2)

mod3<-glm(y1~. -jobblue.collar -jobentrepreneur - jobmanagement -jobself.employed -jobservices -jobtechnician -maritaldivorced -educationunknown - poutcomefailure,data = BankDataTrain, family = 'binomial')
summary(mod3)

dd<-c("jobblue.collar","jobentrepreneur","jobmanagement","jobself.employed","jobservices","jobtechnician","maritaldivorced","educationunknown","poutcomefailure")

BankDataTrain <- BankDataTrain[,!(names(BankDataTrain) %in% dd )]  

mod4<-glm(y1~.  -maritalmarried,data = BankDataTrain, family = 'binomial')
summary(mod4)



step(mod4,direction = "both") 

mod4<-glm(formula = y1 ~ jobadmin + jobhousemaid + jobretired + jobstudent + 
                    maritalmarried + educationsecondary + educationtertiary + 
                    balance + housingyes + loanyes + contacttelephone + contactunknown + 
                    day + monthaug + monthdec + monthfeb + monthjan + monthjul + 
                    monthjun + monthmar + monthmay + monthnov + monthoct + monthsep + 
                    duration + campaign + poutcomeother + poutcomesuccess, family = "binomial", 
                  data = BankDataTrain)
summary(mod4)

dd<-c("jobblue.collar","jobentrepreneur","jobmanagement","jobself.employed","jobservices","jobtechnician","maritaldivorced","educationunknown","poutcomefailure")

BankDataTest <- BankDataTest[,!(names(BankDataTest) %in% dd )] 

prob <- predict(mod4,type="response",BankDataTest)


library(InformationValue)
optCutOff <- optimalCutoff(BankDataTest$y1, prob)[1]

## model accuracy
confusion <- table(prob>0.3999999,BankDataTest$y)

accuracy <-sum(diag(confusion))/sum(confusion) ##0.9028747

rpred <- prediction(prob,BankDataTest$y1)
rpref <- performance(rpred,'tpr','fpr')


plot(rpref,colorize=T,text.adj=c(-0.2,1.7))

auc <- performance(rpred,measure = "auc")

auc1<-auc@y.values[[1]]
auc1

par(pty="s")
roc(BankDataTest$y1,prob,plot = TRUE,legacy.axes=TRUE,percent = TRUE
    ,xlab = "False Positive Percentage",ylab = "True Positive Percentage",col="red",lwd=4,print.auc=TRUE,print.auc.y=40)


###################################################################################################
# affairs.csv
###################################################################################################
##Loading .csv into R
## File name "affairs.csv"
AffairData <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na")) 

sum(is.na(AffairData))

View(AffairData)

AffairData<-AffairData[,-1]

sqldf("select distinct affairs from AffairData")

AffairData1 <- sqldf(c("update AffairData set  affairs=1 where affairs>0","select * from AffairData"))

sqldf("select distinct occupation from AffairData1")

str(AffairData1)

?Affairs

AffairData1$affairs <- as.factor(AffairData1$affairs)
AffairData1$age <- as.factor(AffairData1$age)
AffairData1$yearsmarried <- as.factor(AffairData1$yearsmarried)
AffairData1$religiousness <- as.factor(AffairData1$religiousness)
AffairData1$education <- as.factor(AffairData1$education)
AffairData1$occupation <- as.factor(AffairData1$occupation)
AffairData1$rating <- as.factor(AffairData1$rating)

###########################################################################################
## Feature selection method
###########################################################################################

factor_vars <- c ("gender", "age", "yearsmarried", "children", "religiousness", "education", "occupation", "rating")  

all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)  # init output dataframe

for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=AffairData1[, factor_var], Y=AffairData1$affairs)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=AffairData1[, factor_var], Y=AffairData1$affairs), "howgood")
}

all_iv <- all_iv[order(-all_iv$IV), ] 

all_iv




WOETable(X=AffairData1[, 'rating'], Y=AffairData1$affairs)
WOETable(X=AffairData1[, 'yearsmarried'], Y=AffairData1$affairs)
WOETable(X=AffairData1[, 'age'], Y=AffairData1$affairs)
WOETable(X=AffairData1[, 'religiousness'], Y=AffairData1$affairs)
WOETable(X=AffairData1[, 'children'], Y=AffairData1$affairs)
WOETable(X=AffairData1[, 'education'], Y=AffairData1$affairs)
WOETable(X=AffairData1[, 'occupation'], Y=AffairData1$affairs)

## gender not predictive and hence removing it
str(AffairData1)

AffairData1<-AffairData1[,-2]

set.seed(1221)
mod1<-glm(affairs~.,data = AffairData1, family = 'binomial')
summary(mod1)

## yearsmarried,children,education will be removed
AffairData11<-AffairData1[,-c(3,4,6)]

AffairData2<- dummy.data.frame(AffairData11,sep="")
View(AffairData2)

str(AffairData2)

dd<-c("affairs0","age17.5","children_yes","religiousness1","occupation1","rating1")

AffairData2 <- AffairData2[,!(names(AffairData2) %in% dd )] 

AffairData3<-data.frame(lapply(AffairData2, as.factor))

View(AffairData3)

str(AffairData3)

set.seed(1432)
sample <- createDataPartition(y = AffairData3$affairs1, 
                              p = 0.791, list = F)
AffairDataTrain <- AffairData3[sample,] 
AffairDataTest <- AffairData3[-sample,] 

modAffairs1<-glm(affairs1~.,data = AffairDataTrain, family = 'binomial')
summary(modAffairs1)

modAffairs11<-glm(affairs1~. -age47  -occupation2 -occupation3 -occupation4 -occupation5 -occupation6 -rating2 -rating3,data = AffairDataTrain, family = 'binomial')
summary(modAffairs11)

modAffairs12<-glm(affairs1~. -age47 -age32 -age42 -occupation7 -occupation2 -occupation3 -occupation4 -occupation5 -occupation6 -rating2 -rating3,data = AffairDataTrain, family = 'binomial')
summary(modAffairs12)

modAffairs13<-glm(affairs1~. -age27 -age37 -age52 -age57 -age47 -age32 -age42 -religiousness3 -occupation7 -occupation2 -occupation3 -occupation4 -occupation5 -occupation6 -rating2 -rating3,data = AffairDataTrain, family = 'binomial')
summary(modAffairs13)

dd<-c("age27","age37","age52","age57","age47","age32", "age42","religiousness3","occupation7","occupation2","occupation3","occupation4","occupation5","occupation6","rating2","rating3")

AffairDataTrain <- AffairDataTrain[,!(names(AffairDataTrain) %in% dd )] 

str(AffairDataTrain)

modAffairs2<-glm(affairs1~.,data = AffairDataTrain, family = 'binomial')
summary(modAffairs2)

step(modAffairs2,direction = "both")   

modAffairs3<-glm(formula = affairs1 ~ age22 + religiousness2 + religiousness4 + 
                  religiousness5 + rating4 + rating5, family = "binomial", 
                data = AffairDataTrain)

summary(modAffairs3)

dd<-c("age27","age37","age52","age57","age47","age32", "age42","religiousness3","occupation7","occupation2","occupation3","occupation4","occupation5","occupation6","rating2","rating3")

AffairDataTest <- AffairDataTest[,!(names(AffairDataTest) %in% dd )] 

str(AffairDataTest)

prob <- predict(modAffairs3,type="response",AffairDataTest)


library(InformationValue)
optCutOff <- optimalCutoff(AffairDataTest$affairs1, prob)[1]
optCutOff

## model accuracy
confusion <- table(prob>optCutOff,AffairDataTest$affairs1)
confusion

accuracy <-sum(diag(confusion))/sum(confusion) ##0.776
accuracy

rpred <- prediction(prob,AffairDataTest$affairs1)
rpref <- performance(rpred,'tpr','fpr')


plot(rpref,colorize=T,text.adj=c(-0.2,1.7))

auc <- performance(rpred,measure = "auc")

auc1<-auc@y.values[[1]]
auc1

par(pty="s")
roc(AffairDataTest$affairs1,prob,plot = TRUE,legacy.axes=TRUE,percent = TRUE
    ,xlab = "False Positive Percentage",ylab = "True Positive Percentage",col="red",lwd=4,print.auc=TRUE,print.auc.y=40)





