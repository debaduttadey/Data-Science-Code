#########################################
# File Name:-Assignment12.R
# Subject:- Naive Bayesian
# Author:- Debadutta Dey
#########################################

install.packages("naivebayes")
install.packages("mlbench")
install.packages("caret")
install.packages("InformationValue")
install.packages("tm")
install.packages("SnowballC")
install.packages("gmodels")
install.packages("e1071")



library(XML)
library(naivebayes)
library(car)
library(mlbench)
library(caret)
library(caTools)
library(sqldf)
library(InformationValue)
library(tm)
library(SnowballC)
library(gmodels)
library(e1071)

###################################################################################################
# SalaryData_Train.csv
###################################################################################################
##Loading .csv into R
## File name "SalaryData_Train.csv"
salary <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(salary))

str(salary)

View(salary)

summary(salary)




normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


salary2<-salary
salary2$age<-scale(salary2$age)
salary2$educationno<-normalize(salary2$educationno)
salary2$capitalgain<-normalize(salary2$capitalgain)
salary2$capitalloss<-normalize(salary2$capitalloss)
salary2$hoursperweek<-normalize(salary2$hoursperweek)
salary2$Ca<-normalize(salary2$Ca)
salary2$K<-normalize(salary2$K)
salary2$Ba<-normalize(salary2$Ba)
salary2$Fe<-normalize(salary2$Fe)
summary(salary2)
str(salary)

#####################################################################################
## Modeling
#####################################################################################
for (i in 1:10){
set.seed(451)
Mod1<-naive_bayes(Salary ~ .,data = salary,laplace = i,usepoisson = TRUE)




p1<-predict(Mod1,salary)

Tab1<- table(p1,salary$Salary)
print(i)
print(Tab1)
print(sum(diag(Tab1))/sum(Tab1))
}



Mod2<-naive_bayes(Salary ~ .,data = salary,laplace = 1,usepoisson = TRUE)
p2<-predict(Mod2,salary)

Tab2<- table(p2,salary$Salary)
sum(diag(Tab2))/sum(Tab2)

salaryTest <- read.csv(file.choose(),header = TRUE,sep = ",")
str(salaryTest)
str(salary)

pred<-predict(Mod2,salaryTest)

Tab3<- table(pred,salaryTest$Salary)
sum(diag(Tab3))/sum(Tab3)

CrossTable(pred,salaryTest$Salary,prop.chisq=FALSE,prop.t=FALSE,
           dnn=c('Predicted','Actual'))

###################################################################################################
# sms_raw_NB.csv
###################################################################################################
##Loading .csv into R
## File name "sms_raw_NB.csv"
sms <- read.csv(file.choose(),header = TRUE,sep = ",",stringsAsFactors = FALSE)

sum(is.na(sms))

str(sms)

sms$type<-as.factor(sms$type)

View(sms)

sms$text

summary(sms)

sms_corp<-Corpus(VectorSource(sms$text))


Corp_clean<-tm_map(sms_corp,content_transformer(stripWhitespace))
Corp_clean<-tm_map(Corp_clean,content_transformer(tolower))
Corp_clean<-tm_map(Corp_clean,content_transformer(removeNumbers))
Corp_clean<-tm_map(Corp_clean,removeWords,stopwords("english"))
Corp_clean<-tm_map(Corp_clean,content_transformer(removePunctuation))
remNumPunc<- function(x) gsub("^[:alpha:][:space:]]*","",x)
Corp_clean<-tm_map(Corp_clean,content_transformer(remNumPunc))
class(Corp_clean)


vappl
Corp_clean$content[1:20]

sms_dtm<-DocumentTermMatrix(Corp_clean)





############################################################################################
##Stratified Random Sampling
#############################################################################################
set.seed(1234)
smsTrain<-sms[1:4169,]
smsTest<-sms[4170:5559,]

smsDtmTrain<-sms_dtm[1:4169,]
smsDtmTest<-sms_dtm[4170:5559,]



smsCorpTrain<-Corp_clean[1:4169]
smsCorpTest<-Corp_clean[4170:5559]

prop.table(table(smsTrain$type))
prop.table(table(smsTest$type))

smsDict<-findFreqTerms(smsDtmTrain,5)

sms_train<-DocumentTermMatrix(smsCorpTrain,list(directory=smsDict))
sms_test<-DocumentTermMatrix(smsCorpTest,list(directory=smsDict))

inspect(smsCorpTrain[1:100])

list(smsDict[1:100])

convtCnt<-function(x) {
  x<-ifelse(x>0, 1, 0)
  x<-factor(x,levels=c(0,1),labels=c("No","Yes"))
}
sms_train<-apply(sms_train,MARGIN = 2,convtCnt)
sms_test<-apply(sms_test,MARGIN = 2,convtCnt)

View(sms_train)



smsMod1<-naiveBayes(sms_train,smsTrain$type)

smsPred<-predict(smsMod1,sms_test)
smsPred[1:25]

smsTab<-table(smsPred,smsTest$type)


CrossTable(smsPred,smsTest$type,prop.chisq=FALSE,prop.t=FALSE,
           dnn=c('Predicted','Actual'))

