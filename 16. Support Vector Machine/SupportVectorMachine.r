#########################################
# File Name:-Assignment16.R
# Subject:- Support Vector Machine
# Author:- Debadutta Dey
#########################################

install.packages("kernlab")
install.packages("caTools")

library(XML)
library(kernlab)
library(caret)
library(sqldf)

###################################################################################################
# SalaryData_Train.csv
###################################################################################################
##Loading .csv into R
## File name "SalaryData_Train.csv"
SalaryData <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(SalaryData))

summary(SalaryData)

str(SalaryData)

#############################################################################
# Feature Engineering
##############################################################################
factor_vars <- c ("workclass", "education", "maritalstatus", "occupation", "relationship","race","sex","native")  

all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)  

for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=SalaryData[, factor_var], Y=SalaryData$Salary)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=SalaryData[, factor_var], Y=SalaryData$Salary), "howgood")
}

all_iv <- all_iv[order(-all_iv$IV), ] 

all_iv

SalaryData1<-SalaryData[,c(1,4,10,11,14)]




#############################################################################

mod1<-ksvm(Salary~., data=SalaryData,kernal="rbfdot")   #0.8541169   0.8543825
?ksvm

SalaryDataTest <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))



Pred <-predict(mod1,newdata = SalaryDataTest)

mean(Pred==SalaryDataTest$Salary)

table(Pred,SalaryDataTest$Salary)

## With Feature engineering

mod2<-ksvm(Salary~., data=SalaryData1,kernal="rbfplot")   
?ksvm


SalaryDataTest1<-SalaryData[,c(1,4,10,11,14)]

Pred <-predict(mod2,newdata = SalaryDataTest1)

mean(Pred==SalaryDataTest1$Salary)

table(Pred,SalaryDataTest1$Salary)

## mod1 is final as it has higher accuracy.

###################################################################################################
# forestfires.csv
###################################################################################################
##Loading .csv into R
## File name "forestfires.csv"
forestfires <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(forestfires))

summary(forestfires)

str(forestfires)

forestfires1<-forestfires

forestfires1$dayfri<-as.factor(forestfires1$dayfri)
forestfires1$daymon<-as.factor(forestfires1$daymon)
forestfires1$daysat<-as.factor(forestfires1$daysat)
forestfires1$daysun<-as.factor(forestfires1$daysun)
forestfires1$daythu<-as.factor(forestfires1$daythu)
forestfires1$daytue<-as.factor(forestfires1$daytue)
forestfires1$daywed<-as.factor(forestfires1$daywed)

forestfires1$monthapr<-as.factor(forestfires1$monthapr)
forestfires1$monthaug<-as.factor(forestfires1$monthaug)
forestfires1$monthdec<-as.factor(forestfires1$monthdec)
forestfires1$monthfeb<-as.factor(forestfires1$monthfeb)
forestfires1$monthjan<-as.factor(forestfires1$monthjan)
forestfires1$monthjul<-as.factor(forestfires1$monthjul)
forestfires1$monthjun<-as.factor(forestfires1$monthjun)
forestfires1$monthmar<-as.factor(forestfires1$monthmar)
forestfires1$monthmay<-as.factor(forestfires1$monthmay)
forestfires1$monthnov<-as.factor(forestfires1$monthnov)
forestfires1$monthoct<-as.factor(forestfires1$monthoct)
forestfires1$monthsep<-as.factor(forestfires1$monthsep)


str(forestfires1)
############################################################################
## Stratfied sampling
############################################################################

forestfires1_large<-forestfires1[forestfires1$size_category=="large",] # 139
forestfires1_small<-forestfires1[forestfires1$size_category=="small",] # 378

i=100
j=100

train <- rbind(forestfires1_large[1:i,],forestfires1_small[1:j,])
test <- rbind(forestfires1_large[i+1:139,],forestfires1_small[j+1:378,])
summary(test)

test<-sqldf("select * from test where size_category is not null")


sqldf("select count(1),size_category from forestfires1 group by size_category")
library(caTools)
train_rows = sample.split(forestfires1$size_category, SplitRatio=0.8)
train = forestfires1[train_rows,]
test  = forestfires1[-train_rows,]

#############################################################################
mod1<-ksvm(size_category~., data=train,kernal="vanilladot")   


Pred <-predict(mod1,newdata = test)

mean(Pred==test$size_category)

mod2<-ksvm(size_category~., data=train,kernal="rbfdot")   


Pred <-predict(mod2,newdata = test)

mean(Pred==test$size_category)

table(Pred,test$size_category)

mod3<-ksvm(size_category~., data=train,kernal="besseldot")   

Pred <-predict(mod3,newdata = test)

mean(Pred==test$size_category)

table(Pred,test$size_category)

mod4<-ksvm(size_category~., data=train,kernal="anovadot")   

Pred <-predict(mod4,newdata = test)

mean(Pred==test$size_category)

table(Pred,test$size_category)

mod5<-ksvm(size_category~., data=train,kernal="splinedot")   

Pred <-predict(mod5,newdata = test)

mean(Pred==test$size_category)

table(Pred,test$size_category)

mod6<-ksvm(size_category~., data=train,kernal="stringdot")   

Pred <-predict(mod6,newdata = test)

mean(Pred==test$size_category)

table(Pred,test$size_category)

## mod4 is final model
