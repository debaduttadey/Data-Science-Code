#########################################
# File Name:-Assignment15.R
# Subject:- Neural Network
# Author:- Debadutta Dey
#########################################
install.packages("neuralnet")
install.packages("clusterSim")

library(XML)
library(neuralnet)
library(nnet)
library(caTools)
library(sqldf)
library(clusterSim)
library(dummies)

###################################################################################################
# concrete.csv
###################################################################################################
##Loading .csv into R
## File name "concrete.csv"
concrete <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(concrete))

str(concrete)

View(concrete)

summary(concrete)

boxplot(concrete)

## Standardization of data
?data.Normalization
ConcreteNorm <-data.Normalization(concrete,type="n1",normalization = "column")
summary(ConcreteNorm)

## Applying Random Sampling
set.seed(890)
samp<- sample.split(ConcreteNorm,sample.split(0.80))

ConcreteTrain<-subset(ConcreteNorm,samp ==FALSE) 
ConcreteTest<-subset(ConcreteNorm,samp ==TRUE) 
## False has more values than true. so had to put false for train and true for test

#################################################################################
## Modeling
#################################################################################

set.seed(671)

mod1<-neuralnet(strength~.,data=ConcreteTrain,hidden = 4,linear.output = TRUE)
plot(mod1)

PredTest<-compute(mod1,ConcreteTest[1:8])

PredStrength<-PredTest$net.result
names(PredStrength)<-"ps"

summary(PredStrength)

cor(PredStrength,ConcreteTest$strength,use="pairwise.complete.obs")

plot(PredStrength, ConcreteTest$strength, pch = 19, col = rgb(1, 0, 0, 0.5),xlab="Predicted Strength",ylab="Actual Strength", main="Co-Relation Plot")


###################################################################################################
# forestfires.csv
###################################################################################################
##Loading .csv into R
## File name "forestfires.csv"
forestfires <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(forestfires))

str(forestfires)

View(forestfires)

summary(forestfires1)

str(forestfires)


forestfires1<- forestfires
forestfires1<-forestfires1[,-c(1,2)]

norma <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  
}

sqldf("select distinct size_Category from forestfires1")
forestfires1<-sqldf(c("update forestfires1 set size_Category=1 where size_Category='large'","select * from forestfires1"))
forestfires1<-sqldf(c("update forestfires1 set size_Category=0 where size_Category='small'","select * from forestfires1"))


forestfires1$FFMC<-norma(forestfires$FFMC)
forestfires1$DMC<-norma(forestfires$DMC)
forestfires1$DC<-norma(forestfires$DC)
forestfires1$ISI<-norma(forestfires$ISI)
forestfires1$temp<-norma(forestfires$temp)
forestfires1$RH<-norma(forestfires$RH)
forestfires1$wind<-norma(forestfires$wind)
forestfires1$rain<-norma(forestfires$rain)
forestfires1$area<-norma(forestfires$area)
summary(forestfires1)




forestfires1$size_category<-as.integer(forestfires1$size_category)
str(forestfires1)

## spliting into test and train
set.seed(890)
samp<- sample.split(forestfires1,sample.split(0.80))

forestfiresTrain<-subset(forestfires1,samp ==FALSE) 
forestfiresTest<-subset(forestfires1,samp ==TRUE) 


#################################################################################
## Modeling
#################################################################################


form2<-paste("area",paste(colnames(forestfiresTrain),collapse ="+"),sep="~")



set.seed(6711)

mod2<-neuralnet(formula = form2,data=forestfiresTrain,hidden = 1,linear.output = TRUE)
plot(mod2)

forestfiresTest2<-forestfiresTest[,-9]

PredTest<-compute(mod2,forestfiresTest)

PredArea<-PredTest$net.result


summary(PredArea)

cor(PredArea,forestfiresTest$area,use="pairwise.complete.obs")

plot(PredArea, forestfiresTest$area, pch = 19, col = rgb(1, 0, 0, 0.5),xlab="Predicted Strength",ylab="Actual Strength", main="Co-Relation Plot")

abline(-1.85,4.5)


######################################################################################################
## Feature Selection Method
#######################################################################################################


base.mod <- lm(area ~ 1 , data=forestfiresTrain)  


all.mod <- lm(area ~ . , data= forestfiresTrain) 


stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  


shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] 


print(shortlistedVars)
#######################################################################################################
forestfiresTrain2<-forestfiresTrain[,c(9,5,12,29)]
forestfiresTrain2$area<-as.integer(forestfiresTrain2$area)
forestfiresTrain2$temp<-as.integer(forestfiresTrain2$temp )


#################################################################################
## Modeling
#################################################################################


form<-paste("area",paste(colnames(forestfiresTrain2),collapse ="+"),sep="~")



set.seed(671)

mod1<-neuralnet(formula = form,data=forestfiresTrain2,hidden = 1,linear.output = TRUE)
plot(mod1)

forestfiresTest2<-forestfiresTest[,c(9,5,12,29)]

PredTest<-compute(mod1,forestfiresTest2)

PredArea<-PredTest$net.result


summary(PredArea)

cor(PredArea,forestfiresTest$area,use="pairwise.complete.obs")

plot(PredArea, forestfiresTest$area, pch = 19, col = rgb(1, 0, 0, 0.5),xlab="Predicted Strength",ylab="Actual Strength", main="Co-Relation Plot")

## Model (mod2) with feature engineering is selected as final model

###################################################################################################
# 50_Startups.csv
###################################################################################################
##Loading .csv into R
## File name "50_Startups.csv"
StartUp <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""),col.names = c("Research", "Admin","Marketing","State","Profit"))

sum(is.na(StartUp))

str(StartUp)

View(StartUp)

summary(StartUp)

str(StartUp)

boxplot(StartUp)

StartUp1<- dummy.data.frame(StartUp,sep="")
View(StartUp1)

str(StartUp1)

StartUp2<- StartUp1[,-4]

norma <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  
}
StartUp3<-StartUp2
StartUp3$Research<-norma(StartUp3$Research)
StartUp3$Admin<-norma(StartUp3$Admin)
StartUp3$Marketing<-norma(StartUp3$Marketing)
StartUp3$Profit <-norma(StartUp3$Profit )
str(StartUp3)
summary(StartUp3)

library(data.table)
setnames(StartUp3,"StateNew York","StateNewYork")

### Splitting data into test and train

set.seed(890)
samp<- sample.split(StartUp3,sample.split(0.80))

StartUpTrain<-subset(StartUp3,samp ==FALSE) 
StartUpTest<-subset(StartUp3,samp ==TRUE) 


#################################################################################
## Modeling
#################################################################################


form<-paste("Profit",paste(colnames(StartUpTrain),collapse ="+"),sep="~")



set.seed(671)

mod1<-neuralnet(formula = form,data=StartUpTrain,hidden = 1,linear.output = TRUE)
plot(mod1)



PredTest<-compute(mod1,StartUpTest)

PredArea<-PredTest$net.result


summary(PredArea)

cor(PredArea,StartUpTest$Profit,use="pairwise.complete.obs")

plot(PredArea, StartUpTest$Profit, pch = 19, col = rgb(1, 0, 0, 0.5),xlab="Predicted Strength",ylab="Actual Strength", main="Co-Relation Plot")

