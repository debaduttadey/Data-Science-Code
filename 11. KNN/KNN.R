#########################################
# File Name:-Assignment11.R
# Subject:- KNN
# Author:- Debadutta Dey
#########################################


install.packages("RRF")
install.packages("splitstackshape")
install.packages("sampling")
install.packages("glmnet")
install.packages("gmodels")

library(XML)
library(splitstackshape)
library(sampling)
library(RRF)
library(caret)
library(class)
library(gmodels)
library(sqldf)


###################################################################################################
# glass.csv
###################################################################################################
##Loading .csv into R
## File name "glass.csv"
Glass <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(Glass))

str(Glass)

View(Glass)

Glass$Type<-as.factor(Glass$Type)
glass1<-Glass
str(glass1)

sqldf("select distinct Type from glass1")
sqldf("select count(*),Type from glass1 group by Type")
summary(glass1)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


glass2<-glass1
glass2$RI<-normalize(glass2$RI)
glass2$Na<-normalize(glass2$Na)
glass2$Mg<-normalize(glass2$Mg)
glass2$Al<-normalize(glass2$Al)
glass2$Si<-normalize(glass2$Si)
glass2$Ca<-normalize(glass2$Ca)
glass2$K<-normalize(glass2$K)
glass2$Ba<-normalize(glass2$Ba)
glass2$Fe<-normalize(glass2$Fe)
summary(glass2)
str(glass2)

boxplot(glass2[,-10],horizontal = TRUE)





############################################################################################
##Stratified Random Sampling
#############################################################################################
set.seed(1234)
glassTrain<-as.data.frame(stratified (glass1, "Type", 0.8,keep.rownames = TRUE))
glassTest<-glass1[ !(rownames(glass1) %in% glassTrain$rn), ] 
View(glassTrain)

glassTrain<-glassTrain[,-1]



sqldf("select distinct Type from glassTest")

table(glass1$Type)/nrow(glass1)

table(glassTrain$Type)/nrow(glassTrain)

table(glassTest$Type)/nrow(glassTest)



###########################################################################################
## Feature Engineering
###########################################################################################
library(caret)
set.seed(100)
rPartMod <- train(Type ~ ., data=glassTrain, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

## finding redunant feature

correlationMatrix <- cor(glassTrain[,1:9])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

glasstrain1<-glassTrain[,-c(7,1,3)]

correlationMatrix <- cor(glasstrain1[,1:6])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

##Feature selection
control <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10)

results <- rfe(glasstrain1[,1:6], glasstrain1[,7], sizes=c(1:6), rfeControl=control)

print(results)

# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
str(glasstrain1)

##Only 5 features are selected
glasstrain2<-glasstrain1[,c(1:5,7)]
str(glasstrain2)
str(glassTest)
glasstest1<-glassTest[,-c(1,3,7)]
str(glasstest1)

sum(is.na(glasstrain2))


######################################
##  Applying KNN model
#####################################
## Knn Model With feature engimeering
GlassControl<-trainControl(method="repeatedCV"
                           ,number = 10
                           ,repeats=3
)
set.seed(120)  
Mod1 <-train(Type ~ ., data = glasstrain2, method = "knn", trControl = GlassControl,tuneGrid = expand.grid(k=1:60), tuneLength = 20)
Mod1
?train

plot(Mod1)
varImp(Mod1)

pred<- predict(Mod1, newdata=glasstest1)
confusionMatrix(pred,glasstest1$Type)

## Knn Model Without feature engimeering


set.seed(122)  
Mod2 <-train(Type ~ ., data = glassTrain, method = "knn", trControl = GlassControl,tuneGrid = expand.grid(k=1:60), tuneLength = 20)
Mod2
?train

plot(Mod2)
varImp(Mod2)

pred<- predict(Mod2, newdata=glassTest)
confusionMatrix(pred,glassTest$Type)

## Accuracy of mod2 is higher than mod1 . Hence Mod2 is final model

library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


###################################################################################################
# Zoo.csv
###################################################################################################
##Loading .csv into R
## File name "Zoo.csv"
zoo <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na",""))

sum(is.na(zoo))

str(zoo)

View(zoo)

## Animal.name is unique and hence need to remove it
zoo1<-zoo[,-1]
str(zoo1)

sqldf("select distinct hair from zoo1")
sqldf("select distinct feathers from zoo1")
sqldf("select distinct eggs from zoo1")
sqldf("select distinct milk from zoo1")
sqldf("select distinct airborne from zoo1")
sqldf("select distinct aquatic from zoo1")
sqldf("select distinct predator from zoo1")
sqldf("select distinct toothed from zoo1")
sqldf("select distinct backbone from zoo1")
sqldf("select distinct breathes from zoo1")
sqldf("select distinct venomous from zoo1")
sqldf("select distinct fins from zoo1")
sqldf("select distinct legs from zoo1")
sqldf("select distinct tail from zoo1")
sqldf("select distinct domestic from zoo1")
sqldf("select distinct catsize from zoo1")
sqldf("select distinct type from zoo1")

zoo2<-as.data.frame(lapply(zoo1,as.factor))
str(zoo2)

############################################################################################
##Stratified Random Sampling
#############################################################################################
set.seed(1234)
zooTrain<-as.data.frame(stratified (zoo2, "type", 0.8,keep.rownames = TRUE))
zooTest<-zoo2[ !(rownames(zoo2) %in% zooTrain$rn), ] 
View(zooTrain)
?stratified

zooTrain<-zooTrain[,-1]

sqldf("select count(*) from zoo2")
sqldf("select count(*) from zooTrain")
sqldf("select count(*) from zooTest")

######################################
##  Applying KNN model
#####################################

zooControl<-trainControl(method="repeatedCV"
                           ,number = 10
                           ,repeats=3
                           #,classProbs = TRUE
)
?trainControl
set.seed(120)  
Mod1 <-train(type ~ ., data = zooTrain, method = "knn", trControl = zooControl,tuneGrid = expand.grid(k=3:60), tuneLength = 20)
Mod1
?train

plot(Mod1)
varImp(Mod1)

pred<- predict(Mod1, newdata=zooTest)
confusionMatrix(pred,zooTest$type)


