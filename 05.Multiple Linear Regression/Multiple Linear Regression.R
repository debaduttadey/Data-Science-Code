#########################################
# File Name:-Assignment5.R
# Subject Multiple Linear Regression
# Author:- Debadutta Dey
# Date:- 23/06/2019
#########################################

#Downloading and Installing requird packages
install.packages("XML")
install.packages("moments")
install.packages("sqldf")
install.packages("GGally")
install.packages("stringi")
install.packages("corpcor")
install.packages("ggcorrplot")
install.packages("car")
install.packages("pracma")

# Importing required packages
library(XML)
library(moments)
library(sqldf)
library(GGally)
library(corpcor)
library(ggcorrplot)
library(car)
library(ggplot2)
library(pracma)
###################################################################################################
# 50_StartUpTrains.csv
###################################################################################################
##Loading .csv into R
## File name "50_Startups.csv"
StartUp <- read.csv(file.choose(),header=TRUE,col.names = c("Research", "Admin","Marketing","State","Profit")) 

StartUp<-as.data.frame(StartUp)
#attach(StartUp)

## Viewing the data
View(StartUp)

summary(StartUp)

sqldf("select * from StartUp")

StartUp <- sqldf(c("alter table StartUp add column NewYork bit","alter table StartUp add column California bit","alter table StartUp add column Florida bit","select * from StartUp"))
View(StartUp)


## State is discrete data and hence need to do One Hot encoding
StartUp <- sqldf(c("Update StartUp set NewYork = case when state='New York' then 1 else 0 end"
                   ,"Update StartUp set California = case when state='California' then 1 else 0 end"
                   ,"Update StartUp set Florida = case when state='Florida' then 1 else 0 end"
                   ,"select * from StartUp"), method = "raw")
StartUp <- sqldf("select Research, Admin, Marketing, NewYork, California, Florida, Profit
                 from StartUp")
## Checking the summary. Mean is different from median for weight_gained column but Calorie_consumed has approximately equal for mean and median values

str(StartUp)

## Checking For NA values
sum(is.na(StartUp))

## Checking for the summary

summary(StartUp)
str(StartUp)

boxplot(StartUp$Research, Col="blue", horizontal = T)
boxplot(StartUp$Admin, Col="blue", horizontal = T)
boxplot(StartUp$Marketing, Col="blue", horizontal = T)
boxplot(StartUp$Profit, Col="blue", horizontal = T)

## Updating 0 with mean as there is no outliers

StartUp<- sqldf(c("update StartUp set Research=73722 where Research=0"
                  ,"update StartUp set Marketing=211025 where Marketing=0"
                  ,"select * from StartUp")
)



ggpairs(StartUp)


ggcorrplot(cor(StartUp),hc.order = TRUE,type="lower",lab=TRUE)

## Marketing and research have collinearity. Admin and research. California and research has collinearity
purecorr <- as.data.frame(cor2pcor(cor(StartUp)),row.names = c("Research","Admin","Marketing","NewYork","California","Florida","Profit"))

names(purecorr) <- c("Research","Admin","Marketing","NewYork","California","Florida","Profit")


ggcorrplot(purecorr,type="lower",lab=TRUE)



## Creating first Model
model_StartUp_All <- lm(StartUp$Profit ~ ., data = StartUp)
summary(model_StartUp_All)

## Admin, New York, California, Florida are insignificant

model_StartUp_Admin <- lm(StartUp$Profit ~ StartUp$Admin, data = StartUp)
summary(model_StartUp_Admin)



model_StartUp_NewYork <- lm(StartUp$Profit ~ StartUp$NewYork, data = StartUp)
summary(model_StartUp_NewYork)

model_StartUp_California <- lm(StartUp$Profit ~ StartUp$California, data = StartUp)
summary(model_StartUp_California)

model_StartUpTrain_Florida <- lm(StartUp$Profit ~ StartUp$Florida, data = StartUp)
summary(model_StartUpTrain_Florida)

##All states are not contribputing to profit and hence deleting all the city 
summary(StartUp)

StartUp1 <-StartUp[,c(1,3,7)]  

## Creating first Model
model_StartUp1_All <- lm(StartUp$Profit ~ ., data = StartUp1)
summary(model_StartUp1_All)

set.seed(11)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(StartUp1,SplitRatio = 0.51)

StartUpTrain =subset(StartUp1,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
StartUpTest=subset(StartUp1, sample==FALSE)

StartUpTrain<- as.data.frame(StartUpTrain)
StartUpTest<- as.data.frame(StartUpTest)

View(StartUp1)

par(mfrow=c(3,3))
qqnorm(StartUpTrain$Research)
qqline(StartUpTrain$Research)

qqnorm((StartUpTrain$Research)^2)    #ssss
qqline((StartUpTrain$Research)^2)

qqnorm(sqrt(StartUpTrain$Research))
qqline(sqrt(StartUpTrain$Research))

qqnorm(log(StartUpTrain$Research))    
qqline(log(StartUpTrain$Research))

qqnorm(sqrt(StartUpTrain$Research)+log(StartUpTrain$Research)+(StartUpTrain$Research)^2)
qqline(sqrt(StartUpTrain$Research)+log(StartUpTrain$Research)+(StartUpTrain$Research)^2)

qqnorm(StartUpTrain$Marketing)
qqline(StartUpTrain$Marketing)

qqnorm(StartUpTrain$Profit)
qqline(StartUpTrain$Profit)



#####################################################
## Model building
######################################################

Model_Startup1_simple <-lm(Profit ~ Research+Marketing, data=StartUpTrain)
summary(Model_Startup1_simple)

Mod_Startup_Details <- data.frame('Model_Startup1_simple'
                                  ,'Profit'
                                  ,'Research+Marketing'
                                  ,cor(StartUpTrain1$Profit , StartUpTrain1$Research+StartUpTrain1$Marketing)
                                  ,summary(Model_Startup1_simple)$r.squared
                                  ,mean(Model_Startup1_simple$residuals)
                                  ,sqrt(mean((Model_Startup1_simple$residuals)^2)))
names(Mod_Startup_Details) <- c('ModelName','Y','X','r','R^2','ErrorSum','RMSE')
View(Mod_Startup_Details)

###########################
Model_Startup1_Research_SQMarketing <-lm(Profit ~ Research+(Marketing)^2, data=StartUpTrain)
summary(Model_Startup1_Research_SQMarketing)


InsertRow <- data.frame('Model_Startup1_Research_SQMarketing'
                        ,'Profit'
                        ,'Research+(Marketing)^2'
                        ,cor(StartUpTrain1$Profit , StartUpTrain1$Research)
                        ,summary(Model_Startup1_Research_SQMarketing)$r.squared
                        ,mean(Model_Startup1_Research_SQMarketing$residuals)
                        ,sqrt(mean((Model_Startup1_Research_SQMarketing$residuals)^2))
                        ,row.names = "2")
names(InsertRow) <- names(Mod_Startup_Details)
Mod_Startup_Details <-rbind(Mod_Startup_Details,InsertRow)
View(Mod_Startup_Details)
###########################
Model_Startup1_Sqx <-lm(Profit ~ (Research)^2+Marketing, data=StartUpTrain1)
summary(Model_Startup1_Sqx)

InsertRow <- data.frame('Model_Startup1_Sqx'
                                   ,'Profit'
                                   ,'(Research)^2+Marketing'
                                   ,cor(StartUpTrain1$Profit , (StartUpTrain1$Research)^2+StartUpTrain1$Marketing)
                                   ,summary(Model_Startup1_Sqx)$r.squared
                                   ,mean(Model_Startup1_Sqx$residuals)
                                   ,sqrt(mean((Model_Startup1_Sqx$residuals)^2))
                        ,row.names = "3")
names(InsertRow) <- names(Mod_Startup_Details)
Mod_Startup_Details <-rbind(Mod_Startup_Details,InsertRow)
View(Mod_Startup_Details)


Model_Startup1_Sqrtx <-lm(Profit ~ sqrt(Research)+Marketing, data=StartUpTrain1)
summary(Model_Startup1_Sqrtx)

InsertRow <- data.frame('Model_Startup1_Sqrtx'
                        ,'Profit'
                        ,'sqrt(Research)+Marketing'
                        ,cor(StartUpTrain1$Profit , sqrt(StartUpTrain1$Research)+StartUpTrain1$Marketing)
                        ,summary(Model_Startup1_Sqrtx)$r.squared
                        ,mean(Model_Startup1_Sqrtx$residuals)
                        ,sqrt(mean((Model_Startup1_Sqrtx$residuals)^2))
                        ,row.names = "3")
names(InsertRow) <- names(Mod_Startup_Details)
Mod_Startup_Details <-rbind(Mod_Startup_Details,InsertRow)
View(Mod_Startup_Details)


## Model "Model_Startup1_Sqx" is final as it has higher r.

#############################################################################
## Predicting for test dataset
#############################################################################

pred_Final  <- predict(Model_Startup1_Sqx, StartUpTest[-3])

errors_Startup_Test<-StartUpTest$Profit - pred_Final

sqrt(mean((errors_Startup_Test)^2))

## Visualization

##Visualization
ggplot(data = StartUp, aes(x = (Research)^2+Marketing, y = Profit)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = StartUpTest, aes(x = (Research)^2+Marketing, y = pred_Final))

par(mfrow=c(1,1))
############################################################
## Predicting Sales of a Computer
###########################################################

##Loading .csv into R
## File name "02.Computer_Data.csv"
CompData <- read.csv(file.choose(),header=TRUE,na.strings=c("","NA","na","Na","nA")) 

CompData<-as.data.frame(CompData)
#attach(StartUp)

## Viewing the data
View(CompData)

summary(CompData)

sum(is.na(CompData))


##removing the first column as it contains id
CompData <- CompData[,-1]

str(CompData)


CompData <- sqldf(c("alter table CompData add column CDYes bit","alter table CompData add column CDNo bit"
                    ,"alter table CompData add column MultiYes bit","alter table CompData add column MultiNo bit"
                    ,"alter table CompData add column PremiumYes bit","alter table CompData add column PremiumNo bit","select * from CompData"))
View(CompData)

## State is discrete data and hence need to do One Hot encoding
CompData <- sqldf(c("Update CompData set CDYes = case when CD='yes' then 1 else 0 end"
                    ,"Update CompData set CDNo = case when CD='no' then 1 else 0 end"
                    ,"Update CompData set MultiYes = case when Multi='yes' then 1 else 0 end"
                    ,"Update CompData set MultiNo = case when Multi='no' then 1 else 0 end"
                    ,"Update CompData set PremiumYes = case when Premium='yes' then 1 else 0 end"
                    ,"Update CompData set PremiumNo = case when Premium='no' then 1 else 0 end"
                    ,"select * from CompData"), method = "raw")

CompData <- CompData[,c(-6,-7,-8)]

CompData <- CompData[,c(-9,-11,-13)]


str(CompData)

boxplot(CompData$price, Col="blue", horizontal = T)  ##outliers
boxplot(CompData$speed, Col="blue", horizontal = T)
boxplot(CompData$hd, Col="blue", horizontal = T)      ##outliers
boxplot(CompData$ram, Col="blue", horizontal = T)    ##outliers
boxplot(CompData$screen, Col="blue", horizontal = T) ##outliers
boxplot(CompData$ads, Col="blue", horizontal = T)   
boxplot(CompData$trend, Col="blue", horizontal = T)

boxplot(CompData$screen, plot=FALSE)$out
outliersScreen <- boxplot(CompData$screen, plot=FALSE)$out
CompData[which(CompData$screen %in% outliersScreen),]

boxplot(CompData$ram, plot=FALSE)$out
outliersRam <- boxplot(CompData$ram, plot=FALSE)$out
CompData[which(CompData$ram %in% outliersRam),]

## Keeping the outliers as it is.. Imputing may result in wrong prediction and removing will be heavy data loss
model1<- lm(price~.,data = CompData)
summary(model1)

vif(model1)
avPlots(model1)
## All features are significant and VIF is less than 7


##StartUpStandard <-as.data.frame(scale(StartUp))
# Splitting the dataset into test and train
set.seed(1211)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(CompData,SplitRatio = 0.75)

CompDataTrain =subset(CompData,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
CompDataTest=subset(CompData, sample==FALSE)


ggcorrplot(cor(CompDataTrain),hc.order = TRUE,type="lower",lab=TRUE)

## hd and ram have collinearity. hd and trend has colinearity
purecorr <- as.data.frame(cor2pcor(cor(CompDataTrain)),row.names = c("price","speed","hd","ram","screen","ads","trend","CDYes","MultiYes","PremiumYes"))

names(purecorr) <- c("price","speed","hd","ram","screen","ads","trend","CDYes","MultiYes","PremiumYes")
ggcorrplot(purecorr,type="lower",lab=TRUE)

##Standardizing the data


Price_Std <- sqrt(var(CompDataTrain$price))

Price_Mean <- mean(CompDataTrain$price)
View(CompDataTrain)
summary(CompDataTrain)

CompDataStandard <-CompDataTrain
## keeping the outlier as it is.

CompDataStandard$price <-as.numeric(scale(CompDataTrain$price))
CompDataStandard$speed <-as.numeric(scale(CompDataTrain$speed))
CompDataStandard$hd <-as.numeric(scale(CompDataTrain$hd))
CompDataStandard$ram <-as.numeric(scale(CompDataTrain$ram))
CompDataStandard$screen <-as.numeric(scale(CompDataTrain$screen))
CompDataStandard$ads <-as.numeric(scale(CompDataTrain$ads))
CompDataStandard$trend <-as.numeric(scale(CompDataTrain$trend))

View(CompDataStandard)
summary(CompDataStandard)
str(CompDataStandard)
CompDataStandard <- as.data.frame(CompDataStandard)

par(mfrow=c(3,3))
qqnorm(CompDataStandard$price)
qqline(CompDataStandard$price)

qqnorm((CompDataStandard$price)^2)
qqline((CompDataStandard$price)^2)

qqnorm(sqrt(CompDataStandard$price)) ###ssss
qqline(sqrt(CompDataStandard$price))

qqnorm(log(CompDataStandard$price)+nthroot(CompDataStandard$price,3))    
qqline(log(CompDataStandard$price)+nthroot(CompDataStandard$price,3)) 


qqnorm(CompDataStandard$speed)  ###ssss
qqline(CompDataStandard$speed)

qqnorm((CompDataStandard$speed)^2)
qqline((CompDataStandard$speed)^2)

qqnorm(sqrt(CompDataStandard$speed))
qqline(sqrt(CompDataStandard$speed))

qqnorm(log(CompDataStandard$speed))    
qqline(log(CompDataStandard$speed)) 

qqnorm(CompDataStandard$hd)  
qqline(CompDataStandard$hd)

qqnorm((CompDataStandard$hd)^2)
qqline((CompDataStandard$hd)^2)

qqnorm(sqrt(CompDataStandard$hd)) ###ssss
qqline(sqrt(CompDataStandard$hd))

qqnorm(log(CompDataStandard$hd))    
qqline(log(CompDataStandard$hd)) 

qqnorm(CompDataStandard$ram)  ###ssss
qqline(CompDataStandard$ram)

qqnorm((CompDataStandard$ram)^2)
qqline((CompDataStandard$ram)^2)

qqnorm(sqrt(CompDataStandard$ram)) 
qqline(sqrt(CompDataStandard$ram))

qqnorm(log(CompDataStandard$ram))    
qqline(log(CompDataStandard$ram)) 

qqnorm(CompDataStandard$screen)  ###ssss
qqline(CompDataStandard$screen)

qqnorm((CompDataStandard$screen)^2)
qqline((CompDataStandard$screen)^2)

qqnorm(sqrt(CompDataStandard$screen)) 
qqline(sqrt(CompDataStandard$screen))

qqnorm(log(CompDataStandard$screen))    
qqline(log(CompDataStandard$screen)) 


qqnorm(CompDataStandard$ads)  ###ssss
qqline(CompDataStandard$ads)

qqnorm((CompDataStandard$ads)^2)
qqline((CompDataStandard$ads)^2)

qqnorm(sqrt(CompDataStandard$ads)) 
qqline(sqrt(CompDataStandard$ads))

qqnorm(log(CompDataStandard$ads))    
qqline(log(CompDataStandard$ads)) 


qqnorm(CompDataStandard$trend)  ###ssss
qqline(CompDataStandard$trend)

qqnorm((CompDataStandard$trend)^2)
qqline((CompDataStandard$trend)^2)

qqnorm(sqrt(CompDataStandard$trend)) 
qqline(sqrt(CompDataStandard$trend))

qqnorm(log(CompDataStandard$trend))    
qqline(log(CompDataStandard$trend)) 

######################################################
## Model Building
######################################################

model_CompData_Simple <- lm(price ~ ., data=CompDataStandard)
summary(model_CompData_Simple)

Mod_CompData_Details <- data.frame('model_CompData_Simple'
                                  ,'price'
                                  ,'speed+hd+ram+screen+ads+trend+CDYes+MultiYes+PremiumYes'
                                  ,cor(CompDataStandard$price , CompDataStandard$speed+CompDataStandard$hd+CompDataStandard$ram+CompDataStandard$screen+CompDataStandard$ads+CompDataStandard$trend+CompDataStandard$CDYes+CompDataStandard$MultiYes+CompDataStandard$PremiumYes)
                                  ,summary(model_CompData_Simple)$r.squared
                                  ,mean(model_CompData_Simple$residuals)
                                  ,sqrt(mean((model_CompData_Simple$residuals)^2)))
names(Mod_CompData_Details) <- c('ModelName','Y','X','r','R^2','ErrorSum','RMSE')
View(Mod_CompData_Details)
###########################################
model_CompData_S1<- lm(sqrt(price) ~ speed+sqrt(hd)+ram+screen+ads+trend+CDYes+MultiYes+PremiumYes, data=CompDataStandard)
summary(model_CompData_S1)

pred_CompData_S1<-(predict(model_CompData_S1,CompDataStandard)^2)

error_CompData_S1<-CompDataStandard$price - pred_CompData_S1

InsertRow <- data.frame('model_CompData_S1'
                        ,'sqrt(price)'
                        ,'speed+sqrt(hd)+ram+screen+ads+trend+CDYes+MultiYes+PremiumYes'
                        ,cor(sqrt(CompDataStandard$price) , CompDataStandard$speed+sqrt(CompDataStandard$hd)+CompDataStandard$ram+CompDataStandard$screen+CompDataStandard$ads+CompDataStandard$trend+CompDataStandard$CDYes+CompDataStandard$MultiYes+CompDataStandard$PremiumYes,use = "pairwise.complete.obs")
                        ,summary(model_CompData_S1)$r.squared
                        ,mean(error_CompData_S1,na.rm = TRUE)
                        ,sqrt(mean((error_CompData_S1)^2,na.rm = TRUE))
                        ,row.names = "2")
names(InsertRow) <- names(Mod_CompData_Details)
Mod_CompData_Details <-rbind(Mod_CompData_Details,InsertRow)
View(Mod_CompData_Details)

###########################################

CompDataStandardTest <-CompDataTest
## keeping the outlier as it is.

Price_Std <- sqrt(var(CompDataTest$price))

Price_Mean <- mean(CompDataTest$price)

## function to convert prediction to original format.

NonZTransform <-function (Y)
{
  Y <- (Y*Price_Std)+Price_Mean
  return(Y)
}

CompDataStandardTest$price <-as.numeric(scale(CompDataTest$price))
CompDataStandardTest$speed <-as.numeric(scale(CompDataTest$speed))
CompDataStandardTest$hd <-as.numeric(scale(CompDataTest$hd))
CompDataStandardTest$ram <-as.numeric(scale(CompDataTest$ram))
CompDataStandardTest$screen <-as.numeric(scale(CompDataTest$screen))
CompDataStandardTest$ads <-as.numeric(scale(CompDataTest$ads))
CompDataStandardTest$trend <-as.numeric(scale(CompDataTest$trend))

CompDataStandardTest <- as.data.frame(CompDataStandardTest)

pred_CompData_Final <- NonZTransform(predict(model_CompData_Simple,CompDataStandardTest))

errors_CompDataStandardTest<-CompDataStandardTest$price - pred_CompData_Final



##Visualization
ggplot(data = CompDataTest, aes(x = speed+hd+ram+screen+ads+trend+CDYes+MultiYes+PremiumYes, y = price)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = CompDataTest, aes(x = speed+hd+ram+screen+ads+trend+CDYes+MultiYes+PremiumYes, y = pred_CompData_Final))

par(mfrow=c(1,1))

###################################################################################################
# 04.ToyotaCorolla.csv
###################################################################################################
##Loading .csv into R
## File name "04.ToyotaCorolla.csv"
ToyotaData <- read.csv(file.choose(),header=TRUE) 

ToyotaData<-as.data.frame(ToyotaData)

library(data.table)
library(mltools)
## Viewing the data
View(ToyotaData)

summary(ToyotaData)

ToyotaData<-sqldf("select Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight from ToyotaData")

sqldf("select distinct Gears from ToyotaData")

str(ToyotaData)

ToyotaData$Doors <- as.factor(ToyotaData$Doors)
ToyotaData$Gears <- as.factor(ToyotaData$Gears)

sum(is.na(ToyotaData))


onehotencoder <- function(df_orig) {
  df<-cbind(df_orig)
  df_clmtyp<-data.frame(clmtyp=sapply(df,class))
  df_col_typ<-data.frame(clmnm=colnames(df),clmtyp=df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    if (df_col_typ[rownm,"clmtyp"]=="factor") {
      clmn_obj<-df[toString(df_col_typ[rownm,"clmnm"])] 
      dummy_matx<-data.frame(model.matrix( ~.-1, data = clmn_obj))
      dummy_matx<-dummy_matx[,c(1,3:ncol(dummy_matx))]
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
      df<-cbind(df,dummy_matx)
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
    }  }
  return(df)
}


ToyotaData1 <- onehotencoder(ToyotaData)
View(ToyotaData1)
str(ToyotaData1)

sum(is.na(ToyotaData1))


# Splitting the dataset into test and train
set.seed(1221)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(ToyotaData1,SplitRatio = 0.75)

ToyotaDataTrain =subset(ToyotaData1,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
ToyotaDataTest=subset(ToyotaData1, sample==FALSE)

Price_Std <- sqrt(var(ToyotaDataTrain$Price))

Profit_Mean <- mean(ToyotaDataTrain$Price)

## function to convert prediction to original format.

NonZTransform <-function (Y)
{
  Y <- (Y*Price_Std)+Profit_Mean
  return(Y)
}

ToyotaDataTrainStd<-as.data.frame(ToyotaDataTrain)

ToyotaDataTrainStd$Price <- as.numeric(scale(ToyotaDataTrain$Price))
ToyotaDataTrainStd$Age_08_04 <- as.numeric(scale(ToyotaDataTrain$Age_08_04))
ToyotaDataTrainStd$KM <- as.numeric(scale(ToyotaDataTrain$KM))
ToyotaDataTrainStd$HP <- as.numeric(scale(ToyotaDataTrain$HP))
ToyotaDataTrainStd$cc <- as.numeric(scale(ToyotaDataTrain$cc))
ToyotaDataTrainStd$Quarterly_Tax <- as.numeric(scale(ToyotaDataTrain$Quarterly_Tax))
ToyotaDataTrainStd$Weight <- as.numeric(scale(ToyotaDataTrain$Weight))



summary(ToyotaDataTrainStd)
View(ToyotaDataTrainStd)
str(ToyotaDataTrainStd)

boxplot(ToyotaDataTrain$Price, Col="blue", horizontal = T)   ##Outlier
boxplot(ToyotaDataTrain$Age_08_04, Col="blue", horizontal = T) ##Outlier
boxplot(ToyotaDataTrain$KM, Col="blue", horizontal = T) ##Outlier
boxplot(ToyotaDataTrain$HP, Col="blue", horizontal = T) ##Outlier
boxplot(ToyotaDataTrain$cc, Col="blue", horizontal = T) ##Outlier
boxplot(ToyotaDataTrain$Quarterly_Tax, Col="blue", horizontal = T) ##Outlier
boxplot(ToyotaDataTrain$Weight, Col="blue", horizontal = T) ##Outlier

sqldf("select max(weight) from ToyotaDataTrain")  #1480

boxplot(ToyotaDataTrain$Weight)$out
outliersHP<-boxplot(ToyotaDataTrain$Weight, plot=FALSE)$out

View(ToyotaDataTrain[which(ToyotaDataTrain$Weight %in% 7.915018),])

ToyotaDataTrain1 <- ToyotaDataTrain[-which(ToyotaDataTrain$Weight %in% 7.915018),]

boxplot(ToyotaDataTrain1$Weight, Col="blue", horizontal = T)

ggpairs(ToyotaDataTrain)
ggcorrplot(cor(ToyotaDataTrain),hc.order = TRUE,type="lower",lab=TRUE)


purecorr <- as.data.frame(cor2pcor(cor(ToyotaDataTrain)),row.names = c("price","Age_08_04","KM","HP","cc","Quarterly_Tax","Weight","Doors2","Doors4","Doors5","Gears3","Gears5","Gears6"))

names(purecorr) <- c("price","Age_08_04","KM","HP","cc","Quarterly_Tax","Weight","Doors2","Doors4","Doors5","Gears3","Gears5","Gears6")


ggcorrplot(purecorr,type="lower",lab=TRUE)


## Creating first Model
model_ToyotaDataTrain_all <- lm(ToyotaDataTrain$Price ~ ., data = ToyotaDataTrain)
summary(model_ToyotaDataTrain_all)

## cc,Doors2,Doors4,door5,Gear5,Gears6 are not significant. Lets check individually
model_ToyotaTrainData_cc <-lm(Price~cc, data = ToyotaDataTrain)  ##significant
summary(model_ToyotaTrainData_cc)

model_ToyotaTrainData_Doors2 <-lm(Price~Doors2, data = ToyotaDataTrain)
summary(model_ToyotaTrainData_Doors2)

model_ToyotaTrainData_Doors4 <-lm(Price~Doors4, data = ToyotaDataTrain) ##significant
summary(model_ToyotaTrainData_Doors4)

model_ToyotaTrainData_Doors5 <-lm(Price~Doors5, data = ToyotaDataTrain)  ##significant
summary(model_ToyotaTrainData_Doors5)

model_ToyotaTrainData_Gears5 <-lm(Price~Gears5, data = ToyotaDataTrain) ##significant
summary(model_ToyotaTrainData_Gears5)

model_ToyotaTrainData_Gears6 <-lm(Price~Gears6, data = ToyotaDataTrain) ##significant
summary(model_ToyotaTrainData_Gears6)

## Door2 is not contributing to price and hence removing it from the model
ToyotaDataTrain2<-sqldf("select price,Age_08_04,KM,HP,cc,Quarterly_Tax,Weight,Doors4,Doors5,Gears3,Gears5,Gears6 from ToyotaDataTrain")

## Creating first Model
model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2)
summary(model_ToyotaDataTrain2_all)

model_ToyotaTrainData2_Doors5_cc <-lm(Price~cc+Doors5, data = ToyotaDataTrain2)   ##significant
summary(model_ToyotaTrainData2_Doors5_cc)

model_ToyotaTrainData2_Doors45 <-lm(Price~Doors4+Doors5, data = ToyotaDataTrain2)  
summary(model_ToyotaTrainData2_Doors45)

model_ToyotaTrainData2_Doors5_Gears5 <-lm(Price~Gears5+Doors5, data = ToyotaDataTrain2)  ##significant
summary(model_ToyotaTrainData2_Doors5_Gears5)

model_ToyotaTrainData2_Doors5_Gears6 <-lm(Price~Gears6+Doors5, data = ToyotaDataTrain2)  ##significant
summary(model_ToyotaTrainData2_Doors5_Gears6)

model_ToyotaTrainData2_Doors5_cc_Gears6 <-lm(Price~Gears6+cc+Doors5, data = ToyotaDataTrain2)   ##significant
summary(model_ToyotaTrainData2_Doors5_cc_Gears6)

model_ToyotaTrainData2_Doors5_cc_Gears5 <-lm(Price~Gears5+cc+Doors5, data = ToyotaDataTrain2)   ##significant
summary(model_ToyotaTrainData2_Doors5_cc_Gears5)

model_ToyotaTrainData2_Doors5_cc_Gears56 <-lm(Price~Gears6+Gears5+cc+Doors5, data = ToyotaDataTrain2)  
summary(model_ToyotaTrainData2_Doors5_cc_Gears56)

model_ToyotaTrainData_Doors5_Gears56 <-lm(Price~Gears6+Gears5+Doors5, data = ToyotaDataTrain)  
summary(model_ToyotaTrainData_Doors5_Gears56)

## Door2, gear5,gear6 is not all contributing and hence needs to be removed from model.



vif(model_ToyotaDataTrain2_all)

attributes(alias(model_ToyotaDataTrain_all)$Complete)$dimnames[[1]]
## VIF value for Gears5 and Gears6 are more than 7.

avPlots(model_ToyotaDataTrain2_all)


influence.measures(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4) 

model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2[-57,])
summary(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4) 

## cc has been significant

model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2[-c(57,154),])
summary(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4) 

model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2[-c(57,154,665),])  ## final removal
summary(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4) 

model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2[-c(57,154,665,418),])
summary(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4) 

model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2[-c(57,154,665,418,455),])
summary(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4) 

model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2[-c(57,154,665,418,455,364),])
summary(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4)

model_ToyotaDataTrain2_all <- lm(Price ~ ., data = ToyotaDataTrain2[-c(57,154,665,418,455,364,133),])
summary(model_ToyotaDataTrain2_all)

influenceIndexPlot(model_ToyotaDataTrain2_all,id.n=4) 
influencePlot(model_ToyotaDataTrain2_all,id.n=4)

ToyotaDataTrain3<-ToyotaDataTrain2[-c(57,154,665,418,455,364,133),]
View(ToyotaDataTrain3)

## Removing Grear5 and gear 6 as they are insignificant
ToyotaDataTrain3<-ToyotaDataTrain3[,-c(11,12)]

str(ToyotaDataTrain3)

ToyotaTrainstand <- ToyotaDataTrain3

ToyotaTrainstand$Price <-as.numeric(scale(ToyotaTrainstand$Price))
ToyotaTrainstand$Age_08_04 <-as.numeric(scale(ToyotaTrainstand$Age_08_04))
ToyotaTrainstand$KM <-as.numeric(scale(ToyotaTrainstand$KM))
ToyotaTrainstand$HP <-as.numeric(scale(ToyotaTrainstand$HP))
ToyotaTrainstand$cc <-as.numeric(scale(ToyotaTrainstand$cc))
ToyotaTrainstand$Quarterly_Tax <-as.numeric(scale(ToyotaTrainstand$Quarterly_Tax))
ToyotaTrainstand$Weight <-as.numeric(scale(ToyotaTrainstand$Weight))

summary(ToyotaTrainstand)

str(ToyotaTrainstand)

par(mfrow=c(3,3))
qqnorm(ToyotaTrainstand$Price)
qqline(ToyotaTrainstand$Price)

qqnorm((ToyotaTrainstand$Price)^2)
qqline((ToyotaTrainstand$Price)^2)

qqnorm(sqrt(ToyotaTrainstand$Price))
qqline(sqrt(ToyotaTrainstand$Price))

qqnorm(log(ToyotaTrainstand$Price))  ##select
qqline(log(ToyotaTrainstand$Price))

qqnorm(exp(ToyotaTrainstand$Price))
qqline(exp(ToyotaTrainstand$Price))

par(mfrow=c(3,3))
qqnorm(ToyotaTrainstand$Age_08_04) ##select
qqline(ToyotaTrainstand$Age_08_04)

qqnorm((ToyotaTrainstand$Age_08_04)^2)
qqline((ToyotaTrainstand$Age_08_04)^2)

qqnorm(sqrt(ToyotaTrainstand$Age_08_04))
qqline(sqrt(ToyotaTrainstand$Age_08_04))

qqnorm(log(ToyotaTrainstand$Age_08_04))#+sqrt(ToyotaTrainstand$Price))  
qqline(log(ToyotaTrainstand$Age_08_04))#+sqrt(ToyotaTrainstand$Price))

qqnorm(exp(ToyotaTrainstand$Age_08_04))
qqline(exp(ToyotaTrainstand$Age_08_04))

par(mfrow=c(3,3))
qqnorm(ToyotaTrainstand$KM) 
qqline(ToyotaTrainstand$KM)

qqnorm((ToyotaTrainstand$KM)^2)
qqline((ToyotaTrainstand$KM)^2)

qqnorm(sqrt(ToyotaTrainstand$KM)) ##select
qqline(sqrt(ToyotaTrainstand$KM))

qqnorm(log(ToyotaTrainstand$KM)+sqrt(ToyotaTrainstand$KM))   ##select
qqline(log(ToyotaTrainstand$KM)+sqrt(ToyotaTrainstand$KM))

qqnorm(exp(ToyotaTrainstand$KM))
qqline(exp(ToyotaTrainstand$KM))


par(mfrow=c(3,3))
qqnorm(ToyotaTrainstand$HP) 
qqline(ToyotaTrainstand$HP)

qqnorm((ToyotaTrainstand$HP)^2)
qqline((ToyotaTrainstand$HP)^2)

qqnorm(sqrt(ToyotaTrainstand$HP)) 
qqline(sqrt(ToyotaTrainstand$HP))

qqnorm(log(ToyotaTrainstand$HP))   
qqline(log(ToyotaTrainstand$HP))

qqnorm(exp(ToyotaTrainstand$HP)+(ToyotaTrainstand$HP)^2) ##select
qqline(exp(ToyotaTrainstand$HP)+(ToyotaTrainstand$HP)^2)

par(mfrow=c(3,3))
qqnorm(ToyotaTrainstand$cc) ##select
qqline(ToyotaTrainstand$cc)

qqnorm((ToyotaTrainstand$cc)^2)
qqline((ToyotaTrainstand$cc)^2)

qqnorm((ToyotaTrainstand$cc)^2+ToyotaTrainstand$cc)
qqline((ToyotaTrainstand$cc)^2+ToyotaTrainstand$cc)

qqnorm((ToyotaTrainstand$cc)^2+ToyotaTrainstand$cc+sqrt(ToyotaTrainstand$cc))
qqline((ToyotaTrainstand$cc)^2+ToyotaTrainstand$cc+sqrt(ToyotaTrainstand$cc))

qqnorm(sqrt(ToyotaTrainstand$cc)) 
qqline(sqrt(ToyotaTrainstand$cc))

qqnorm(log(ToyotaTrainstand$cc))   
qqline(log(ToyotaTrainstand$cc))

qqnorm(exp(ToyotaTrainstand$cc)) 
qqline(exp(ToyotaTrainstand$cc))

par(mfrow=c(3,3))
qqnorm(ToyotaTrainstand$Quarterly_Tax) 
qqline(ToyotaTrainstand$Quarterly_Tax)

qqnorm((ToyotaTrainstand$Quarterly_Tax)^2)
qqline((ToyotaTrainstand$Quarterly_Tax)^2)

qqnorm(sqrt(ToyotaTrainstand$Quarterly_Tax))  ##select
qqline(sqrt(ToyotaTrainstand$Quarterly_Tax))

qqnorm(log(ToyotaTrainstand$Quarterly_Tax)+nthroot(ToyotaTrainstand$Quarterly_Tax,3))   
qqline(log(ToyotaTrainstand$Quarterly_Tax)+nthroot(ToyotaTrainstand$Quarterly_Tax,3))

qqnorm(exp(ToyotaTrainstand$Quarterly_Tax)) 
qqline(exp(ToyotaTrainstand$Quarterly_Tax))

par(mfrow=c(3,3))
qqnorm(ToyotaTrainstand$Weight) 
qqline(ToyotaTrainstand$Weight)

qqnorm((ToyotaTrainstand$Weight)^2)
qqline((ToyotaTrainstand$Weight)^2)

qqnorm(sqrt(ToyotaTrainstand$Weight))  
qqline(sqrt(ToyotaTrainstand$Weight))

qqnorm(log(ToyotaTrainstand$Weight))  ##select 
qqline(log(ToyotaTrainstand$Weight))

qqnorm(exp(ToyotaTrainstand$Weight)) 
qqline(exp(ToyotaTrainstand$Weight))

##################################################################
## Model building
#################################################################3
model1<-lm(Price ~ Age_08_04+KM+HP+cc+Quarterly_Tax+Weight+Doors4+Doors5+Gears3,data = ToyotaTrainstand)
summary(model1)

Mod_Corolla_Details <- data.frame('model1'
                                   ,'Price'
                                   ,'Age_08_04+KM+HP+cc+Quarterly_Tax+Weight+Doors4+Doors5+Gears3'
                                   ,cor(ToyotaTrainstand$Price , ToyotaTrainstand$Age_08_04+ToyotaTrainstand$KM+ToyotaTrainstand$HP+ToyotaTrainstand$cc+ToyotaTrainstand$Quarterly_Tax+ToyotaTrainstand$Weight+ToyotaTrainstand$Doors4+ToyotaTrainstand$Doors5+ToyotaTrainstand$Gears3)
                                   ,summary(model1)$r.squared
                                   ,mean(model1$residuals)
                                   ,sqrt(mean((model1$residuals)^2)))
names(Mod_Corolla_Details) <- c('ModelName','Y','X','r','R^2','ErrorSum','RMSE')
View(Mod_Corolla_Details)

model2<-lm(log(Price) ~ Age_08_04+sqrt(KM)+exp(HP)+cc+sqrt(Quarterly_Tax)+log(Weight)+Doors4+Doors5+Gears3,data = ToyotaTrainstand)
summary(model2)

pred_model2<-exp(predict(model2,ToyotaTrainstand))

error_model2<-ToyotaTrainstand$Price - NonZTransform(pred_model2)

InsertRow1 <- data.frame('model2'
                        ,'log(Price)'
                        ,'Age_08_04+sqrt(KM)+exp(HP)+cc+sqrt(Quarterly_Tax)+log(Weight)+Doors4+Doors5+Gears3'
                        ,cor(log(ToyotaTrainstand$Price) , ToyotaTrainstand$Age_08_04+sqrt(ToyotaTrainstand$KM)+exp(ToyotaTrainstand$HP)+ToyotaTrainstand$cc+sqrt(ToyotaTrainstand$Quarterly_Tax)+log(ToyotaTrainstand$Weight)+ToyotaTrainstand$Doors4+ToyotaTrainstand$Doors5+ToyotaTrainstand$Gears3,use = "pairwise.complete.obs")
                        ,summary(model2)$r.squared
                        ,mean(error_model2,na.rm = TRUE)
                        ,sqrt(mean((error_model2)^2,na.rm = TRUE))
                        ,row.names = "2")
names(InsertRow1) <- names(Mod_Corolla_Details)
Mod_Corolla_Details <-rbind(Mod_Corolla_Details,InsertRow1)
View(Mod_Corolla_Details)

is.numeric(ToyotaTrainstand$Price)
str(ToyotaTrainstand)


model3<-lm(log(Price) ~ Age_08_04+sqrt(KM)+log(KM)+exp(HP)+cc+sqrt(Quarterly_Tax)+log(Weight)+Doors4+Doors5+Gears3,data = ToyotaTrainstand)
summary(model3)

pred_model3<-exp(predict(model3,ToyotaTrainstand))

error_model3<-ToyotaTrainstand$Price - NonZTransform(pred_model3)

InsertRow <- data.frame('model3'
                        ,'log(price)'
                        ,'Age_08_04+sqrt(KM)+log(KM)+exp(HP)+(HP)^2+cc+sqrt(Quarterly_Tax)+log(Weight)+Doors4+Doors5+Gears3'
                        ,cor(log(ToyotaTrainstand$Price) , ToyotaTrainstand$Age_08_04+log(ToyotaTrainstand$KM)+exp(ToyotaTrainstand$KM)+exp(ToyotaTrainstand$HP)+(ToyotaTrainstand$HP)^2+ToyotaTrainstand$cc+sqrt(ToyotaTrainstand$Quarterly_Tax)+log(ToyotaTrainstand$Weight)+ToyotaTrainstand$Doors4+ToyotaTrainstand$Doors5+ToyotaTrainstand$Gears3,use = "pairwise.complete.obs")
                        ,summary(model3)$r.squared
                        ,mean(error_model3,na.rm = TRUE)
                        ,sqrt(mean((error_model3)^2,na.rm = TRUE))
                        ,row.names = "3")
names(InsertRow) <- names(Mod_Corolla_Details)
Mod_Corolla_Details <-rbind(Mod_Corolla_Details,InsertRow)
View(Mod_Corolla_Details)

##selecting model1 as it has least rmse

ToyotaDataTestSTD<- ToyotaDataTest

ToyotaDataTestSTD$Price <- as.numeric(scale(ToyotaDataTestSTD$Price))
ToyotaDataTestSTD$Age_08_04 <- as.numeric(scale(ToyotaDataTestSTD$Age_08_04))
ToyotaDataTestSTD$KM <- as.numeric(scale(ToyotaDataTestSTD$KM))
ToyotaDataTestSTD$HP <- as.numeric(scale(ToyotaDataTestSTD$HP))
ToyotaDataTestSTD$cc <- as.numeric(scale(ToyotaDataTestSTD$cc))
ToyotaDataTestSTD$Quarterly_Tax <- as.numeric(scale(ToyotaDataTestSTD$Quarterly_Tax))
ToyotaDataTestSTD$Weight <- as.numeric(scale(ToyotaDataTestSTD$Weight))

PriceTest_Std <- sqrt(var(ToyotaDataTest$Price))

PriceTest_mean <- mean(ToyotaDataTest$Price)

## function to convert prediction to original format.

NonZTransform_Test<-function (Y)
{
  Y <- (Y*PriceTest_Std)+PriceTest_mean
  return(Y)
}

pred_final<- NonZTransform_Test(predict(model1,ToyotaDataTestSTD))



##Visualization
ggplot(data = ToyotaDataTest, aes(x = Age_08_04+KM+HP+cc+Quarterly_Tax+Weight+Doors4+Doors5+Gears3, y = Price)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = ToyotaDataTest, aes(x = Age_08_04+KM+HP+cc+Quarterly_Tax+Weight+Doors4+Doors5+Gears3, y = pred_final))

par(mfrow=c(1,1))

