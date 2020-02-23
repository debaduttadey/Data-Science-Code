#########################################
# File Name:-Assignment4.R
# Subject Simple Linear Regression
# Author:- Debadutta Dey
# Date:- 16/06/2019
#########################################
#Downloading and Installing requird packages
install.packages("XML")
install.packages("moments")
install.packages("caTools")

# Importing required packages
library(XML)
library(moments)
library(caTools)
library(pracma)
###################################################################################################
# Calorie Data
###################################################################################################
##Loading .csv into R
## File name "calories_consumed.csv"
calorie.data <- read.csv(file.choose(),col.names = c("Weight_Gained", "Calorie_Consumed") ) 

calorie.data<-as.data.frame(calorie.data)


## Viewing the data
View(calorie.data)

## Checking for null values
sum(is.na(calorie.data))

## Checking the summary. Mean is different from median for weight_gained column but Calorie_consumed has approximately equal for mean and median values
summary(calorie.data)

## still confirming for outliers
boxplot(calorie.data$Weight_Gained, col="blue", horizontal = T,outline = TRUE)
boxplot(calorie.data$Calorie_Consumed, col="blue", horizontal = T,outline = TRUE)

## Histogram
hist(calorie.data$Weight_Gained)
hist(calorie.data$Calorie_Consumed)


## saving original mean
WeightGained_Mean <- mean(calorie.data$Weight_Gained)
calorieConsumed_Mean <- mean(calorie.data$Calorie_Consumed)

## saving original standard deviation
WeightGained_Std <- sqrt(var(calorie.data$Weight_Gained))
calorieConsumed_Std <- sqrt(var(calorie.data$Calorie_Consumed))

WeightGained_Std<-

## Need to stanardizing the data..
CalorieDataScaled <- as.data.frame(scale(calorie.data))

## finding 
cor(CalorieDataScaled$Weight_Gained, CalorieDataScaled$Calorie_Consumed) ##0.946991
cor(CalorieDataScaled$Weight_Gained, (CalorieDataScaled$Calorie_Consumed)^2) ##0.6703888
cor(CalorieDataScaled$Weight_Gained, sqrt(CalorieDataScaled$Calorie_Consumed),use = "pairwise.complete.obs") ##0.9807456
cor(CalorieDataScaled$Weight_Gained, log(CalorieDataScaled$Calorie_Consumed),use = "pairwise.complete.obs")  ##0.9613015
cor(CalorieDataScaled$Weight_Gained, exp(CalorieDataScaled$Calorie_Consumed),use = "pairwise.complete.obs")  ## 0.9265574
cor(CalorieDataScaled$Weight_Gained, (CalorieDataScaled$Calorie_Consumed)^3,use = "pairwise.complete.obs")  ##0.847165
cor(CalorieDataScaled$Weight_Gained, (CalorieDataScaled$Calorie_Consumed)^4,use = "pairwise.complete.obs")  ##0.6963694


cor(CalorieDataScaled$Weight_Gained, CalorieDataScaled$Calorie_Consumed) ##0.946991
cor((CalorieDataScaled$Weight_Gained)^2, CalorieDataScaled$Calorie_Consumed) ##0.7077285
cor(sqrt(CalorieDataScaled$Weight_Gained), CalorieDataScaled$Calorie_Consumed,use = "pairwise.complete.obs") ##0.9917669
cor(log(CalorieDataScaled$Weight_Gained), CalorieDataScaled$Calorie_Consumed,use = "pairwise.complete.obs") ##0.9796794
cor(exp(CalorieDataScaled$Weight_Gained), CalorieDataScaled$Calorie_Consumed,use = "pairwise.complete.obs") ##0.8547915
cor(nthroot(CalorieDataScaled$Weight_Gained,3), CalorieDataScaled$Calorie_Consumed,use = "pairwise.complete.obs") ##0.9052956

cor(sqrt(CalorieDataScaled$Weight_Gained),(CalorieDataScaled$Calorie_Consumed)^3,use = "pairwise.complete.obs") ##0.9241437
cor(sqrt(CalorieDataScaled$Weight_Gained),(CalorieDataScaled$Calorie_Consumed)^2,use = "pairwise.complete.obs") ##0.9622139
cor(sqrt(CalorieDataScaled$Weight_Gained),sqrt(CalorieDataScaled$Calorie_Consumed),use = "pairwise.complete.obs") ##0.9988353
cor(sqrt(CalorieDataScaled$Weight_Gained),(CalorieDataScaled$Calorie_Consumed)^3,use = "pairwise.complete.obs") ##0.9241437

cor(nthroot(CalorieDataScaled$Weight_Gained,3),(CalorieDataScaled$Calorie_Consumed)^3,use = "pairwise.complete.obs") ##0.7220927
cor(nthroot(CalorieDataScaled$Weight_Gained,3),(CalorieDataScaled$Calorie_Consumed)^2,use = "pairwise.complete.obs") ##0.5511061
cor(nthroot(CalorieDataScaled$Weight_Gained,3),sqrt(CalorieDataScaled$Calorie_Consumed),use = "pairwise.complete.obs") ##0.8619711
cor(nthroot(CalorieDataScaled$Weight_Gained,3),(CalorieDataScaled$Calorie_Consumed)^3,use = "pairwise.complete.obs") ##0.7220927

cor(log(CalorieDataScaled$Weight_Gained),(CalorieDataScaled$Calorie_Consumed)^3,use = "pairwise.complete.obs") ##0.8943659
cor(log(CalorieDataScaled$Weight_Gained),(CalorieDataScaled$Calorie_Consumed)^2,use = "pairwise.complete.obs") ##0.9397962
cor(log(CalorieDataScaled$Weight_Gained),sqrt(CalorieDataScaled$Calorie_Consumed),use = "pairwise.complete.obs") ##0.9928417
cor(log(CalorieDataScaled$Weight_Gained),(CalorieDataScaled$Calorie_Consumed)^3,use = "pairwise.complete.obs") ##0.8943659

########################################################################################
## Creating Models
########################################################################################

model_calorieData_Simple <- lm(Weight_Gained ~ Calorie_Consumed, data=CalorieDataScaled)
summary(model_calorieData_Simple)

Mod_Cal_Details <- data.frame('model_calorieData_Simple'
                              ,'Weight_Gained'
                              ,'Calorie_Consumed'
                              ,cor(CalorieDataScaled$Weight_Gained , CalorieDataScaled$Calorie_Consumed)
                              ,summary(model_calorieData_Simple)$r.squared
                              ,mean(model_calorieData_Simple$residuals)
                              ,sqrt(mean((model_calorieData_Simple$residuals)^2)))
names(Mod_Cal_Details) <- c('ModelName','Y','X','r','R^2','ErrorSum','RMSE')
View(Mod_Cal_Details)
###########################################
model_calorieData_logY_sqrtX <- lm(log(Weight_Gained) ~ sqrt(Calorie_Consumed), data=CalorieDataScaled)
summary(model_calorieData_logY_sqrtX)

pred_model_calorieData_logY_sqrtX<- exp(predict(model_calorieData_logY_sqrtX, data=CalorieDataScaled))

error_model_calorieData_logY_sqrtX <- CalorieDataScaled$Weight_Gained - pred_model_calorieData_logY_sqrtX

InsertRow <- data.frame('model_calorieData_logY_sqrtX'
                        ,'log(Weight_Gained)'
                        ,'(Calorie_Consumed)^2'
                        ,cor(log(CalorieDataScaled$Weight_Gained) , (CalorieDataScaled$Calorie_Consumed)^2,use = "pairwise.complete.obs")
                        ,summary(model_calorieData_logY_sqrtX)$r.squared
                        ,mean(error_model_calorieData_logY_sqrtX)
                        ,sqrt(mean((error_model_calorieData_logY_sqrtX)^2))
                        ,row.names = "2")
names(InsertRow) <- names(Mod_Cal_Details)
Mod_Cal_Details <-rbind(Mod_Cal_Details,InsertRow)
View(Mod_Cal_Details)
###########################################
model_calorieData_logY_sqrtX_X <- lm(log(Weight_Gained) ~ sqrt(Calorie_Consumed)+Calorie_Consumed, data=CalorieDataScaled)
summary(model_calorieData_logY_sqrtX_X)

pred_model_calorieData_logY_sqrtX_X<- exp(predict(model_calorieData_logY_sqrtX_X, data=CalorieDataScaled))

error_model_calorieData_logY_sqrtX_X <- CalorieDataScaled$Weight_Gained - pred_model_calorieData_logY_sqrtX_X

InsertRow <- data.frame('model_calorieData_logY_sqrtX_X'
                        ,'log(Weight_Gained)'
                        ,'(Calorie_Consumed)^2+Calorie_Consumed'
                        ,cor(log(CalorieDataScaled$Weight_Gained) , (CalorieDataScaled$Calorie_Consumed)^2+CalorieDataScaled$Calorie_Consumed,use = "pairwise.complete.obs")
                        ,summary(model_calorieData_logY_sqrtX_X)$r.squared
                        ,mean(error_model_calorieData_logY_sqrtX_X)
                        ,sqrt(mean((error_model_calorieData_logY_sqrtX_X)^2))
                        ,row.names = "3")
names(InsertRow) <- names(Mod_Cal_Details)
Mod_Cal_Details <-rbind(Mod_Cal_Details,InsertRow)
View(Mod_Cal_Details)

###########################################
model_calorieData_SqrtY_sqrtX_X <- lm(sqrt(Weight_Gained) ~ sqrt(Calorie_Consumed)+Calorie_Consumed, data=CalorieDataScaled)
summary(model_calorieData_logY_sqrtX_X)

pred_model_calorieData_SqrtY_sqrtX_X<- (predict(model_calorieData_logY_sqrtX_X, data=CalorieDataScaled)^2)

error_model_calorieData_SqrtY_sqrtX_X <- CalorieDataScaled$Weight_Gained - pred_model_calorieData_logY_sqrtX_X

InsertRow <- data.frame('model_calorieData_SqrtY_sqrtX_X'
                        ,'sqrt(Weight_Gained)'
                        ,'(Calorie_Consumed)^2+Calorie_Consumed'
                        ,cor(sqrt(CalorieDataScaled$Weight_Gained) , (CalorieDataScaled$Calorie_Consumed)^2+CalorieDataScaled$Calorie_Consumed,use = "pairwise.complete.obs")
                        ,summary(model_calorieData_SqrtY_sqrtX_X)$r.squared
                        ,mean(error_model_calorieData_SqrtY_sqrtX_X)
                        ,sqrt(mean((error_model_calorieData_SqrtY_sqrtX_X)^2))
                        ,row.names = "4")
names(InsertRow) <- names(Mod_Cal_Details)
Mod_Cal_Details <-rbind(Mod_Cal_Details,InsertRow)
View(Mod_Cal_Details)
###########################################
model_calorieData_SqrtY_sqrtX <- lm(sqrt(Weight_Gained) ~ sqrt(Calorie_Consumed), data=CalorieDataScaled)
summary(model_calorieData_logY_sqrtX)

pred_model_calorieData_SqrtY_sqrtX<- (predict(model_calorieData_logY_sqrtX, data=CalorieDataScaled)^2)

error_model_calorieData_SqrtY_sqrtX <- CalorieDataScaled$Weight_Gained - pred_model_calorieData_logY_sqrtX_X

InsertRow <- data.frame('model_calorieData_SqrtY_sqrtX'
                        ,'sqrt(Weight_Gained)'
                        ,'(Calorie_Consumed)^2'
                        ,cor(sqrt(CalorieDataScaled$Weight_Gained) , (CalorieDataScaled$Calorie_Consumed)^2,use = "pairwise.complete.obs")
                        ,summary(model_calorieData_SqrtY_sqrtX)$r.squared
                        ,mean(error_model_calorieData_SqrtY_sqrtX)
                        ,sqrt(mean((error_model_calorieData_SqrtY_sqrtX)^2))
                        ,row.names = "5")
names(InsertRow) <- names(Mod_Cal_Details)
Mod_Cal_Details <-rbind(Mod_Cal_Details,InsertRow)
View(Mod_Cal_Details)

###########################################
model_calorieData_SqrtY <- lm(sqrt(Weight_Gained) ~ Calorie_Consumed, data=CalorieDataScaled)
summary(model_calorieData_SqrtY)

pred_model_calorieData_SqrtY<- (predict(model_calorieData_SqrtY, data=CalorieDataScaled)^2)

error_model_calorieData_SqrtY <- CalorieDataScaled$Weight_Gained - pred_model_calorieData_logY_sqrtX_X

InsertRow <- data.frame('model_calorieData_SqrtY'
                        ,'sqrt(Weight_Gained)'
                        ,'Calorie_Consumed'
                        ,cor(sqrt(CalorieDataScaled$Weight_Gained) , CalorieDataScaled$Calorie_Consumed,use = "pairwise.complete.obs")
                        ,summary(model_calorieData_SqrtY)$r.squared
                        ,mean(error_model_calorieData_SqrtY)
                        ,sqrt(mean((error_model_calorieData_SqrtY)^2))
                        ,row.names = "6")
names(InsertRow) <- names(Mod_Cal_Details)
Mod_Cal_Details <-rbind(Mod_Cal_Details,InsertRow)
View(Mod_Cal_Details)

###########################################
model_calorieData_SqX <- lm(Weight_Gained ~ (Calorie_Consumed)^2, data=CalorieDataScaled)
summary(model_calorieData_SqX)

pred_model_calorieData_SqX<- predict(model_calorieData_SqX, data=CalorieDataScaled)


InsertRow <- data.frame('model_calorieData_SqX'
                        ,'Weight_Gained'
                        ,'(Calorie_Consumed)^2'
                        ,cor(CalorieDataScaled$Weight_Gained , (CalorieDataScaled$Calorie_Consumed)^2,use = "pairwise.complete.obs")
                        ,summary(model_calorieData_SqX)$r.squared
                        ,mean(model_calorieData_SqX$residuals)
                        ,sqrt(mean((model_calorieData_SqX$residuals)^2))
                        ,row.names = "7")
names(InsertRow) <- names(Mod_Cal_Details)
Mod_Cal_Details <-rbind(Mod_Cal_Details,InsertRow)
View(Mod_Cal_Details)



## Final Model is model_calorieData_Simple as it has higher r and minimum RMSE.


NonZTransform <-function (Y)
{
  Y <- (Y*WeightGained_Std)+WeightGained_Mean
  return(Y)
}

pred<- NonZTransform(predict(model_calorieData_Simple,CalorieDataScaled))



## Visualization
library(ggplot2)

ggplot(data = calorie.data, aes(x = Calorie_Consumed, y = Weight_Gained)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = calorie.data, aes(x=Calorie_Consumed, y=pred))

#####################################################################################################################################
## delivery_time.csv
#####################################################################################################################################

##Loading .csv into R
## File name "delivery_time.csv"
DeliveryTime <- read.csv(file.choose(),header=TRUE,na.strings=c("","NA","na","Na","nA")) 

DeliveryTime<-as.data.frame(DeliveryTime)

View(DeliveryTime)

summary(DeliveryTime)

sum(is.na(DeliveryTime))

# Checking for outliers and they are not present
boxplot(DeliveryTime$Delivery.Time)

boxplot(DeliveryTime$Sorting.Time)



## Plotting the graph between Delivery.Time and Sorting.Time
## x= Sorting.Time y=Delivery.Time
par(mfrow=c(2,2))

plot(DeliveryTime$Delivery.Time ~ DeliveryTime$Sorting.Time)
cor(DeliveryTime$Delivery.Time , DeliveryTime$Sorting.Time) ## 0.8259973

plot(DeliveryTime$Delivery.Time ~ (DeliveryTime$Sorting.Time)^2)
cor(DeliveryTime$Delivery.Time , (DeliveryTime$Sorting.Time)^2) ## 0.7939063

plot(DeliveryTime$Delivery.Time ~ sqrt(DeliveryTime$Sorting.Time))
cor(DeliveryTime$Delivery.Time , sqrt(DeliveryTime$Sorting.Time)) ## 0.83415

plot(DeliveryTime$Delivery.Time ~ log(DeliveryTime$Sorting.Time))
cor(DeliveryTime$Delivery.Time , log(DeliveryTime$Sorting.Time)) ## 0.8339325

# For Y

plot((DeliveryTime$Delivery.Time)^2 ~ DeliveryTime$Sorting.Time)
cor((DeliveryTime$Delivery.Time)^2 , DeliveryTime$Sorting.Time) ## 0.7763201

plot(sqrt(DeliveryTime$Delivery.Time) ~ DeliveryTime$Sorting.Time)
cor(sqrt(DeliveryTime$Delivery.Time) , DeliveryTime$Sorting.Time) ## 0.8390768

plot(log(DeliveryTime$Delivery.Time) ~ DeliveryTime$Sorting.Time)
cor(log(DeliveryTime$Delivery.Time) , DeliveryTime$Sorting.Time) ## 0.8431773

## mixng X & Y

plot(log(DeliveryTime$Delivery.Time) ~ sqrt(DeliveryTime$Sorting.Time))
cor(log(DeliveryTime$Delivery.Time) , sqrt(DeliveryTime$Sorting.Time)) ## 0.8647743

plot(log(DeliveryTime$Delivery.Time) ~ sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time))
cor(log(DeliveryTime$Delivery.Time) , sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)) ## 0.8726203

plot(log(DeliveryTime$Delivery.Time) ~ sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)+log((DeliveryTime$Sorting.Time)^2))
cor(log(DeliveryTime$Delivery.Time) , sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)+log((DeliveryTime$Sorting.Time)^2)) ## 0.8759517

#####################
## Model building
#####################

DelMod_Simple <- lm(DeliveryTime$Delivery.Time ~ DeliveryTime$Sorting.Time)
summary(DelMod_Simple)

pred_DelMod_Simple<-predict(DelMod_Simple)

#View(data.frame(DeliveryTime$Delivery.Time,pred_DelMod_Simple,DelMod_Simple$residuals,DeliveryTime$Delivery.Time-DelMod_Simple$fitted.values))


## checking RMSE
sqrt(mean((DelMod_Simple$residuals)^2))

DeliveryModDetails<- data.frame('DelMod_Simple','Delivery.Time','Sorting.Time',cor(DeliveryTime$Delivery.Time , DeliveryTime$Sorting.Time),0.6823,sum(DelMod_Simple$residuals),sqrt(mean((DelMod_Simple$residuals)^2)))
names(DeliveryModDetails) <- c('ModelName','Y','X','r','R^2','ErrorSum','RMSE')

#############################################################
DelMod_logY_sqrtX <- lm(log(DeliveryTime$Delivery.Time) ~ sqrt(DeliveryTime$Sorting.Time))
summary(DelMod_logY_sqrtX)

pred_DelMod_logY_sqrtX<- exp(predict(DelMod_logY_sqrtX))

errors_DelMod_logY_sqrtX <- DeliveryTime$Delivery.Time - pred_DelMod_logY_sqrtX

## RMSE
sqrt(mean((errors_DelMod_logY_sqrtX)^2))


InsertRow <- data.frame('DelMod_logY_sqrtX','log(DeliveryTime$Delivery.Time)','sqrt(DeliveryTime$Sorting.Time)',cor(log(DeliveryTime$Delivery.Time), sqrt(DeliveryTime$Sorting.Time)),0.7478,sum(errors_DelMod_logY_sqrtX),sqrt(mean((errors_DelMod_logY_sqrtX)^2)),row.names = "2")
names(InsertRow) <- names(DeliveryModDetails)
DeliveryModDetails <-rbind(DeliveryModDetails,InsertRow)

#############################################################

DelMod_logY_sqrtX_LogX <- lm(log(DeliveryTime$Delivery.Time) ~ sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time))
summary(DelMod_logY_sqrtX_LogX)

pred_DelMod_logY_sqrtX_LogX <- exp(predict(DelMod_logY_sqrtX_LogX))


errors_DelMod_logY_sqrtX_LogX <- DeliveryTime$Delivery.Time - pred_DelMod_logY_sqrtX_LogX

## RMSE
sqrt(mean((errors_DelMod_logY_sqrtX_LogX)^2))

InsertRow <- data.frame('DelMod_logY_sqrtX_LogX','log(DeliveryTime$Delivery.Time)','sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)',cor(log(DeliveryTime$Delivery.Time), sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)),0.7782,sum(errors_DelMod_logY_sqrtX_LogX),sqrt(mean((errors_DelMod_logY_sqrtX_LogX)^2)),row.names = "3")
names(InsertRow) <- names(DeliveryModDetails)
DeliveryModDetails <-rbind(DeliveryModDetails,InsertRow)
View(DeliveryModDetails)
#############################################################

DelMod_logY_sqrtX_LogX_logSqX <- lm(log(DeliveryTime$Delivery.Time) ~ sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)+log((DeliveryTime$Sorting.Time)^2))
summary(DelMod_logY_sqrtX_LogX_logSqX)

pred_DelMod_logY_sqrtX_LogX_logSqX <- exp(predict(DelMod_logY_sqrtX_LogX_logSqX))

errors_DelMod_logY_sqrtX_LogX_logSqX <- DeliveryTime$Delivery.Time - pred_DelMod_logY_sqrtX_LogX_logSqX

## RMSE
sqrt(mean((pred_DelMod_logY_sqrtX_LogX_logSqX)^2))

InsertRow <- data.frame('DelMod_logY_sqrtX_LogX_logSqX','log(DeliveryTime$Delivery.Time)','sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)+log((DeliveryTime$Sorting.Time)^2)',cor(log(DeliveryTime$Delivery.Time), sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time)+log((DeliveryTime$Sorting.Time)^2)),0.7782,sum(pred_DelMod_logY_sqrtX_LogX_logSqX),sqrt(mean((pred_DelMod_logY_sqrtX_LogX_logSqX)^2)),row.names = "4")
names(InsertRow) <- names(DeliveryModDetails)
DeliveryModDetails <-rbind(DeliveryModDetails,InsertRow)
View(DeliveryModDetails)


## since RMSE is less so DelMod_logY_sqrtX_LogX is final model

##Visualization

ggplot(data = DeliveryTime, aes(x = sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time), y = Delivery.Time)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = DeliveryTime, aes(x = sqrt(DeliveryTime$Sorting.Time)+log(DeliveryTime$Sorting.Time),
                                                       y = pred_DelMod_logY_sqrtX_LogX))

par(mfrow=c(1,1))

#######################################################################################################################################
## emp_data.csv
#######################################################################################################################################

##Loading .csv into R
## File name "emp_data.csv"
EmpData <- read.csv(file.choose(),col.names = c("SalaryHike", "ChurnRate"),header=TRUE) 

EmpData<-as.data.frame(EmpData)

View(EmpData)

summary(EmpData)

sum(is.na(EmpData))

# Checking for outliers and they are not present
boxplot(EmpData$SalaryHike,horizontal = TRUE)

boxplot(EmpData$ChurnRate,horizontal = TRUE)

## saving original mean
SalaryHike_Mean <- mean(EmpData$SalaryHike)
ChurnRate_Mean <- mean(EmpData$ChurnRate)

## saving original standard deviation
SalaryHike_Std <- sqrt(var(EmpData$SalaryHike))
ChurnRate_Std <- sqrt(var(EmpData$ChurnRate))

## Need to stanardizing the data..
EmpDataScaled <- as.data.frame(scale(EmpData))



## Plotting the graph between Delivery.Time and Sorting.Time
## x= SalaryHike y=ChurnRate
par(mfrow=c(2,2))

plot(EmpDataScaled$ChurnRate ~ EmpDataScaled$SalaryHike, data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , EmpDataScaled$SalaryHike) ## -0.9117216

plot(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^2, data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , (EmpDataScaled$SalaryHike)^2) ## -0.2233553

plot(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^3, data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , (EmpDataScaled$SalaryHike)^3) ## -0.6848394

plot(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^4, data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , (EmpDataScaled$SalaryHike)^4) ## -0.3894534

plot(EmpDataScaled$ChurnRate ~ sqrt(EmpDataScaled$SalaryHike), data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , sqrt(EmpDataScaled$SalaryHike),use="pairwise.complete.obs")  ##-0.9946863


plot(EmpDataScaled$ChurnRate ~ log(EmpDataScaled$SalaryHike), data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , log(EmpDataScaled$SalaryHike),use="pairwise.complete.obs") ##  -0.9439475

## For Y

plot(EmpDataScaled$ChurnRate ~ EmpDataScaled$SalaryHike, data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , EmpDataScaled$SalaryHike) ## -0.9117216

plot((EmpDataScaled$ChurnRate)^2 ~ EmpDataScaled$SalaryHike, data=EmpDataScaled)
cor((EmpDataScaled$ChurnRate)^2 , EmpDataScaled$SalaryHike) ## -0.1111079

plot((EmpDataScaled$ChurnRate)^3 ~ EmpDataScaled$SalaryHike, data=EmpDataScaled)
cor((EmpDataScaled$ChurnRate)^3 , EmpDataScaled$SalaryHike) ##  -0.7258088

plot(sqrt(EmpDataScaled$ChurnRate) ~ EmpDataScaled$SalaryHike, data=EmpDataScaled)
cor(sqrt(EmpDataScaled$ChurnRate) , EmpDataScaled$SalaryHike,use="pairwise.complete.obs") ## -0.9935626

plot(log(EmpDataScaled$ChurnRate) ~ EmpDataScaled$SalaryHike, data=EmpDataScaled)
cor(log(EmpDataScaled$ChurnRate) , EmpDataScaled$SalaryHike,use="pairwise.complete.obs") ## -0.9906614

plot(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^3+EmpDataScaled$SalaryHike, data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , (EmpDataScaled$SalaryHike)^3+EmpDataScaled$SalaryHike) ## -0.7667426

plot(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^3+EmpDataScaled$SalaryHike+log(EmpDataScaled$SalaryHike), data=EmpDataScaled)
cor(EmpDataScaled$ChurnRate , (EmpDataScaled$SalaryHike)^3+EmpDataScaled$SalaryHike+log(EmpDataScaled$SalaryHike),use="pairwise.complete.obs")

###################
## Model building
###################

ED_Mod_Simple <- lm(EmpDataScaled$ChurnRate ~ EmpDataScaled$SalaryHike)

summary(ED_Mod_Simple)

pred_ED_Mod_Simple<-predict(ED_Mod_Simple)

confint(ED_Mod_Simple,level=0.95)

predict(ED_Mod_Simple,interval="predict")

## checking RMSE
sqrt(sum(ED_Mod_Simple$residuals^2))

EmpDataModDetails<- data.frame('ED_Mod_Simple','ChurnRate','SalaryHike',cor(EmpDataScaled$ChurnRate,EmpDataScaled$SalaryHike),0.8312,sum(ED_Mod_Simple$residuals),sqrt(mean((ED_Mod_Simple$residuals)^2)))
names(EmpDataModDetails) <- c('ModelName','Y','X','r','R^2','ErrorSum','RMSE')

##########################################################
ED_Mod_Xsq_X<- lm(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^2+EmpDataScaled$SalaryHike)
summary(ED_Mod_Xsq_X)

pred_ED_Mod_Xsq_X<-predict(ED_Mod_Xsq_X)


## checking RMSE
sqrt(mean(ED_Mod_Xsq_X$residuals^2))

InsertRow <- data.frame('ED_Mod_logY_LogX','ChurnRate','(SalaryHike)^2+SalaryHike',
                        cor(EmpDataScaled$ChurnRate, (EmpDataScaled$SalaryHike)^2+EmpDataScaled$SalaryHike),0.8312,
                        sum(ED_Mod_Xsq_X$residuals),
                        sqrt(mean((ED_Mod_Xsq_X$residuals)^2)),row.names = "2")
names(InsertRow) <- names(EmpDataModDetails)
EmpDataModDetails <-rbind(EmpDataModDetails,InsertRow)
View(EmpDataModDetails)

##########################################################################33

ED_Mod_Xcube_X<- lm(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^3+EmpDataScaled$SalaryHike)
summary(ED_Mod_Xcube_X)

pred_ED_Mod_Xcube_X<-predict(ED_Mod_Xcube_X)


## checking RMSE
sqrt(mean(ED_Mod_Xcube_X$residuals^2))

InsertRow <- data.frame('ED_Mod_Xcube_X','ChurnRate','(SalaryHike)^3+SalaryHike',
                        cor(EmpDataScaled$ChurnRate, (EmpDataScaled$SalaryHike)^3+EmpDataScaled$SalaryHike),0.8312,
                        sum(ED_Mod_Xcube_X$residuals),
                        sqrt(mean((ED_Mod_Xcube_X$residuals)^2)),row.names = "3")
names(InsertRow) <- names(EmpDataModDetails)
EmpDataModDetails <-rbind(EmpDataModDetails,InsertRow)
View(EmpDataModDetails)
#############################################################################################################


ED_Mod_Xcube_Xsq_X_logX <- lm(EmpDataScaled$ChurnRate ~ (EmpDataScaled$SalaryHike)^3+(EmpDataScaled$SalaryHike)^2+EmpDataScaled$SalaryHike+log(EmpDataScaled$SalaryHike),na.action=na.exclude)
summary(ED_Mod_Xcube_Xsq_X_logX)

pred_ED_Mod_Xcube_Xsq_X_logX<-predict(ED_Mod_Xcube_Xsq_X_logX)

## checking RMSE
sqrt(mean((ED_Mod_Xcube_Xsq_X_logX$residuals)^2,na.rm = TRUE ))

InsertRow <- data.frame('ED_Mod_Xcube_Xsq_X_logX','ChurnRate','(SalaryHike)^3+(SalaryHike)^2+SalaryHike+log(SalaryHike)',
                        cor(EmpDataScaled$ChurnRate, (EmpDataScaled$SalaryHike)^3+(EmpDataScaled$SalaryHike)^2+EmpDataScaled$SalaryHike+log(EmpDataScaled$SalaryHike),use = "pairwise.complete.obs")
                        ,0.9815,
                        sum(ED_Mod_Xcube_X_logX$residuals),
                        sqrt(mean((ED_Mod_Xcube_Xsq_X_logX$residuals)^2,na.rm = TRUE )),
                        row.names = "5")
names(InsertRow) <- names(EmpDataModDetails)
EmpDataModDetails <-rbind(EmpDataModDetails,InsertRow)
View(EmpDataModDetails)
#############################################################################################################

## model "ED_Mod_Simple" is final as it has less RMSE and more r. 

## now transforming back to normal values
## Converting prrediction to original format.

NonZTransform <-function (Y)
{
  Y <- (Y*ChurnRate_Std)+ChurnRate_Mean
  return(Y)
}

pred_Final<- NonZTransform(pred_ED_Mod_Simple)

############################################################################################################
##Visualization
ggplot(data = EmpData, aes(x = SalaryHike, y = ChurnRate)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = EmpData, aes(x = SalaryHike, y = pred_Final))


par(mfrow=c(1,1))
#######################################################################################################################################
## Salary_Data.csv
#######################################################################################################################################

##Loading .csv into R
## File name "Salary_Data.csv"
SalaryData <- read.csv(file.choose(),col.names = c("Experience", "Salary"),header=TRUE,na.strings=c("","NA","na","Na","nA")) 

SalaryData<-as.data.frame(SalaryData)

View(SalaryData)

summary(SalaryData)


# Checking for outliers and they are not present
boxplot(SalaryData$Experience,horizontal = TRUE)

boxplot(SalaryData$Salary,horizontal = TRUE)

## Need to scale the data.
## saving original mean
Experience_Mean <- mean(SalaryData$Experience)
Salary_Mean <- mean(SalaryData$Salary)

## saving original standard deviation
Experience_Std <- sqrt(var(SalaryData$Experience))
Salary_Std <- sqrt(var(SalaryData$Salary))

## Need to stanardizing the data..
SalaryDataScaled <- as.data.frame(scale(SalaryData))


cor(SalaryDataScaled$Salary,SalaryDataScaled$Experience) ##0.9782416
cor(SalaryDataScaled$Salary,(SalaryDataScaled$Experience)^2) ##0.3646331
cor(SalaryDataScaled$Salary,sqrt(SalaryDataScaled$Experience),use="pairwise.complete.obs") ##0.9396405
cor(SalaryDataScaled$Salary,log(SalaryDataScaled$Experience),use="pairwise.complete.obs") ##0.9191775
cor(SalaryDataScaled$Salary,exp(SalaryDataScaled$Experience),use="pairwise.complete.obs") ##0.8987212
cor(SalaryDataScaled$Salary,nthroot(SalaryDataScaled$Experience,3),use="pairwise.complete.obs") ##0.9710923

cor(SalaryDataScaled$Salary,SalaryDataScaled$Experience) ##0.9782416
cor((SalaryDataScaled$Salary)^2,SalaryDataScaled$Experience) ##0.4099312
cor(sqrt(SalaryDataScaled$Salary),SalaryDataScaled$Experience,use="pairwise.complete.obs") ##0.935865
cor(log(SalaryDataScaled$Salary),SalaryDataScaled$Experience,use="pairwise.complete.obs") ##0.8940651
cor(exp(SalaryDataScaled$Salary),SalaryDataScaled$Experience,use="pairwise.complete.obs") ##0.9345979
cor(nthroot(SalaryDataScaled$Salary,3),SalaryDataScaled$Experience,use="pairwise.complete.obs") ##0.92698

cor(sqrt(SalaryDataScaled$Salary),nthroot(SalaryDataScaled$Experience,3),use="pairwise.complete.obs") ##0.8577897
cor(exp(SalaryDataScaled$Salary),nthroot(SalaryDataScaled$Experience,3),use="pairwise.complete.obs") ##0.9026612
cor(sqrt(SalaryDataScaled$Salary),sqrt(SalaryDataScaled$Experience),use="pairwise.complete.obs") ##0.9303871
cor(exp(SalaryDataScaled$Salary),sqrt(SalaryDataScaled$Experience),use="pairwise.complete.obs") ##0.9088419
cor(sqrt(SalaryDataScaled$Salary),log(SalaryDataScaled$Experience),use="pairwise.complete.obs") ##0.9273791
cor(exp(SalaryDataScaled$Salary),log(SalaryDataScaled$Experience),use="pairwise.complete.obs") ##0.8647108
cor(sqrt(SalaryDataScaled$Salary),sqrt(SalaryDataScaled$Experience)+SalaryDataScaled$Experience,use="pairwise.complete.obs") ##0.9231017


qqnorm(SalaryDataScaled$Experience)
qqline(SalaryDataScaled$Experience)

qqnorm((SalaryDataScaled$Experience)^2)
qqline((SalaryDataScaled$Experience)^2)

qqnorm(sqrt(SalaryDataScaled$Experience))
qqline(sqrt(SalaryDataScaled$Experience))

qqnorm(log(SalaryDataScaled$Experience)+(SalaryDataScaled$Experience)^2+(SalaryDataScaled$Experience))
qqline(log(SalaryDataScaled$Experience)+(SalaryDataScaled$Experience)^2+(SalaryDataScaled$Experience))

qqnorm(SalaryDataScaled$Salary)
qqline(SalaryDataScaled$Salary)

qqnorm((SalaryDataScaled$Salary)^3+SalaryDataScaled$Salary)
qqline((SalaryDataScaled$Salary)^3+SalaryDataScaled$Salary)

#############################################
## Creating Model
#############################################
Model_SalData_Simple <- lm(Salary~Experience, data=SalaryDataScaled)
summary(Model_SalData_Simple)


Mod_SalData_Details <- data.frame('Model_SalData_Simple'
                              ,'Salary'
                              ,'Experience'
                              ,cor(SalaryDataScaled$Salary , SalaryDataScaled$Experience)
                              ,summary(Model_SalData_Simple)$r.squared
                              ,mean(Model_SalData_Simple$residuals)
                              ,sqrt(mean((Model_SalData_Simple$residuals)^2)))
names(Mod_SalData_Details) <- c('ModelName','Y','X','r','R^2','ErrorSum','RMSE')
View(Mod_SalData_Details)

#######
Model_SalData_LogY_SqrtX <- lm(log(Salary)~sqrt(Experience), data=SalaryDataScaled)
summary(Model_SalData_LogY_SqrtX)

pred_Model_SalData_LogY_SqrtX <- log(predict(Model_SalData_LogY_SqrtX, data=SalaryDataScaled))

error_Model_SalData_LogY_SqrtX <-SalaryDataScaled$Salary - pred_Model_SalData_LogY_SqrtX

InsertRow <- data.frame('Model_SalData_LogY_SqrtX'
                         ,'log(Salary)'
                        ,'sqrt(Experience)'
                        ,cor(log(SalaryDataScaled$Salary),sqrt(SalaryDataScaled$Experience),use="pairwise.complete.obs")
                        ,summary(Model_SalData_LogY_SqrtX)$r.squared
                        ,sum(error_Model_SalData_LogY_SqrtX,na.rm = TRUE)
                        ,sqrt(mean((error_Model_SalData_LogY_SqrtX)^2,na.rm = TRUE))
                        ,row.names = "3")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)

#######
Model_SalData_sqrtY_SqrtX <- lm(sqrt(Salary)~sqrt(Experience), data=SalaryDataScaled)
summary(Model_SalData_sqrtY_SqrtX)

pred_Model_SalData_sqrtY_SqrtX <- (predict(Model_SalData_sqrtY_SqrtX, data=SalaryDataScaled)^2)

error_Model_SalData_sqrtY_SqrtX <-SalaryDataScaled$Salary - pred_Model_SalData_sqrtY_SqrtX

InsertRow <- data.frame('Model_SalData_sqrtY_SqrtX'
                        ,'sqrt(Salary)'
                        ,'sqrt(Experience)'
                        ,cor(sqrt(SalaryDataScaled$Salary),sqrt(SalaryDataScaled$Experience),use="pairwise.complete.obs")
                        ,summary(Model_SalData_sqrtY_SqrtX)$r.squared
                        ,sum(error_Model_SalData_sqrtY_SqrtX,na.rm = TRUE)
                        ,sqrt(mean((error_Model_SalData_sqrtY_SqrtX)^2,na.rm = TRUE))
                        ,row.names = "4")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)

#######
Model_SalData_sqrtY_SqrtX_logX <- lm(sqrt(Salary)~sqrt(Experience)+log(Experience), data=SalaryDataScaled)
summary(Model_SalData_sqrtY_SqrtX_logX)

pred_Model_SalData_sqrtY_SqrtX_logX <- (predict(Model_SalData_sqrtY_SqrtX_logX, data=SalaryDataScaled)^2)

error_Model_SalData_sqrtY_SqrtX_logX <-SalaryDataScaled$Salary - pred_Model_SalData_sqrtY_SqrtX_logX

InsertRow <- data.frame('Model_SalData_sqrtY_SqrtX_logX'
                        ,'sqrt(Salary)'
                        ,'sqrt(Experience)+log(Experience)'
                        ,cor(sqrt(SalaryDataScaled$Salary),sqrt(SalaryDataScaled$Experience)+log(SalaryDataScaled$Experience),use="pairwise.complete.obs")
                        ,summary(Model_SalData_sqrtY_SqrtX_logX)$r.squared
                        ,sum(error_Model_SalData_sqrtY_SqrtX_logX,na.rm = TRUE)
                        ,sqrt(mean((error_Model_SalData_sqrtY_SqrtX_logX)^2,na.rm = TRUE))
                        ,row.names = "4")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)


#######
Model_SalData_sqrtY_SqrtX_X <- lm(sqrt(Salary)~sqrt(Experience)+Experience, data=SalaryDataScaled)
summary(Model_SalData_sqrtY_SqrtX_X)

pred_Model_SalData_sqrtY_SqrtX_X <- (predict(Model_SalData_sqrtY_SqrtX_X, data=SalaryDataScaled)^2)

error_Model_SalData_sqrtY_SqrtX_X <-SalaryDataScaled$Salary - pred_Model_SalData_sqrtY_SqrtX_X

InsertRow <- data.frame('Model_SalData_sqrtY_SqrtX_X'
                        ,'sqrt(Salary)'
                        ,'sqrt(Experience)+Experience'
                        ,cor(sqrt(SalaryDataScaled$Salary),sqrt(SalaryDataScaled$Experience)+SalaryDataScaled$Experience,use="pairwise.complete.obs")
                        ,summary(Model_SalData_sqrtY_SqrtX_X)$r.squared
                        ,sum(error_Model_SalData_sqrtY_SqrtX_X,na.rm = TRUE)
                        ,sqrt(mean((error_Model_SalData_sqrtY_SqrtX_X)^2,na.rm = TRUE))
                        ,row.names = "5")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)



#######
Model_SalData_logX_sqX_x <- lm(Salary ~log(Experience)+(Experience)^2+(Experience), data=SalaryDataScaled)
summary(Model_SalData_logX_sqX_x)

pred_Model_SalData_logX_sqX_x <- predict(Model_SalData_logX_sqX_x, data=SalaryDataScaled)


InsertRow <- data.frame('Model_SalData_logX_sqX_x'
                        ,'Salary'
                        ,'log(Experience)+(Experience)^2+(Experience)'
                        ,cor(SalaryDataScaled$Salary,log(SalaryDataScaled$Experience)+(SalaryDataScaled$Experience)^2+(SalaryDataScaled$Experience),use="pairwise.complete.obs")
                        ,summary(Model_SalData_logX_sqX_x)$r.squared
                        ,sum(Model_SalData_logX_sqX_x$residuals,na.rm = TRUE)
                        ,sqrt(mean((Model_SalData_logX_sqX_x$residuals)^2,na.rm = TRUE))
                        ,row.names = "6")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)
#######
Model_SalData_sqrty_logX_sqX_x <- lm(sqrt(Salary) ~log(Experience)+(Experience)^2+(Experience), data=SalaryDataScaled)
summary(Model_SalData_sqrty_logX_sqX_x)

pred_Model_SalData_sqrty_logX_sqX_x <- (predict(Model_SalData_sqrty_logX_sqX_x, data=SalaryDataScaled)^2)

error_Model_SalData_sqrty_logX_sqX_x<- SalaryDataScaled$Salary - pred_Model_SalData_sqrty_logX_sqX_x

InsertRow <- data.frame('Model_SalData_sqrty_logX_sqX_x'
                        ,'sqrt(Salary)'
                        ,'log(Experience)+(Experience)^2+(Experience)'
                        ,cor(sqrt(SalaryDataScaled$Salary),log(SalaryDataScaled$Experience)+(SalaryDataScaled$Experience)^2+(SalaryDataScaled$Experience),use="pairwise.complete.obs")
                        ,summary(Model_SalData_sqrty_logX_sqX_x)$r.squared
                        ,sum(error_Model_SalData_sqrty_logX_sqX_x,na.rm = TRUE)
                        ,sqrt(mean((error_Model_SalData_sqrty_logX_sqX_x)^2,na.rm = TRUE))
                        ,row.names = "7")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)



#######
Model_SalData_logX <- lm(Salary ~log(Experience)+Experience, data=SalaryDataScaled)
summary(Model_SalData_logX)

pred_Model_SalData_logX <- predict(Model_SalData_logX, data=SalaryDataScaled)

error_Model_SalData_sqrty_logX_sqX_x<- SalaryDataScaled$Salary - pred_Model_SalData_sqrty_logX_sqX_x

InsertRow <- data.frame('Model_SalData_logX'
                        ,'Salary'
                        ,'log(Experience)+Experience'
                        ,cor(SalaryDataScaled$Salary,log(SalaryDataScaled$Experience)+SalaryDataScaled$Experience,use="pairwise.complete.obs")
                        ,summary(Model_SalData_logX)$r.squared
                        ,sum(Model_SalData_logX$residuals,na.rm = TRUE)
                        ,sqrt(mean((Model_SalData_logX$residuals)^2,na.rm = TRUE))
                        ,row.names = "8")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)



#######
Model_SalData_sqx_X <- lm(Salary ~ (Experience)^2+Experience, data=SalaryDataScaled)
summary(Model_SalData_sqx_X)

pred_Model_SalData_cubeX_sqx_X <- predict(Model_SalData_sqx_X, data=SalaryDataScaled)



InsertRow <- data.frame('Model_SalData_sqx_X'
                        ,'Salary'
                        ,'(Experience)^2+Experience'
                        ,cor(SalaryDataScaled$Salary,(SalaryDataScaled$Experience)^2+SalaryDataScaled$Experience,use="pairwise.complete.obs")
                        ,summary(Model_SalData_sqx_X)$r.squared
                        ,sum(Model_SalData_sqx_X$residuals,na.rm = TRUE)
                        ,sqrt(mean((Model_SalData_sqx_X$residuals)^2,na.rm = TRUE))
                        ,row.names = "10")
names(InsertRow) <- names(Mod_SalData_Details)
Mod_SalData_Details <-rbind(Mod_SalData_Details,InsertRow)
View(Mod_SalData_Details)

## Model_SalData_logX_sqX_x is final model.

pred_final <-predict(Model_SalData_logX,SalaryDataScaled)


NonZTransform <-function (Y)
{
  Y <- (Y*Salary_Std)+Salary_Mean
  return(Y)
}

pred_Final<- NonZTransform(pred_final)

##Visualization
ggplot(data = SalaryData, aes(x = log(Experience)+Experience, y = Salary)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = SalaryData, aes(x = log(Experience)+Experience, y = pred_Final))
#######################################################################################################################################
## Salary_Data.csv
#######################################################################################################################################

##Loading .csv into R
## File name "Salary_Data.csv"
SalaryData <- read.csv(file.choose(),col.names = c("Experience", "Salary"),header=TRUE,na.strings=c("","NA","na","Na","nA")) 

SalaryData<-as.data.frame(SalaryData)

View(SalaryData)

summary(SalaryData)


# Checking for outliers and they are not present
boxplot(SalaryData$Experience,horizontal = TRUE)

boxplot(SalaryData$Salary,horizontal = TRUE)


## Plotting the graph between Delivery.Time and Sorting.Time
## x= Experience y=Salary
par(mfrow=c(2,2))

plot(SalaryData$Salary ~ SalaryData$Experience)
cor(SalaryData$Salary , SalaryData$Experience) ## 0.9782416

plot(SalaryData$Salary ~ (SalaryData$Experience)^2)
cor(SalaryData$Salary , (SalaryData$Experience)^2) ## 0.9567235

plot(SalaryData$Salary ~ sqrt(EmpDataScaled$Experience))
cor(SalaryData$Salary , sqrt(EmpDataScaled$Experience)) ## 0.9648839

plot(SalaryData$Salary ~ log(EmpDataScaled$Experience))
cor(SalaryData$Salary , log(EmpDataScaled$Experience)) ##  0.9240611

## For Y

plot((SalaryData$Salary)^2 ~ SalaryData$Experience)
cor((SalaryData$Salary)^2 , SalaryData$Experience) ##  0.9731053

plot((SalaryData$Salary)^3 ~ SalaryData$Experience)
cor((SalaryData$Salary)^3 , SalaryData$Experience) ## 0.9578624

plot(sqrt(SalaryData$Salary) ~ SalaryData$Experience)
cor(sqrt(SalaryData$Salary) , SalaryData$Experience) ## 0.974595

plot(log(SalaryData$Salary) ~ SalaryData$Experience)
cor(log(SalaryData$Salary) , SalaryData$Experience) ## 0.9653844

plot(log(sqrt(SalaryData$Salary))~ SalaryData$Experience)
cor(log(sqrt(SalaryData$Salary)) , SalaryData$Experience) ## 0.9653844

plot(log(sqrt(SalaryData$Salary))~ (SalaryData$Experience)^2)
cor(log(sqrt(SalaryData$Salary)) , (SalaryData$Experience)^2) ## 0.9157747

###################
## Model building
###################

SD_Mod_Simple <- lm(SalaryData$Salary ~ SalaryData$Experience)

summary(SD_Mod_Simple)

pred_SD_Mod_Simple<-predict(SD_Mod_Simple)

confint(SD_Mod_Simple,level=0.95)

predict(SD_Mod_Simple,interval="predict")

## checking RMSE
sqrt(sum(SD_Mod_Simple$residuals^2))  ## 30628.88

#############################################################################

SD_Mod_SqrtY <- lm(sqrt(SalaryData$Salary) ~ SalaryData$Experience)
summary(SD_Mod_SqrtY)

pred_SD_Mod_SqrtY<- (SD_Mod_SqrtY$fitted.values)^2

errors_SD_Mod_SqrtY <- SalaryData$Salary - pred_SD_Mod_SqrtY

## RMSE
sqrt(mean((errors_SD_Mod_SqrtY)^2)) ## 5926.009

#############################################################################

SD_Mod_LogY <- lm(log(SalaryData$Salary) ~ SalaryData$Experience)
summary(SD_Mod_LogY)

pred_SD_Mod_LogY<- exp(SD_Mod_LogY$fitted.values)

errors_SD_Mod_LogY <- SalaryData$Salary - pred_SD_Mod_LogY

## RMSE
sqrt(mean((errors_SD_Mod_LogY)^2)) ## 7213.235

#############################################################################

SD_Mod_LogY_SqrtY <- lm(log(sqrt(SalaryData$Salary))~ SalaryData$Experience)
summary(SD_Mod_LogY_SqrtY)

pred_SD_Mod_LogY_SqrtY<- exp((SD_Mod_LogY_SqrtY$fitted.values)^2)

errors_SD_Mod_LogY_SqrtY <- SalaryData$Salary - pred_SD_Mod_LogY_SqrtY

## RMSE
sqrt(mean((errors_SD_Mod_LogY_SqrtY)^2)) ## 7213.235


predSD <- as.data.frame(predict(SD_Mod_Simple, interval = "predict",data=SalaryData))

cor(predSD$fit,SalaryData$Salary) ## 0.9782416

predSD_final <- predict(SD_Mod_Simple)

##Visualization
ggplot(data = SalaryData, aes(x = SalaryData$Experience, y = SalaryData$Salary)) + 
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(colour = "lightgreen"),
        panel.grid.minor = element_line(colour = "yellow", size=1))+
  geom_point(color='black') +
  geom_line(color='firebrick',size=1,data = SalaryData, aes(x = SalaryData$Experience, y = predSD_final))

#Final model is SD_Mod_Simple.