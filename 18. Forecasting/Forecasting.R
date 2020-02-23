#########################################
# File Name:-Assignment18.R
# Subject:- Forecasting
# Author:- Debadutta Dey
#########################################

install.packages("forcast")
install.packages("fpp")
install.packages("smooth")
install.packages("tseries")

library(readxl)
library(sqldf)
library(dummies)
library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(metrics)
###################################################################################################
# CocaCola_Sales_Rawdata.xlsx
###################################################################################################
##Loading .xlsx into R
## File name "CocaCola_Sales_Rawdata.xlsx"
CocaCola <- read_xlsx(file.choose(),sheet="Sheet1" ,col_names = TRUE,na = c("NA","Na","nA","na",""))
?read_xlsx

CocaCola1<-as.data.frame(CocaCola)

sqldf("select * from CocaCola")
#CocaCola1<-sqldf("select * from CocaCola where Quarter not in ('Q1_96','Q2_96') ")

sum(is.na(CocaCola1))

str(CocaCola1)
View(CocaCola1)

CocaCola2<-as.data.frame(CocaCola1)


#CocaCola$Years<- sqldf("select '19'||SUBSTR(Quarter,4,length(Quarter)) from CocaCola")

plot(CocaCola2$Sales,type="o")


s<- dummy(CocaCola2$Quarter,sep="")
s<-as.data.frame(s)
head(s)

CocaCola2$Q186<-as.factor(s$QuarterQ1_86)
CocaCola2$Q286<-as.factor(s$QuarterQ2_86)
CocaCola2$Q386<-as.factor(s$QuarterQ3_86)
CocaCola2$Q486<-as.factor(s$QuarterQ4_86)
CocaCola2$Q187<-as.factor(s$QuarterQ1_87)
CocaCola2$Q287<-as.factor(s$QuarterQ2_87)
CocaCola2$Q387<-as.factor(s$QuarterQ3_87)
CocaCola2$Q487<-as.factor(s$QuarterQ4_87)
CocaCola2$Q188<-as.factor(s$QuarterQ1_88)
CocaCola2$Q288<-as.factor(s$QuarterQ2_88)
CocaCola2$Q388<-as.factor(s$QuarterQ3_88)
CocaCola2$Q488<-as.factor(s$QuarterQ4_88)
CocaCola2$Q189<-as.factor(s$QuarterQ1_89)
CocaCola2$Q289<-as.factor(s$QuarterQ2_89)
CocaCola2$Q389<-as.factor(s$QuarterQ3_89)
CocaCola2$Q489<-as.factor(s$QuarterQ4_89)
CocaCola2$Q190<-as.factor(s$QuarterQ1_90)
CocaCola2$Q290<-as.factor(s$QuarterQ2_90)
CocaCola2$Q390<-as.factor(s$QuarterQ3_90)
CocaCola2$Q490<-as.factor(s$QuarterQ4_90)
CocaCola2$Q191<-as.factor(s$QuarterQ1_91)
CocaCola2$Q291<-as.factor(s$QuarterQ2_91)
CocaCola2$Q391<-as.factor(s$QuarterQ3_91)
CocaCola2$Q491<-as.factor(s$QuarterQ4_91)
CocaCola2$Q192<-as.factor(s$QuarterQ1_92)
CocaCola2$Q292<-as.factor(s$QuarterQ2_92)
CocaCola2$Q392<-as.factor(s$QuarterQ3_92)
CocaCola2$Q492<-as.factor(s$QuarterQ4_92)
CocaCola2$Q193<-as.factor(s$QuarterQ1_93)
CocaCola2$Q293<-as.factor(s$QuarterQ2_93)
CocaCola2$Q393<-as.factor(s$QuarterQ3_93)
CocaCola2$Q493<-as.factor(s$QuarterQ4_93)
CocaCola2$Q194<-as.factor(s$QuarterQ1_94)
CocaCola2$Q294<-as.factor(s$QuarterQ2_94)
CocaCola2$Q394<-as.factor(s$QuarterQ3_94)
CocaCola2$Q494<-as.factor(s$QuarterQ4_94)
CocaCola2$Q195<-as.factor(s$QuarterQ1_95)
CocaCola2$Q295<-as.factor(s$QuarterQ2_95)
CocaCola2$Q395<-as.factor(s$QuarterQ3_95)
CocaCola2$Q495<-as.factor(s$QuarterQ4_95)
CocaCola2$Q196<-as.factor(s$QuarterQ1_96)
CocaCola2$Q296<-as.factor(s$QuarterQ2_96)

str(CocaCola2)

CocaCola2<- as.data.frame(lapply(CocaCola2[,-c(1,2)],as.factor))
#CocaCola2<-s
CocaCola2$Sales<-CocaCola1$Sales
#CocaCola2$Sales<-CocaCola1$Sales
str(CocaCola2)



CocaCola2$tSq<-CocaCola2$t*CocaCola2$t

CocaCola2$logSales<-log(CocaCola2$Sales)
str(CocaCola2)

i=38
CocaColaTrain<-CocaCola2[1:i,]
CocaColaTest<-CocaCola2[i+1:42,]
CocaColaTest<-sqldf("select * from CocaColaTest where t is not null")
#summary(CocaColaTrain)

####################################################################################
## Implementing Model Based Apporach
#####################################################################################


linmod<-lm(Sales~t,data=CocaColaTrain)
linpred<-data.frame(predict(linmod,interval='predict',newdata =CocaColaTest))

rmse_linear<-sqrt(mean((CocaColaTest$Sales-linpred$fit)^2,na.rm = TRUE))
rmse_linear # 815.2212

CocaCola_ModDetails <- data.frame('linmod'
                                  ,'Linear Model'
                                  , 'Sales'
                                  ,'t'
                                  ,rmse_linear
                                  )
names(CocaCola_ModDetails) <- c('ModelName','Model Description','Y','X','RMSE')


expmod<-lm(logSales~t,data=CocaColaTrain)
#summary(expmod)
exppred<-data.frame(predict(expmod,interval='predict',newdata =CocaColaTest))
exprmse<-sqrt(mean((CocaColaTest$Sales-exp(exppred$fit))^2,na.rm = TRUE))
exprmse # 665.79

InsertRow <- data.frame('expmod'
                        ,'Exponential Model'
                        ,'logSales'
                        ,'t'
                        ,exprmse
                        ,row.names = "2")
names(InsertRow) <- names(CocaCola_ModDetails)
CocaCola_ModDetails <-rbind(CocaCola_ModDetails,InsertRow)


Quadmod<-lm(Sales~t+tSq,data=CocaColaTrain)
QuadPred<-data.frame(predict(Quadmod,interval='predict',newdata =CocaColaTest))
Quadrmse<-sqrt(mean((CocaColaTest$Sales-QuadPred$fit)^2,na.rm=TRUE))
Quadrmse # 900.8873


InsertRow <- data.frame('Quadmod'
                        ,'Quadratic Model'
                        ,'Sales'
                        ,'t+tSq'
                        ,Quadrmse
                        ,row.names = "3")
names(InsertRow) <- names(CocaCola_ModDetails)
CocaCola_ModDetails <-rbind(CocaCola_ModDetails,InsertRow)

summary(CocaColaTrain)

x="QuarterQ1_86+QuarterQ1_87+QuarterQ1_88+QuarterQ1_89+QuarterQ1_90+QuarterQ1_91+QuarterQ1_92+QuarterQ1_93+QuarterQ1_94+QuarterQ1_95+QuarterQ1_96+QuarterQ2_86+QuarterQ2_87+QuarterQ2_88+QuarterQ2_89+QuarterQ2_90+QuarterQ2_91+QuarterQ2_92+QuarterQ2_93+QuarterQ2_94+QuarterQ2_95+QuarterQ2_96+QuarterQ3_86+QuarterQ3_87+QuarterQ3_88+QuarterQ3_89+QuarterQ3_90+QuarterQ3_91+QuarterQ3_92+QuarterQ3_93+QuarterQ3_94+QuarterQ3_95+QuarterQ4_86+QuarterQ4_87+QuarterQ4_88+QuarterQ4_89+QuarterQ4_90+QuarterQ4_91+QuarterQ4_92+QuarterQ4_93+QuarterQ4_94"

SeasonalAddModel<-lm(Sales~Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295,data=CocaColaTrain)
SeasonalAddpred<-data.frame(predict(SeasonalAddModel,newdata =CocaColaTest,interval='predict'))
SeasonalAddrmse<-sqrt(mean((CocaColaTest$Sales-SeasonalAddpred$fit)^2,na.rm = TRUE))
SeasonalAddrmse # 2372.704


InsertRow <- data.frame('SeasonalAddModel'
                        ,'Additive Seasonal Model'
                        ,'Sales'
                        ,'Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295'
                        ,SeasonalAddrmse
                        ,row.names = "4")
names(InsertRow) <- names(CocaCola_ModDetails)
CocaCola_ModDetails <-rbind(CocaCola_ModDetails,InsertRow)


AdditiveSeasonalLinearModel<-lm(Sales~t+Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295,data=CocaColaTrain)
AdditiveSeasonalLinearPred<-data.frame(predict(AdditiveSeasonalLinearModel,interval='predict',newdata =CocaColaTest))
AdditiveSeasonalLinearRmse<-sqrt(mean((CocaColaTest$Sales-AdditiveSeasonalLinearPred$fit)^2,na.rm=TRUE))
AdditiveSeasonalLinearRmse # 777.0584


InsertRow <- data.frame('AdditiveSeasonalLinearModel'
                        ,'Additive Seasonality with Linear Model'
                        ,'Sales'
                        ,'t+Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295'
                        ,AdditiveSeasonalLinearRmse
                        ,row.names = "5")
names(InsertRow) <- names(CocaCola_ModDetails)
CocaCola_ModDetails <-rbind(CocaCola_ModDetails,InsertRow)



AdditiveSeasonalQuadraticModel<-lm(Sales~t+tSq+Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295,data=CocaColaTrain)

AdditiveSeasonalQuadraticpred<-data.frame(predict(AdditiveSeasonalQuadraticModel,interval='predict',newdata =CocaColaTest))
AdditiveSeasonalQuadraticRmse<-sqrt(mean((CocaColaTest$Sales-AdditiveSeasonalQuadraticpred$fit)^2,na.rm=TRUE))
AdditiveSeasonalQuadraticRmse # 376.7647


InsertRow <- data.frame('AdditiveSeasonalQuadraticModel'
                        ,'Additive Seasonality with Quadratic Model'
                        ,'Sales'
                        ,'t+tSq+Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295'
                        ,AdditiveSeasonalQuadraticRmse
                        ,row.names = "6")
names(InsertRow) <- names(CocaCola_ModDetails)
CocaCola_ModDetails <-rbind(CocaCola_ModDetails,InsertRow)
View(CocaCola_ModDetails)


MultiplicativeSeasonalityModel<-lm(logSales~Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295,data = CocaColaTrain)
MultiplicativeSeasonalityPred<-data.frame(predict(MultiplicativeSeasonalityModel,newdata =CocaColaTest,interval='predict'))
MultiplicativeSeasonalityPredRmse<-sqrt(mean((CocaColaTest$Sales-exp(MultiplicativeSeasonalityPred$fit))^2,na.rm = TRUE))
MultiplicativeSeasonalityPredRmse # 2372.704


InsertRow <- data.frame('MultiplicativeSeasonalityModel'
                        ,'Multiplicative Seasonality Model'
                        ,'logSales'
                        ,'Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295'
                        ,MultiplicativeSeasonalityPredRmse
                        ,row.names = "7")
names(InsertRow) <- names(CocaCola_ModDetails)
CocaCola_ModDetails <-rbind(CocaCola_ModDetails,InsertRow)


MultiplicativeSeasonalityLinearModel<-lm(logSales~t+Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295,data = CocaColaTrain)
MultiplicativeSeasonalityLinearPred<-data.frame(predict(MultiplicativeSeasonalityLinearModel,newdata =CocaColaTest,interval='predict'))
MultiplicativeSeasonalityLinearModelRmse<-sqrt(mean((CocaColaTest$Sales-exp(MultiplicativeSeasonalityLinearPred$fit))^2,na.rm = TRUE))
MultiplicativeSeasonalityLinearModelRmse # 547.5685


InsertRow <- data.frame('MultiplicativeSeasonalityLinearModel'
                        ,'Multiplicative Seasonality Linear Model'
                        ,'logSales'
                        ,'t+Q186+Q286+Q386+Q486+Q187+Q287+Q387+Q487+Q188+Q288+Q388+Q488+Q189+Q289+Q389+Q489+Q190+Q290+Q390+Q490+Q191+Q291+Q391+Q491+Q192+Q292+Q392+Q492+Q193+Q293+Q393+Q493+Q194+Q294+Q394+Q494+Q195+Q295'
                        ,MultiplicativeSeasonalityLinearModelRmse
                        ,row.names = "8")
names(InsertRow) <- names(CocaCola_ModDetails)
CocaCola_ModDetails <-rbind(CocaCola_ModDetails,InsertRow)
View(CocaCola_ModDetails)

sqldf("select ModelName,`Model Description` from CocaCola_ModDetails order by Rmse limit 1")



######## checking for errors

residu<-residuals(expmod)

residu[1:38]
#windows()
i=4
acf(residu,lag.max = i)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

residu1 <- arima(residu, order=c(1,0,0))
str(residu1)

acf(residu1$residuals,lag.max = 4)
pred_res1<- predict(arima(residu1$residuals,order=c(1,0,0)),n.ahead = 4)

residu2 <- arima(pred_res1$se, order=c(1,0,0))
acf(residu2$residuals,lag.max = 4)
pred_res2<- predict(arima(residu2$residuals,order=c(1,0,0)),n.ahead = 4)


CocaColaOrigTest2$ErrorForcast<-pred_res2$pred
CocaColaOrigTest2$FinalForcast<-CocaColaOrigTest2$SalesForecast+CocaColaOrigTest2$ErrorForcast
View(CocaColaOrigTest2)

plot(CocaColaOrigTest2$FinalForcast)

########################################################################################################################
## Applying data based models
########################################################################################################################

##Loading .xlsx into R
## File name "CocaCola_Sales_Rawdata.xlsx"
CocaCola11 <- read_xlsx(file.choose(),sheet="Sheet1" ,col_names = TRUE,na = c("NA","Na","nA","na",""))

CocaCola10<-ts(CocaCola11$Sales,frequency = 4,start=c(42))

CocaColaTrain11<-CocaCola10[1:38]
CocaColaTest11<-CocaCola10[39:42]

# converting time series object
CocaColaTrain12<-ts(CocaColaTrain11,frequency = 4)
CocaColaTest12<-ts(CocaColaTest11,frequency = 4)

plot(CocaCola10)

##Applying ARIMA
CocaColaArimaM<-Arima(CocaColaTrain12,order = c(0,0,4))
Forcas<-forecast(CocaColaArimaM,h=4)
Accu<-accuracy(Forcas,x=as.numeric(CocaColaTest12))
Accu1<-as.data.frame(Accu)
Accu1[2,5]
plot(Forcas)


CocaColaHWMa<-HoltWinters(CocaColaTrain12,alpha = 0.2,beta = F,gamma = F)
CocaColaHWapred<-data.frame(predict(CocaColaHWMa,n.ahead=4))
plot(forecast(CocaColaHWMa,h=4))
CocaColaHWMa_mape<-sqrt(MSE(CocaColaTest12,CocaColaHWapred$fit))


CocaColaHWMab<-HoltWinters(CocaColaTrain12,alpha = 0.2,beta = 0.1,gamma = F)
CocaColaHWMabpred<-data.frame(predict(CocaColaHWMab,n.ahead = 4))
plot(forecast(CocaColaHWMab,h=4))
CocaColaHWMab_mape<-sqrt(MSE(CocaColaTest12,CocaColaHWMabpred$fit))


CocaColaHWMabg<-HoltWinters(CocaColaTrain12,alpha = 0.2,beta = 0.1,gamma = 0.1)
CocaColaHWMabgpred<-data.frame(predict(CocaColaHWMabg,n.ahead = 4))
plot(forecast(CocaColaHWMabg,h=4))
CocaColaHWMabg_mape<-sqrt(MSE(CocaColaTest12,CocaColaHWMabgpred$fit))


CocaColaHWMna<-HoltWinters(CocaColaTrain12,beta = F,gamma = F)
CocaColaHWMnapred<-data.frame(predict(CocaColaHWMna,n.ahead = 4))
plot(forecast(CocaColaHWMna,h=4))
CocaColaHWMna_mape<-sqrt(MSE(CocaColaTest12,CocaColaHWMnapred$fit))

CocaColaHWMnab<-HoltWinters(CocaColaTrain12,gamma=F)
CocaColaHWMpred<-data.frame(predict(CocaColaHWMnab,n.ahead=4))
plot(forecast(CocaColaHWMnab,h=4))
CocaColaHWMnab_mape<-sqrt(MSE(CocaColaTest12,CocaColaHWMpred$fit))

CocaColaHWMnabg<-HoltWinters(CocaColaTrain12)
CocaColaHWMnabgpred<-data.frame(predict(CocaColaHWMnabg,n.ahead =4))
plot(forecast(CocaColaHWMnabg,h=4))
CocaColaHWMnabg_mape<-sqrt(MSE(CocaColaTest12,CocaColaHWMnabgpred$fit))

CocaCola_ModDetails1<-data.frame(c("CocaColaArimaM","CocaColaHWMa","CocaColaHWMab","CocaColaHWMabg","CocaColaHWMna","CocaColaHWMnab","CocaColaHWMnabg")
                                ,c(Accu1[2,5],CocaColaHWMa_mape,CocaColaHWMab_mape,CocaColaHWMna_mape,hwna_mape,CocaColaHWMnab_mape,CocaColaHWMnabgpred))

colnames(CocaCola_ModDetails1)<-c("Model","RMSE")
View(CocaCola_ModDetails1)

########################################################################################


CocaColases_a<-ses(CocaColaTrain12,alpha = 0.2) # 
CocaColasesaPred<-data.frame(predict(CocaColases_a,h=4))
plot(forecast(CocaColases_a,n.ahead=4))
CocaColasesa_mape<-sqrt(MSE(CocaColaTest12,CocaColasesaPred$Point.Forecast))

InsertRow <- data.frame('CocaColases_a'
                        ,CocaColasesa_mape
                        ,row.names = "8")
names(InsertRow) <- names(CocaCola_ModDetails1)
CocaCola_ModDetails1 <-rbind(CocaCola_ModDetails1,InsertRow)
View(CocaCola_ModDetails1)


CocaColaholt_ab<-holt(CocaColaTrain12,alpha = 0.2,beta = 0.1)
CocaColaholtabPred<-data.frame(predict(CocaColaholt_ab,h=4))
plot(forecast(CocaColaholt_ab,h=4))
CocaColaholtab_mape<-sqrt(MSE(CocaColaTest12,CocaColaholtabPred$Point.Forecast))
?MSE

InsertRow <- data.frame('CocaColaholt_ab'
                        ,CocaColaholtab_mape
                        ,row.names = "9")
names(InsertRow) <- names(CocaCola_ModDetails1)
CocaCola_ModDetails1 <-rbind(CocaCola_ModDetails1,InsertRow)
View(CocaCola_ModDetails1)

CocaColahw_abg_new<-hw(CocaColaTrain12,alpha = 0.2,beta = 0.1,gamma = 0.1)
CocaColahwabg_pred_new<-data.frame(predict(CocaColahw_abg_new,h = 4))
plot(forecast(CocaColahw_abg_new,h=4))
CocaColahwabg_mape_new<-sqrt(MSE(CocaColaTest12,CocaColahwabg_pred_new$Point.Forecast))

InsertRow <- data.frame('CocaColahw_abg_new'
                        ,CocaColahwabg_mape_new
                        ,row.names = "10")
names(InsertRow) <- names(CocaCola_ModDetails1)
CocaCola_ModDetails1 <-rbind(CocaCola_ModDetails1,InsertRow)
View(CocaCola_ModDetails1)


CocaColases_na<-ses(CocaColaTrain12,alpha=NULL)
CocaColasesna_pred<-data.frame(predict(CocaColases_na,h = 4))
plot(forecast(CocaColases_na,h=4))
CocaColasesna_mape<-sqrt(MSE(CocaColaTest12,CocaColasesna_pred$Point.Forecast))

InsertRow <- data.frame('CocaColases_na'
                        ,CocaColasesna_mape
                        ,row.names = "11")
names(InsertRow) <- names(CocaCola_ModDetails1)
CocaCola_ModDetails1 <-rbind(CocaCola_ModDetails1,InsertRow)
View(CocaCola_ModDetails1)

# Holts winter method 

CocaColaholt_nab<-holt(CocaColaTrain12,alpha = NULL,beta = NULL)
CocaColaholtnab_pred<-data.frame(predict(CocaColaholt_nab,h=4))
plot(forecast(CocaColaholt_nab,h=4))
CocaColaholtnab_mape<-sqrt(MSE(CocaColaTest12,CocaColaholtnab_pred$Point.Forecast))

InsertRow <- data.frame('CocaColaholt_nab'
                        ,CocaColaholtnab_mape
                        ,row.names = "12")
names(InsertRow) <- names(CocaCola_ModDetails1)
CocaCola_ModDetails1 <-rbind(CocaCola_ModDetails1,InsertRow)
View(CocaCola_ModDetails1)
# Holts winter Exponential method

CocaColahw_nabg_new<-hw(CocaColaTrain12,alpha=NULL,beta=NULL,gamma = NULL)
CocaColahwnabg_pred_new<-data.frame(predict(CocaColahw_nabg_new,h=4))
plot(forecast(CocaColahw_nabg_new,h=4))
CocaColahwnabg_mape_new<-sqrt(MSE(CocaColaTest12,CocaColahwnabg_pred_new$Point.Forecast))


InsertRow <- data.frame('CocaColahw_nabg_new'
                        ,CocaColahwnabg_mape_new
                        ,row.names = "13")
names(InsertRow) <- names(CocaCola_ModDetails1)
CocaCola_ModDetails1 <-rbind(CocaCola_ModDetails1,InsertRow)
View(CocaCola_ModDetails1)


# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 

sqldf("select ModelName,RMSE from CocaCola_ModDetails 
      union all
      select model,RMSE from CocaCola_ModDetails1
      order by RMSE limit 1")

CocaColaFinalModel<-HoltWinters(CocaCola10)

plot(forecast(CocaColaFinalModel,n.ahead=4))

# Forecasted values for the next 4 quarters
NewForecast <- data.frame(predict(CocaColaFinalModel,n.ahead=4))

###################################################################################################
# Airlines+Data.xlsx
###################################################################################################
##Loading .xlsx into R
## File name "Airlines+Data.xlsx"
Airlines <- read_xlsx(file.choose(),sheet="Sheet1" ,col_names = TRUE,na = c("NA","Na","nA","na",""))

Airlines1<-as.data.frame(Airlines)

plot(Airlines1$Passengers,type="o")

str(Airlines1)

Airlines1$Month<-as.factor(Airlines1$Month)


s<- dummy(Airlines1$Month,sep="")
s<-as.data.frame(s)
head(s)


Airlines1$Jan1995<-as.factor(s$`Month1995-01-01`)
Airlines1$Feb1995<-as.factor(s$`Month1995-02-01`)
Airlines1$Mar1995<-as.factor(s$`Month1995-03-01`)
Airlines1$Apr1995<-as.factor(s$`Month1995-04-01`)
Airlines1$May1995<-as.factor(s$`Month1995-05-01`)
Airlines1$Jun1995<-as.factor(s$`Month1995-06-01`)
Airlines1$Jul1995<-as.factor(s$`Month1995-07-01`)
Airlines1$Aug1995<-as.factor(s$`Month1995-08-01`)
Airlines1$Sep1995<-as.factor(s$`Month1995-09-01`)
Airlines1$Oct1995<-as.factor(s$`Month1995-10-01`)
Airlines1$Nov1995<-as.factor(s$`Month1995-11-01`)
Airlines1$Dec1995<-as.factor(s$`Month1995-12-01`)

Airlines1$Jan1996<-as.factor(s$`Month1996-01-01`)
Airlines1$Feb1996<-as.factor(s$`Month1996-02-01`)
Airlines1$Mar1996<-as.factor(s$`Month1996-03-01`)
Airlines1$Apr1996<-as.factor(s$`Month1996-04-01`)
Airlines1$May1996<-as.factor(s$`Month1996-05-01`)
Airlines1$Jun1996<-as.factor(s$`Month1996-06-01`)
Airlines1$Jul1996<-as.factor(s$`Month1996-07-01`)
Airlines1$Aug1996<-as.factor(s$`Month1996-08-01`)
Airlines1$Sep1996<-as.factor(s$`Month1996-09-01`)
Airlines1$Oct1996<-as.factor(s$`Month1996-10-01`)
Airlines1$Nov1996<-as.factor(s$`Month1996-11-01`)
Airlines1$Dec1996<-as.factor(s$`Month1996-12-01`)

Airlines1$Jan1997<-as.factor(s$`Month1997-01-01`)
Airlines1$Feb1997<-as.factor(s$`Month1997-02-01`)
Airlines1$Mar1997<-as.factor(s$`Month1997-03-01`)
Airlines1$Apr1997<-as.factor(s$`Month1997-04-01`)
Airlines1$May1997<-as.factor(s$`Month1997-05-01`)
Airlines1$Jun1997<-as.factor(s$`Month1997-06-01`)
Airlines1$Jul1997<-as.factor(s$`Month1997-07-01`)
Airlines1$Aug1997<-as.factor(s$`Month1997-08-01`)
Airlines1$Sep1997<-as.factor(s$`Month1997-09-01`)
Airlines1$Oct1997<-as.factor(s$`Month1997-10-01`)
Airlines1$Nov1997<-as.factor(s$`Month1997-11-01`)
Airlines1$Dec1997<-as.factor(s$`Month1997-12-01`)

Airlines1$Jan1998<-as.factor(s$`Month1998-01-01`)
Airlines1$Feb1998<-as.factor(s$`Month1998-02-01`)
Airlines1$Mar1998<-as.factor(s$`Month1998-03-01`)
Airlines1$Apr1998<-as.factor(s$`Month1998-04-01`)
Airlines1$May1998<-as.factor(s$`Month1998-05-01`)
Airlines1$Jun1998<-as.factor(s$`Month1998-06-01`)
Airlines1$Jul1998<-as.factor(s$`Month1998-07-01`)
Airlines1$Aug1998<-as.factor(s$`Month1998-08-01`)
Airlines1$Sep1998<-as.factor(s$`Month1998-09-01`)
Airlines1$Oct1998<-as.factor(s$`Month1998-10-01`)
Airlines1$Nov1998<-as.factor(s$`Month1998-11-01`)
Airlines1$Dec1998<-as.factor(s$`Month1998-12-01`)

Airlines1$Jan1999<-as.factor(s$`Month1999-01-01`)
Airlines1$Feb1999<-as.factor(s$`Month1999-02-01`)
Airlines1$Mar1999<-as.factor(s$`Month1999-03-01`)
Airlines1$Apr1999<-as.factor(s$`Month1999-04-01`)
Airlines1$May1999<-as.factor(s$`Month1999-05-01`)
Airlines1$Jun1999<-as.factor(s$`Month1999-06-01`)
Airlines1$Jul1999<-as.factor(s$`Month1999-07-01`)
Airlines1$Aug1999<-as.factor(s$`Month1999-08-01`)
Airlines1$Sep1999<-as.factor(s$`Month1999-09-01`)
Airlines1$Oct1999<-as.factor(s$`Month1999-10-01`)
Airlines1$Nov1999<-as.factor(s$`Month1999-11-01`)
Airlines1$Dec1999<-as.factor(s$`Month1999-12-01`)

Airlines1$Jan2000<-as.factor(s$`Month2000-01-01`)
Airlines1$Feb2000<-as.factor(s$`Month2000-02-01`)
Airlines1$Mar2000<-as.factor(s$`Month2000-03-01`)
Airlines1$Apr2000<-as.factor(s$`Month2000-04-01`)
Airlines1$May2000<-as.factor(s$`Month2000-05-01`)
Airlines1$Jun2000<-as.factor(s$`Month2000-06-01`)
Airlines1$Jul2000<-as.factor(s$`Month2000-07-01`)
Airlines1$Aug2000<-as.factor(s$`Month2000-08-01`)
Airlines1$Sep2000<-as.factor(s$`Month2000-09-01`)
Airlines1$Oct2000<-as.factor(s$`Month2000-10-01`)
Airlines1$Nov2000<-as.factor(s$`Month2000-11-01`)
Airlines1$Dec2000<-as.factor(s$`Month2000-12-01`)

Airlines1$Jan2001<-as.factor(s$`Month2001-01-01`)
Airlines1$Feb2001<-as.factor(s$`Month2001-02-01`)
Airlines1$Mar2001<-as.factor(s$`Month2001-03-01`)
Airlines1$Apr2001<-as.factor(s$`Month2001-04-01`)
Airlines1$May2001<-as.factor(s$`Month2001-05-01`)
Airlines1$Jun2001<-as.factor(s$`Month2001-06-01`)
Airlines1$Jul2001<-as.factor(s$`Month2001-07-01`)
Airlines1$Aug2001<-as.factor(s$`Month2001-08-01`)
Airlines1$Sep2001<-as.factor(s$`Month2001-09-01`)
Airlines1$Oct2001<-as.factor(s$`Month2001-10-01`)
Airlines1$Nov2001<-as.factor(s$`Month2001-11-01`)
Airlines1$Dec2001<-as.factor(s$`Month2001-12-01`)

Airlines1$Jan2002<-as.factor(s$`Month2002-01-01`)
Airlines1$Feb2002<-as.factor(s$`Month2002-02-01`)
Airlines1$Mar2002<-as.factor(s$`Month2002-03-01`)
Airlines1$Apr2002<-as.factor(s$`Month2002-04-01`)
Airlines1$May2002<-as.factor(s$`Month2002-05-01`)
Airlines1$Jun2002<-as.factor(s$`Month2002-06-01`)
Airlines1$Jul2002<-as.factor(s$`Month2002-07-01`)
Airlines1$Aug2002<-as.factor(s$`Month2002-08-01`)
Airlines1$Sep2002<-as.factor(s$`Month2002-09-01`)
Airlines1$Oct2002<-as.factor(s$`Month2002-10-01`)
Airlines1$Nov2002<-as.factor(s$`Month2002-11-01`)
Airlines1$Dec2002<-as.factor(s$`Month2002-12-01`)

Airlines1$t<-1:96
  
  Airlines1$tSq<-Airlines1$t*Airlines1$t

Airlines1$logPassengers<-log(Airlines1$Passengers)


AirlinesTrain<-Airlines1[1:84,]
AirlinesTest<-Airlines1[85:96,]
AirlinesTest<-sqldf("select * from AirlinesTest where t is not null")
#summary(CocaColaTrain)

####################################################################################
## Implementing Model Based Apporach
#####################################################################################


linmod<-lm(Passengers~t,data=AirlinesTrain)
linpred<-data.frame(predict(linmod,interval='predict',newdata =AirlinesTest))

rmse_linear<-sqrt(mean((AirlinesTest$Passengers-linpred$fit)^2,na.rm = TRUE))
rmse_linear # 815.2212

Airlines_ModDetails <- data.frame('linmod'
                                  ,'Linear Model'
                                  , 'Passengers'
                                  ,'t'
                                  ,rmse_linear
)
names(Airlines_ModDetails) <- c('ModelName','Model Description','Y','X','RMSE')


expmod<-lm(logPassengers~t,data=AirlinesTrain)
#summary(expmod)
exppred<-data.frame(predict(expmod,interval='predict',newdata =AirlinesTest))
exprmse<-sqrt(mean((AirlinesTest$Passengers-exp(exppred$fit))^2,na.rm = TRUE))
exprmse # 665.79

InsertRow <- data.frame('expmod'
                        ,'Exponential Model'
                        ,'logPassengers'
                        ,'t'
                        ,exprmse
                        ,row.names = "2")
names(InsertRow) <- names(Airlines_ModDetails)
Airlines_ModDetails <-rbind(Airlines_ModDetails,InsertRow)


Quadmod<-lm(Passengers~t+tSq,data=AirlinesTrain)
QuadPred<-data.frame(predict(Quadmod,interval='predict',newdata =AirlinesTest))
Quadrmse<-sqrt(mean((AirlinesTest$Passengers-QuadPred$fit)^2,na.rm=TRUE))
Quadrmse # 900.8873


InsertRow <- data.frame('Quadmod'
                        ,'Quadratic Model'
                        ,'Passengers'
                        ,'t+tSq'
                        ,Quadrmse
                        ,row.names = "3")
names(InsertRow) <- names(Airlines_ModDetails)
Airlines_ModDetails <-rbind(Airlines_ModDetails,InsertRow)

summary(AirlinesTrain)

Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001


SeasonalAddModel<-lm(Passengers~Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
                       Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
                       Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
                       Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
                       Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001,data=AirlinesTrain)
SeasonalAddpred<-data.frame(predict(SeasonalAddModel,newdata =AirlinesTest,interval='predict'))
SeasonalAddrmse<-sqrt(mean((AirlinesTest$Passengers-SeasonalAddpred$fit)^2,na.rm = TRUE))
SeasonalAddrmse # 2372.704


InsertRow <- data.frame('SeasonalAddModel'
                        ,'Additive Seasonal Model'
                        ,'Passengers'
                        ,'Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001'
                        ,SeasonalAddrmse
                        ,row.names = "4")
names(InsertRow) <- names(Airlines_ModDetails)
Airlines_ModDetails <-rbind(Airlines_ModDetails,InsertRow)


AdditiveSeasonalLinearModel<-lm(Passengers~t+Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
                                  Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
                                  Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
                                  Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
                                  Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001,data=AirlinesTrain)
AdditiveSeasonalLinearPred<-data.frame(predict(AdditiveSeasonalLinearModel,interval='predict',newdata =AirlinesTest))
AdditiveSeasonalLinearRmse<-sqrt(mean((AirlinesTest$Passengers-AdditiveSeasonalLinearPred$fit)^2,na.rm=TRUE))
AdditiveSeasonalLinearRmse # 777.0584


InsertRow <- data.frame('AdditiveSeasonalLinearModel'
                        ,'Additive Seasonality with Linear Model'
                        ,'Passengers'
                        ,'t+Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001'
                        ,AdditiveSeasonalLinearRmse
                        ,row.names = "5")
names(InsertRow) <- names(Airlines_ModDetails)
Airlines_ModDetails <-rbind(Airlines_ModDetails,InsertRow)



AdditiveSeasonalQuadraticModel<-lm(Passengers~t+tSq+Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
                                     Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
                                     Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
                                     Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
                                     Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001,data=AirlinesTrain)

AdditiveSeasonalQuadraticpred<-data.frame(predict(AdditiveSeasonalQuadraticModel,interval='predict',newdata =AirlinesTest))
AdditiveSeasonalQuadraticRmse<-sqrt(mean((AirlinesTest$Passengers-AdditiveSeasonalQuadraticpred$fit)^2,na.rm=TRUE))
AdditiveSeasonalQuadraticRmse # 376.7647


InsertRow <- data.frame('AdditiveSeasonalQuadraticModel'
                        ,'Additive Seasonality with Quadratic Model'
                        ,'Passengers'
                        ,'t+tSq+Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001'
                        ,AdditiveSeasonalQuadraticRmse
                        ,row.names = "6")
names(InsertRow) <- names(Airlines_ModDetails)
Airlines_ModDetails <-rbind(Airlines_ModDetails,InsertRow)
View(Airlines_ModDetails)


MultiplicativeSeasonalityModel<-lm(logPassengers~ Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
                                     Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
                                     Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
                                     Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
                                     Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001,data = AirlinesTrain)
MultiplicativeSeasonalityPred<-data.frame(predict(MultiplicativeSeasonalityModel,newdata =AirlinesTest,interval='predict'))
MultiplicativeSeasonalityPredRmse<-sqrt(mean((AirlinesTest$Passengers-exp(MultiplicativeSeasonalityPred$fit))^2,na.rm = TRUE))
MultiplicativeSeasonalityPredRmse # 2372.704


InsertRow <- data.frame('MultiplicativeSeasonalityModel'
                        ,'Multiplicative Seasonality Model'
                        ,'logPassengers'
                        ,'Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001'
                        ,MultiplicativeSeasonalityPredRmse
                        ,row.names = "7")
names(InsertRow) <- names(Airlines_ModDetails)
Airlines_ModDetails <-rbind(Airlines_ModDetails,InsertRow)


MultiplicativeSeasonalityLinearModel<-lm(logPassengers~t+Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
                                           Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
                                           Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
                                           Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
                                           Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001,data = AirlinesTrain)
MultiplicativeSeasonalityLinearPred<-data.frame(predict(MultiplicativeSeasonalityLinearModel,newdata =AirlinesTest,interval='predict'))
MultiplicativeSeasonalityLinearModelRmse<-sqrt(mean((AirlinesTest$Passengers-exp(MultiplicativeSeasonalityLinearPred$fit))^2,na.rm = TRUE))
MultiplicativeSeasonalityLinearModelRmse # 547.5685


InsertRow <- data.frame('MultiplicativeSeasonalityLinearModel'
                        ,'Multiplicative Seasonality Linear Model'
                        ,'logPassengers'
                        ,'t+Jan1995+Feb1995+Mar1995+Apr1995+May1995+Jun1995+Jul1995+Aug1995+Sep1995+Oct1995+Nov1995+Dec1995+Jan1996+Feb1996+Mar1996+
Apr1996+May1996+Jun1996+Jul1996+Aug1996+Sep1996+Oct1996+Nov1996+Dec1996+Jan1997+Feb1997+Mar1997+Apr1997+May1997+Jun1997+Jul1997+Aug1997+Sep1997+Oct1997+
Nov1997+Dec1997+Jan1998+Feb1998+Mar1998+Apr1998+May1998+Jun1998+Jul1998+Aug1998+Sep1998+Oct1998+Nov1998+Dec1998+Jan1999+Feb1999+Mar1999+Apr1999+May1999+
Jun1999+Jul1999+Aug1999+Sep1999+Oct1999+Nov1999+Dec1999+Jan2000+Feb2000+Mar2000+Apr2000+May2000+Jun2000+Jul2000+Aug2000+Sep2000+Oct2000+Nov2000+Dec2000+
Jan2001+Feb2001+Mar2001+Apr2001+May2001+Jun2001+Jul2001+Aug2001+Sep2001+Oct2001+Nov2001+Dec2001'
                        ,MultiplicativeSeasonalityLinearModelRmse
                        ,row.names = "8")
names(InsertRow) <- names(Airlines_ModDetails)
Airlines_ModDetails <-rbind(Airlines_ModDetails,InsertRow)
View(Airlines_ModDetails)

sqldf("select ModelName,`Model Description` from Airlines_ModDetails order by Rmse limit 1")




########################################################################################################################
## Applying data based models
########################################################################################################################

##Loading .xlsx into R
## File name "Airlines_Passengers_Rawdata.xlsx"
Airlines11 <- read_xlsx(file.choose(),sheet="Sheet1" ,col_names = TRUE,na = c("NA","Na","nA","na",""))

Airlines10<-ts(Airlines11$Passengers,frequency = 12,start=c(96))

AirlinesTrain11<-Airlines10[1:84]
AirlinesTest11<-Airlines10[85:96]

# converting time series object
AirlinesTrain12<-ts(AirlinesTrain11,frequency = 12)
AirlinesTest12<-ts(AirlinesTest11,frequency = 12)

plot(Airlines10)



##Applying ARIMA
AirlinesArimaM<-Arima(AirlinesTrain12,order = c(0,0,12))
Forcas<-forecast(AirlinesArimaM,h=12)
Accu<-accuracy(Forcas,x=as.numeric(AirlinesTest12))
Accu1<-as.data.frame(Accu)
Accu1[2,2]
plot(Forcas)


AirlinesHWMa<-HoltWinters(AirlinesTrain12,alpha = 0.2,beta = F,gamma = F)
AirlinesHWapred<-data.frame(predict(AirlinesHWMa,n.ahead=12))
plot(forecast(AirlinesHWMa,h=12))
AirlinesHWMaRmse<-sqrt(MSE(AirlinesTest12, AirlinesHWapred$fit))



AirlinesHWMab<-HoltWinters(AirlinesTrain12,alpha = 0.2,beta = 0.1,gamma = F)
AirlinesHWMabpred<-data.frame(predict(AirlinesHWMab,n.ahead = 12))
plot(forecast(AirlinesHWMab,h=12))
AirlinesHWMabRmse<-sqrt(MSE(AirlinesTest12, AirlinesHWMabpred$fit))


AirlinesHWMabg<-HoltWinters(AirlinesTrain12,alpha = 0.2,beta = 0.1,gamma = 0.1)
AirlinesHWMabgpred<-data.frame(predict(AirlinesHWMabg,n.ahead = 12))
plot(forecast(AirlinesHWMabg,h=12))
AirlinesHWMabgRmse<-sqrt(MSE(AirlinesTest12, AirlinesHWMabgpred$fit))


AirlinesHWMna<-HoltWinters(AirlinesTrain12,beta = F,gamma = F)
AirlinesHWMnapred<-data.frame(predict(AirlinesHWMna,n.ahead = 12))
plot(forecast(AirlinesHWMna,h=12))
AirlinesHWMnaRMSE<-sqrt(MSE(AirlinesTest12, AirlinesHWMnapred$fit))

AirlinesHWMnab<-HoltWinters(AirlinesTrain12,gamma=F)
AirlinesHWMpred<-data.frame(predict(AirlinesHWMnab,n.ahead=12))
plot(forecast(AirlinesHWMnab,h=12))
AirlinesHWMnabRMSE<-sqrt(MSE(AirlinesTest12, AirlinesHWMpred$fit))

AirlinesHWMnabg<-HoltWinters(AirlinesTrain12)
AirlinesHWMpred<-data.frame(predict(AirlinesHWMnabg,n.ahead =12))
plot(forecast(AirlinesHWMnabg,h=12))
AirlinesHWMnabgRMSE<-sqrt(MSE(AirlinesTest12, AirlinesHWMpred$fit))

Airlines_ModDetails1<-data.frame(c("AirlinesArimaM","AirlinesHWMa","AirlinesHWMab","AirlinesHWMabg","AirlinesHWMna","AirlinesHWMnab","AirlinesHWMnabg")
                                 ,c(Accu1[2,5],AirlinesHWMaRmse,AirlinesHWMabRmse,AirlinesHWMabgRmse,AirlinesHWMnaRMSE,AirlinesHWMnabRMSE,AirlinesHWMnabgRMSE))

colnames(Airlines_ModDetails1)<-c("Model","RMSE")
View(Airlines_ModDetails1)

########################################################################################


Airlinesses_a<-ses(AirlinesTrain12,alpha = 0.2,h=12) # 
AirlinessesaPred<-data.frame(predict(Airlinesses_a,h=12))
plot(forecast(Airlinesses_a,n.ahead=12))
AirlinessesaRMSE<-sqrt(MSE(AirlinesTest12, AirlinessesaPred$Point.Forecast))

InsertRow <- data.frame('Airlinesses_a'
                        ,AirlinessesaRMSE
                        ,row.names = "8")
names(InsertRow) <- names(Airlines_ModDetails1)
Airlines_ModDetails1 <-rbind(Airlines_ModDetails1,InsertRow)
View(Airlines_ModDetails1)


# with alpha = 0.2, beta = 0.1

Airlinesholt_ab<-holt(AirlinesTrain12,alpha = 0.2,beta = 0.1,h=12)
AirlinesholtabPred<-data.frame(predict(Airlinesholt_ab,h=12))
plot(forecast(Airlinesholt_ab,h=12))
AirlinesholtabRMSE<-sqrt(MSE(AirlinesTest12, AirlinesholtabPred$Point.Forecast))

InsertRow <- data.frame('Airlinesholt_ab'
                        ,AirlinesholtabPred
                        ,row.names = "9")
names(InsertRow) <- names(Airlines_ModDetails1)
Airlines_ModDetails1 <-rbind(Airlines_ModDetails1,InsertRow)
View(Airlines_ModDetails1)

# with alpha = 0.2, beta = 0.1, gamma = 0.1 

Airlineshw_abg_new<-hw(AirlinesTrain12,alpha = 0.2,beta = 0.1,gamma = 0.1,h=12)
Airlineshwabg_pred_new<-data.frame(predict(Airlineshw_abg_new,h = 12))
plot(forecast(Airlineshw_abg_new,h=12))
Airlineshwabg_RMSE_new<-sqrt(MSE(AirlinesTest12, Airlineshwabg_pred_new$Point.Forecast))

InsertRow <- data.frame('Airlineshw_abg_new'
                        ,Airlineshwabg_RMSE_new
                        ,row.names = "10")
names(InsertRow) <- names(Airlines_ModDetails1)
Airlines_ModDetails1 <-rbind(Airlines_ModDetails1,InsertRow)
View(Airlines_ModDetails1)

# With out optimum values 

# simple exponential method

Airlinesses_na<-ses(AirlinesTrain12,alpha=NULL,h=12)
Airlinessesna_pred<-data.frame(predict(Airlinesses_na,h = 12))
plot(forecast(Airlinesses_na,h=12))
AirlinessesnaRMSE<-sqrt(MSE(AirlinesTest12, Airlinessesna_pred$Point.Forecast))

InsertRow <- data.frame('Airlinesses_na'
                        ,AirlinessesnaRMSE
                        ,row.names = "11")
names(InsertRow) <- names(Airlines_ModDetails1)
Airlines_ModDetails1 <-rbind(Airlines_ModDetails1,InsertRow)
View(Airlines_ModDetails1)

# Holts winter method 

Airlinesholt_nab<-holt(AirlinesTrain12,alpha = NULL,beta = NULL,h=12)
Airlinesholtnab_pred<-data.frame(predict(Airlinesholt_nab,h=12))
plot(forecast(Airlinesholt_nab,h=12))
AirlinesholtnabRMSE<-sqrt(MSE(AirlinesTest12, Airlinesholtnab_pred$Point.Forecast))

InsertRow <- data.frame('Airlinesholt_nab'
                        ,AirlinesholtnabRMSE
                        ,row.names = "12")
names(InsertRow) <- names(Airlines_ModDetails1)
Airlines_ModDetails1 <-rbind(Airlines_ModDetails1,InsertRow)
View(Airlines_ModDetails1)
# Holts winter Exponential method

Airlineshw_nabg_new<-hw(AirlinesTrain12,alpha=NULL,beta=NULL,gamma = NULL,h=12)
Airlineshwnabg_pred_new<-data.frame(predict(Airlineshw_nabg_new,h=12))
plot(forecast(Airlineshw_nabg_new,h=12))
Airlineshwnabg_RMSE_new<-sqrt(MSE(AirlinesTest12, Airlineshwnabg_pred_new$Point.Forecast))


InsertRow <- data.frame('Airlineshw_nabg_new'
                        ,Airlineshwnabg_RMSE_new
                        ,row.names = "13")
names(InsertRow) <- names(Airlines_ModDetails1)
Airlines_ModDetails1 <-rbind(Airlines_ModDetails1,InsertRow)
View(Airlines_ModDetails1)


# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 

sqldf("select ModelName,RMSE from Airlines_ModDetails 
      union all
      select model,RMSE from Airlines_ModDetails1
      order by RMSE limit 1")

AirlinesFinalModel<-HoltWinters(Airlines10,alpha=0.2294643,beta=0.04451169,gamma=1)

plot(forecast(AirlinesFinalModel,n.ahead=12))

# Forecasted values for the next 12 quarters
NewForecast <- data.frame(predict(AirlinesFinalModel,n.ahead=12))
