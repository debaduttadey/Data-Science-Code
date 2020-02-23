#########################################
# File Name:-Assignment7.R
# Subject:- Clustering
# Author:- Debadutta Dey
#########################################

install.packages('animation')
install.packages('kselection')
install.packages('doParallel')

# Importing required packages
library(readxl)
library(sqldf)
library(dummies)
library(cluster)
library(ggplot2)
library(Rtsne)
library(dplyr)
library(animation)
library(kselection)
library(doParallel)


###################################################################################################
# crime_data.csv
###################################################################################################
##Loading .csv into R
## File name "crime_data.csv"
CrimeData <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na"))

sum(is.na(CrimeData))
summary(CrimeData)
View(CrimeData)

str(CrimeData)

## removing first column

CrimeData1<- CrimeData[,-1]
str(CrimeData1)

CrimeDatanorm<-scale(CrimeData1)
View(CrimeDatanorm)

###################################################################
## Hieraical Clustering
####################################################################

CrimeDist<-dist(CrimeDatanorm,method = "euclidean")
CrimeFit<-hclust(CrimeDist,method="complete")
str(CrimeFit)
plot(CrimeFit, hang=-1)

clusterno <-5

rect.hclust(CrimeFit, k=clusterno, border="red")
CrimeHClusters <- cutree(CrimeFit, k=clusterno)


memb<-as.matrix(CrimeHClusters) 

CrimeData2<-CrimeData
final <- data.frame(CrimeData2, memb)

CrimeAggHist<-aggregate(CrimeData2[,-1],by=list(final$memb),mean,na.action=na.omit)

?aggregate


sqldf("select * from CrimeAggHist")


###################################################################
## K-means Clustering
####################################################################

kmfit<-kmeans(CrimeDatanorm, 5)

CrimeData2<-CrimeData
kmfinal<-data.frame(CrimeData2,kmfit$cluster)
kmfinal

KMFinal1<-kmfinal[,c(ncol(kmfinal),1:(ncol(kmfinal)-1))]

aggK<-aggregate(CrimeData2[,-1],by=list(kmfit$cluster),mean)

sqldf("select * from aggK order by Murder asc")
registerDoParallel(cores = 5)

kselection(CrimeDatanorm,parallel= TRUE,k_threshold = 0.9,max_centers = 12)

wss<-NULL
for(i in 2:14)
  wss[i]<- sum(kmeans(CrimeDatanorm,centers=i)$withinss)

plot(1:14, wss, type = "b",
     xlab = "Number of clusters",
     ylab = "Withiness sum of squares")

title(sub="K means clustering for crime data")


kmfit<-kmeans(CrimeDatanorm, 4)

CrimeData2<-CrimeData
kmfinal<-data.frame(CrimeData2,kmfit$cluster)
kmfinal

KMFinal1<-kmfinal[,c(ncol(kmfinal),1:(ncol(kmfinal)-1))]

aggK<-aggregate(CrimeData2[,-1],by=list(kmfit$cluster),mean)

sqldf("select * from aggK order by Murder asc")


###################################################################################################
# EastWestAirlines.xlsx
###################################################################################################
##Loading .csv into R
## File name "EastWestAirlines.xlsx"
AirlinesData <- read_excel(file.choose(),sheet = "data")


View(AirlinesData)

sum(is.na(AirlinesData))

str(AirlinesData)

AirlinesData <-as.data.frame(AirlinesData[,-1])

summary(AirlinesData)

AirlinesData$Award <- AirlinesData$`Award?`

AirlinesData <-as.data.frame(AirlinesData[,-11])

AirlinesData1<- AirlinesData

sqldf("select distinct cc1_miles from AirlinesData")
sqldf("select distinct cc2_miles from AirlinesData")
sqldf("select distinct cc3_miles from AirlinesData")
sqldf("select distinct Award from AirlinesData")

AirlinesData1$cc1_miles <-as.factor(AirlinesData1$cc1_miles)
AirlinesData1$cc2_miles <-as.factor(AirlinesData1$cc2_miles)
AirlinesData1$cc3_miles <-as.factor(AirlinesData1$cc3_miles)
AirlinesData1$Award <-as.factor(AirlinesData1$Award)


summary(AirlinesData1)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

AirlinesData1$Balance<-normalize(AirlinesData1$Balance)
AirlinesData1$Qual_miles<-normalize(AirlinesData1$Qual_miles)
AirlinesData1$Bonus_miles<-normalize(AirlinesData1$Bonus_miles)
AirlinesData1$Bonus_trans<-normalize(AirlinesData1$Bonus_trans)
AirlinesData1$Flight_miles_12mo<-normalize(AirlinesData1$Flight_miles_12mo)
AirlinesData1$Flight_trans_12<-normalize(AirlinesData1$Flight_trans_12)
AirlinesData1$Days_since_enroll<-normalize(AirlinesData1$Days_since_enroll)

str(AirlinesData1)

###################################################################
## Hieraical Clustering
####################################################################

set.seed(1446)

dist <- daisy(AirlinesData1, metric = "gower")
mat <- as.matrix(dist)

fitHist<-hclust(dist)


plot(fitHist, hang=-1)

cluscount <-2


rect.hclust(fitHist, k=cluscount, border="red")

groupHist <- cutree(fitHist, k=cluscount)

AirlinesData2<-AirlinesData



membershipHist<-as.matrix(groupHist) 
finalHist <- data.frame(AirlinesData2, membershipHist)

aggHist<-aggregate(AirlinesData2,by=list(finalHist$membershipHist),mean)


sqldf("select * from aggHist order by balance asc")

###################################################################
## kmetorid Clustering
####################################################################

set.seed(1447)

dist <- daisy(AirlinesData1, metric = "gower")
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

k <- 10
pam_fit <- pam(dist, diss = TRUE, k)
pam_results <- AirlinesData %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

AirlinesData3<-cbind(AirlinesData,pam_fit$clustering)
aggHist<-aggregate(AirlinesData3,list(AirlinesData3$pam),mean)

sqldf("select * from aggHist order by balance asc")

k <- 2
pam_fit <- pam(dist, diss = TRUE, k)
pam_results <- AirlinesData %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

AirlinesData3<-cbind(AirlinesData,pam_fit$clustering)
aggHist<-aggregate(AirlinesData3,list(AirlinesData3$pam),mean)

sqldf("select * from aggHist order by balance asc")


tsne_obj <- Rtsne(dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


