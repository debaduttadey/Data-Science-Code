#########################################
# File Name:-Assignment8.R
# Subject:- PCA
# Author:- Debadutta Dey
#########################################

install.packages("gdata")

library(XML)
library(gdata)
library(sqldf)
###################################################################################################
# wine.csv
###################################################################################################
##Loading .csv into R
## File name "wine.csv"
Wine <- read.csv(file.choose(),header = TRUE,sep = ",",na.strings = c("NA","Na","nA","na"))

sum(is.na(Wine))

str(Wine)

View(Wine)

Wine1<-Wine[,-1]

WinePCA<-princomp(Wine1,cor=TRUE,scores = TRUE,covmat = NULL)

str(WinePCA)

summary(WinePCA)

View(WinePCA)

WinePCA1<-data.frame(cbind(WinePCA$scores[,1],WinePCA$scores[,2],WinePCA$scores[,3]))


View(WinePCA1)


#######################################################################################################
## Hierarical Clustering
######################################################################################################


WineDist<-dist(WinePCA1,method = "euclidean")
WineFit<-hclust(WineDist,method="complete")
str(WineFit)

plot(WineFit, hang=-1)

clusterno <-3

rect.hclust(WineFit, k=clusterno, border="red")
WineHClusters <- cutree(WineFit, k=clusterno)


Winememb<-as.matrix(WineHClusters) 

Wine3<-Wine
Winefinal <- data.frame(Wine3, Winememb)

WineAggHist<-aggregate(Wine3[,-1],by=list(Winefinal$Winememb),mean,na.action=na.omit)

sqldf("select * from WineAggHist order by alcohol")


###################################################################
## K-means Clustering
####################################################################

kmfit<-kmeans(WinePCA1, 5)

Wine3<-Wine
kmfinal<-data.frame(WinePCA1,kmfit$cluster)
kmfinal

KMFinal1<-kmfinal[,c(ncol(kmfinal),1:(ncol(kmfinal)-1))]

aggK<-aggregate(WinePCA1,by=list(kmfit$cluster),mean)

sqldf("select * from aggK order by Alcohol asc")
registerDoParallel(cores = 5)

kselection(WinePCA1,parallel= TRUE,k_threshold = 0.9,max_centers = 12)

wss<-NULL
for(i in 2:14)
  wss[i]<- sum(kmeans(WinePCA1,centers=i)$withinss)

plot(1:14, wss, type = "b",
     xlab = "Number of clusters",
     ylab = "Withiness sum of squares")

title(sub="K means clustering for crime data")


kmfit<-kmeans(WinePCA1, 3)

Wine3<-Wine
kmfinal<-data.frame(Wine3,kmfit$cluster)
kmfinal

KMFinal1<-kmfinal[,c(ncol(kmfinal),1:(ncol(kmfinal)-1))]

aggK<-aggregate(Wine3[,-1],by=list(kmfit$cluster),mean)

sqldf("select * from aggK order by Alcohol desc")


#####################################################################
## K means Clustering with original data
####################################################################

WineNorm <-scale(Wine1)

kmfit<-kmeans(WineNorm, 5)


kmfinal<-data.frame(WineNorm,kmfit$cluster)
kmfinal

KMFinal1<-kmfinal[,c(ncol(kmfinal),1:(ncol(kmfinal)-1))]

aggK<-aggregate(Wine1,by=list(kmfit$cluster),mean)

sqldf("select * from aggK order by Alcohol asc")

wss<-NULL
for(i in 2:14)
  wss[i]<- sum(kmeans(WineNorm,centers=i)$withinss)

plot(1:14, wss, type = "b",
     xlab = "Number of clusters",
     ylab = "Withiness sum of squares")

title(sub="K means clustering for crime data")


kmfit<-kmeans(WineNorm, 3)

Wine3<-Wine
kmfinal<-data.frame(Wine3,kmfit$cluster)
kmfinal

KMFinal1<-kmfinal[,c(ncol(kmfinal),1:(ncol(kmfinal)-1))]

aggK<-aggregate(Wine3[,-1],by=list(kmfit$cluster),mean)

sqldf("select * from aggK order by Alcohol desc")

## Yes we have got 3 cluster with original data