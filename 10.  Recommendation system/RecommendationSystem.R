#########################################
# File Name:-Assignment10.R
# Subject:- Recommendation system
# Author:- Debadutta Dey
#########################################

install.packages("recommender lab",dependencies = TRUE)
install.packages("Matrix")
install.packages("qdapTools")
install.packages("Amelia")

library(XML)
library(Matrix)
library(recommenderlab)
library(caTools)
library(qdapTools)
library(sqldf)
library(dummies)
library(Amelia)

anime<-read.csv(file.choose(),header = TRUE,sep = ',',na.strings = c("NA","nA","Na","na",""))
View(anime)

sqldf("select count(*) from (select distinct anime_id from anime)x")

str(anime)


sum(is.na(anime))

summary(anime)

##Remving animeid column
##anime<-anime[,-1]

View(anime)


## genre, type and rating has na values. Genre and type ahave 82 recods of na value and hence removing them and rating has outliers and hence imputing by median
boxplot(anime$rating,horizontal = TRUE)

median(anime$rating,na.rm = TRUE)

anime<-sqldf(c("update anime set rating=6.570 where rating is null","select * from anime"))
sum(is.na(anime$rating))

anime$genre<- as.factor(anime$genre)
anime$type<- as.factor(anime$type)

anime<-sqldf(c("delete from anime where genre is null","select * from anime"))


anime<-sqldf(c("delete from anime where type is null","select * from anime"))

anime$type<- as.factor(anime$type)

str(anime)
summary(anime)

anime1<-anime


anime1<-cbind(anime1, mtabulate(strsplit(as.character(anime$genre), ", ")))

anime1<-anime1[,-3]
str(anime1)

anime2<-lapply(anime1, 
       as.factor)
str(anime2)

anime2$rating <-anime1$rating
anime2$members <-anime1$members


hist(anime2$rating)




Anime3_Ratingmatrix <- as(anime3, 'realRatingMatrix')
str(Anime3_Ratingmatrix)


#Popularity based 

Anime3_popular_model <- Recommender(Anime3_Ratingmatrix, method="POPULAR")
str(Anime3_popular_model)
a<-getModel(Anime3_popular_model)
a$data

recommended_PopularAnime3 <- predict(Anime3_popular_model, Anime3_Ratingmatrix[1], n=10,type=c("topNList"))
a<-as(recommended_PopularAnime3, "list")
Name<-as.data.frame(a)
Score<-as.data.frame(recommended_PopularAnime3@ratings)
Final_by_Popularity<-cbind(Name,Score)
names(Final_by_Popularity) <- c('AnimeName','Score')
View(Final_by_Popularity)




