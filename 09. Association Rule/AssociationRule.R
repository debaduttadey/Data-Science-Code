#########################################
# File Name:-Assignment9.R
# Subject:- Association Rule Mining
# Author:- Debadutta Dey
#########################################

install.packages("arulesViz")
install.packages("arules")

update.packages("colorspace")

library(arules)
library(arulesViz)
library(colorspace)
library(sqldf)

##library(colorspace)

detach(package:colorspace, unload=TRUE)

###################################################################################################
# book.csv
###################################################################################################
##Loading .csv into R
## File name "book.csv"

books<-read.csv(file.choose())
View(books)
str(books)

books1<-data.frame(sapply(books,as.factor))
books1 <- as(books1, "transactions")

summary(books1)

barplot(sapply(books,sum),col=1:10)


rules <- apriori(books1,parameter = list(support=0.5,confidence=0.9,minlen=6))

rules <- rules[!is.redundant(rules)]

inspect(rules)
plot(rules,control=list(choose_palette()),cex=1)

subrules_lift1.05 <- subset(rules, lift>1.05)
subrules_lift1.05


?subset

plot(subrules_lift1.05, method="matrix", measure="lift")
plot(subrules_lift1.05, method="matrix", measure="lift", control=list(reorder=TRUE))

plot(subrules_lift1.05, measure=c("lift", "confidence"), 
     control=list(reorder=TRUE))

## only finding for lower books count shown in bar plot
rules2 <- apriori(books1,parameter = list(support=0.1,confidence=0.1,minlen=2))
inspect(rules2)
plot(rules2,control=list(choose_palette(pal = sequential_hcl(5,h = 260, c. = c(580, 0), l = c(9930, 9990), power = 2,
                                                            fixup = TRUE, alpha = 1))),cex=1)
 
subrules2<-subset(rules2, subset=lhs %pin% c("RefBks=1","ItalCook=1","ItalAtlas=1","ItalArt=1",'Florence=1'))
plot(subrules2, measure=c("lift", "confidence"), 
     control=list(reorder=TRUE))
subrules2<-subset(subrules2,lift>1.2)
plot(subrules2, measure=c("lift", "confidence"), 
     control=list(reorder=TRUE))
plot(subrules2, method="graph")



##############################################################################################################################

rules_6 <- apriori(as.matrix(books),parameter = list(support=0.02,confidence=0.9,minlen=6))
inspect(rules_6)


?sequential_hcl

rules_6_supp <- sort(rules_6,by="support",decreasing = TRUE)
inspect(rules_6_supp)
rules_6_conf <- sort(rules_6,by="confidence",decreasing = TRUE)
inspect(rules_6_conf)

rules_6_lift <- sort(rules_6,by="lift",decreasing = TRUE)
inspect(rules_6_lift)

plot(rules_6,method = "graph")

###################################################################################################
# my_movies.csv
###################################################################################################
##Loading .csv into R
## File name "my_movies.csv"

movies<-read.csv(file.choose(),col.names = c("v1","v2","v3","v4","v5","SixthSense","Gladiator","LOTR1","HarryPotter1","Patriot","LOTR2","HarryPotter2","LOTR","Braveheart","GreenMile"))

sqldf("select * from movies")

movies<-movies[,-c(1,2,3,4,5)]

sqldf("select * from movies where SixthSense=0 and Gladiator=0 and LOTR1=0 and HarryPotter1=0 and LOTR2=0 and HarryPotter2=0 and LOTR=0 and Braveheart=0 and GreenMile=0")


str(movies)

movies1<-data.frame(sapply(movies,as.factor))
movies1 <- as(movies1, "transactions")

str(movies1)

barplot(sapply(movies,sum),col=1:10)

summary(movies1)

############################################################################################################

Mrules <- apriori(movies1,parameter = list(support=0.4,confidence=0.9,minlen=10))
inspect(Mrules)

plot(Mrules,method = "graph")

Mrules_Lift1.5<- subset(Mrules, lift>1.5)

inspect(Mrules_Lift1.5)
plot(Mrules_Lift1.5,method = "graph")
plot(Mrule_Patriot, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################

Mrules <- apriori(movies1,parameter = list(support=0.4,confidence=0.5,minlen=2))

Mrules <- Mrules[!is.redundant(Mrules)]

inspect(subset(Mrules, subset=lhs %in% c("SixthSense=1","Gladiator=1","LOTR1=1","HarryPotter1=1","Patriot=1","LOTR2=1","HarryPotter2=1","LOTR=1","Braveheart=1","GreenMile=1")))
Mrules_Watch<-subset(Mrules, subset=rhs %in% c("SixthSense=1","Gladiator=1","LOTR1=1","HarryPotter1=1","Patriot=1","LOTR2=1","HarryPotter2=1","LOTR=1","Braveheart=1","GreenMile=1"),by="lift",decreasing = TRUE)

inspect(Mrules_Watch)
Mrule_Patriot<-subset(Mrules_Watch,subset=rhs %in% "Patriot=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_Patriot)

plot(Mrule_Patriot,control=list(choose_palette()),cex=1)
plot(Mrule_Patriot,method = "graph")

plot(Mrule_Patriot, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################
Mrule_Gladiator<-subset(Mrules_Watch,subset=rhs %in% "Gladiator=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_Gladiator)

plot(Mrule_Gladiator,control=list(choose_palette()),cex=1)
plot(Mrule_Gladiator,method = "graph")

plot(Mrule_Gladiator, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################
Mrules <- apriori(movies1,parameter = list(support=0.01,confidence=0.001,minlen=2))

Mrules <- Mrules[!is.redundant(Mrules)]
inspect(Mrules)

Mrule_LOTR1<-subset(Mrules,subset=rhs %in% "LOTR1=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_LOTR1)

plot(Mrule_LOTR1,control=list(choose_palette()),cex=1)
plot(Mrule_LOTR1,method = "graph")

plot(Mrule_LOTR1, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################
Mrule_HarryPotter1<-subset(Mrules,subset=rhs %in% "HarryPotter1=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_HarryPotter1)

plot(Mrule_HarryPotter1,control=list(choose_palette()),cex=1)
plot(Mrule_HarryPotter1,method = "graph")

plot(Mrule_HarryPotter1, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################
Mrule_LOTR2<-subset(Mrules,subset=rhs %in% "LOTR2=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_LOTR2)

plot(Mrule_LOTR2,control=list(choose_palette()),cex=1)
plot(Mrule_LOTR2,method = "graph")

plot(Mrule_LOTR2, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################
Mrule_HarryPotter2<-subset(Mrules,subset=rhs %in% "HarryPotter2=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_HarryPotter2)

plot(Mrule_HarryPotter2,control=list(choose_palette()),cex=1)
plot(Mrule_HarryPotter2,method = "graph")

plot(Mrule_HarryPotter2, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################
Mrule_LOTR<-subset(Mrules,subset=rhs %in% "LOTR=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_LOTR)

plot(Mrule_LOTR,control=list(choose_palette()),cex=1)
plot(Mrule_LOTR,method = "graph")

plot(Mrule_LOTR, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################
Mrule_Braveheart<-subset(Mrules,subset=rhs %in% "Braveheart=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_Braveheart)

plot(Mrule_Braveheart,control=list(choose_palette()),cex=1)
plot(Mrule_Braveheart,method = "graph")

plot(Mrule_Braveheart, method="paracoord", control=list(reorder=TRUE))



###########################################################################################################
Mrule_GreenMile<-subset(Mrules,subset=rhs %in% "GreenMile=1",by=c("support","confidence","lift"),decreasing = TRUE)
inspect(Mrule_GreenMile)

plot(Mrule_GreenMile,control=list(choose_palette()),cex=1)
plot(Mrule_GreenMile,method = "graph")

plot(Mrule_GreenMile, method="paracoord", control=list(reorder=TRUE))

###########################################################################################################


###################################################################################################
# groceries.csv
###################################################################################################
##Loading .csv into R
## File name "groceries.csv"

groceries <- read.transactions(file.choose(), format = "basket", sep = ",")
inspect(groceries[1:20])

summary(groceries)


Grules <- apriori(groceries,parameter = list(support=0.02,confidence=0.4,minlen=2))
inspect(Grules)

plot(Grules,control=list(choose_palette()),cex=1)
plot(Grules,method = "graph")

plot(Grules, method="paracoord", control=list(reorder=TRUE))


Grules <- apriori(groceries,parameter = list(support=0.01,confidence=0.1,minlen=2))
inspect(Grules)

Grules_lift <- sort(Grules,by="lift")
inspect(Grules_lift)

plot(Grules_lift,control=list(choose_palette()),cex=1)
plot(Grules_lift,method = "graph")
