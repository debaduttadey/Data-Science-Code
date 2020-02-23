#########################################
# File Name:-Assignment17.R
# Subject:- Text Mining
# Author:- Debadutta Dey
#########################################

install.packages("rvest")
install.packages("rJava")
install.packages("tm")
install.packages("Rwek")
install.packages("textir")
install.packages("maptpx")
install.packages("data.table")
install.packages("slam")
install.packages("ggplot2")

library(rvest)
library(XML)
library(magrittr)

library(rJava)
library(tm)
library(SnowballC)
library(Rwek)
library(qdap)
library(textir)
library(maptpx)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)
library(sqldf)
library(wordcloud)
library(quanteda)
library(Matrix)




makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 

# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negatice wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names[1:120],freq_neg[1:120],scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}



words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}


# --- func to make cluster dendograms --- #
clusdend = function(a){	# writing func clusdend() 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  # making dissimilarity matrix out of the freq one
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  # now plot collocation dendogram
  d <- dist(test2, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward")
  windows()
  plot(fit) # display dendogram
} # clusdend() func ends

####################################################################
## Quetion 1 :- Twitter Analysis on  "realdonaldtrumph" 
####################################################################


# Load Requried Packages
library("SnowballC")
library("tm")
library("syuzhet")

library("twitteR")
library("ROAuth")

consumer_key <- "Enter consumer_key"
consumer_secret <- "Enter consumer_secret"
access_token <- "Enter access_token"
access_secret <- "Enter access_secret"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


tw = twitteR::searchTwitter('#HowdyMody', n = 1e4, retryOnRateLimit = 1e3)
d = twitteR::twListToDF(tw)

write.csv(d,"Tweetstrump.csv",row.names = F)

pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos.words,"wow", "kudos", "hurray","wealthy") 			# including our own positive words to the existing list
neg.words = c(neg.words,'chor','bhaktards','ghatya')
stopwdrds = readLines(file.choose())

realdonald<-read.csv(file.choose(),header=TRUE,na.strings=c("","NA","na","Na","nA")) 
realdonald[1:10]

sqldf("select text from realdonald limit 10")
str(realdonald)

## Building Corpus

realdonaldCorpus<-iconv(realdonald$text)
realdonaldCorpus<-Corpus(VectorSource(realdonaldCorpus))
inspect(realdonaldCorpus[1:5])
?tm_map

## Clean text
removeRT<-function(x) gsub('(RT|via)((?:\\b\\w*@\\w+)+)','',x)
realdonaldCorpus1<-tm_map(realdonaldCorpus,content_transformer(removeRT))

removeNAME<-function(x) gsub('@\\w+','',x)
realdonaldCorpus1<-tm_map(realdonaldCorpus1,content_transformer(removeNAME))
removeHASH<-function(x) gsub('#\\w+','',x)
realdonaldCorpus1<-tm_map(realdonaldCorpus1,content_transformer(removeHASH))
inspect(realdonaldCorpus1[1:5])
realdonaldCorpus1<-tm_map(realdonaldCorpus1,tolower)
realdonaldCorpus1<-tm_map(realdonaldCorpus1,removePunctuation)
realdonaldCorpus1<-tm_map(realdonaldCorpus1,removeNumbers)
removeURL<-function(x) gsub('http[^[:alnum:]]+','',x)
realdonaldCorpus1<-tm_map(realdonaldCorpus1,content_transformer(removeURL))
realdonaldCorpus1<-tm_map(realdonaldCorpus1,removeWords,stopwords('english'))
realdonaldCorpus1<-tm_map(realdonaldCorpus1,removeWords,c('d.','dâ???¦','food','come','two','modi','amp','uuu','realdonald','uub','uue','uuuu','ueuu','uuue','ufu'
                                                        ,'uuuuu','uuuue','udu','uau','uudufue','uueuuu','uueu','uueuf','ucu','uuc','howdymodi','udueuu','uueuueu'
                                                        ,'ueubuu','f.','unno','uuuuuf','uauduuueuueuuuduu','ufuduueudua','uueuuuc','uuu.','narendramodi'
                                                        ,'uuu.','uuufuueu','uueueuu','uuuucufu','uduuduub','u.','ueu','ueuuuduuuue','uuueu','uufuu','uueuudufuuduue'
                                                        ,'ucuc','uueuuufuu','uueuu','uuudue','uuuuuueu','uuf','uueuuub','uauucu','ueuuufufue','uubu','uueuuufuue'
                                                        ,'uuub','ucuua','uubuu','uufufue','uufu','uuubuucubu','uub.','uueuubu','uueueuu.','ucuu','uuuueu'
                                                        ,'ucufu','u.'))



realdonaldCorpus1<-tm_map(realdonaldCorpus1,stripWhitespace)
inspect(realdonaldCorpus1[1:5])

## Term Document Martrix
realdonaldtdm<-TermDocumentMatrix(realdonaldCorpus1)
realdonaldtdm<-as.matrix(realdonaldtdm)
realdonaldtdm[1:10,1:20]

realdonaldITM <- TermDocumentMatrix(realdonaldCorpus1,control = list(weighting=function(x) weightTfIdf(x,normalize = T)))

a0 <- NULL
a1 <- NULL

for (i1 in 1:ncol(realdonaldtdm))
{ if (sum(realdonaldtdm[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(realdonaldITM))
{ if (sum(realdonaldITM[, i1]) == 0) {a1 = c(a1, i1)} }



realdonaldtdm <- realdonaldtdm[,-a0]
realdonaldITM <- realdonaldITM[,-a1]


realdonalddtm0 <- t(realdonaldtdm)
realdonalddtm1 <- t(realdonaldITM)




makewordc(realdonaldtdm)
title(sub = "Donald Trumph - Wordcloud")


words_bar_plot(realdonaldtdm)


makewordc(realdonaldITM)


words_bar_plot(realdonaldITM)


makeposwordc(realdonaldtdm)
title(sub = "UNIGRAM - realdonald POSITIVE Wordcloud")


pos_words_bar_plot(realdonalddtm0)

makeposwordc(realdonaldITM)
title(sub = "UNIGRAM - realdonald POSITIVE Wordcloud")

pos_words_bar_plot(realdonalddtm1)

makenegwordc(realdonaldTM) 
title(sub = "UNIGRAM - Iphone6 NEGATIVE Wordcloud")


makenegwordc(realdonaldITM) 
title(sub = "UNIGRAM - realdonald NEGATIVE Wordcloud ")


neg_words_bar_plot(realdonalddtm1)


realdonalddtm0_2 <- dfm(unlist(realdonaldCorpus1),ngrams=3,verbose = F)
realdonaldtdm0_2 <- t(realdonalddtm0_2)
a0 = NULL
for (i1 in 1:ncol(realdonalddtm0_2)){ if (sum(realdonalddtm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		
if (length(a0) >0) { realdonalddtm0_2 = realdonalddtm0_2[, -a0]} else {realdonalddtm0_2 = realdonalddtm0_2};	dim(realdonalddtm0_2)	# under TF weighing
a0 <- NULL;i1 <- NULL

realdonalddtm0_2 <- t(realdonalddtm0_2)


makewordc(realdonalddtm0_2)
title(sub = "BIGRAM - Wordcloud using TF")


words_bar_plot(realdonalddtm0_2)



realdonalddtm1_2 <- dfm_tfidf(realdonalddtm0_2)
realdonalddtm1_2 <- t(realdonalddtm1_2)
a0 = NULL
for (i1 in 1:ncol(realdonalddtm1_2)){ if (sum(realdonalddtm1_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { realdonalddtm1_2 = realdonalddtm1_2[, -a0]} else {realdonalddtm1_2 = realdonalddtm1_2};	dim(realdonalddtm1_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
realdonalddtm1_2 <- t(realdonalddtm1_2)


makewordc(realdonalddtm1_2)
title(sub = "BIGRAM - Wordcloud using TFIDF")


words_bar_plot(realdonalddtm1_2)


## Performing sentiment analysis
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)

realdonaldCorpus2<-iconv(realdonald$text)
mySentiment<-get_nrc_sentiment(realdonaldCorpus2)
SentimentScores<-data.frame(colSums(mySentiment[,]))
names(SentimentScores)<-"Score"
SentimentScores<-cbind("Sentiment"=rownames(SentimentScores),SentimentScores)

rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment),stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiment")+ylab("Score")+
  ggtitle("Sentiment Score On DonaldTrump")

#########################################################
## Quetion2 :- Amazon Reviews
#########################################################


samp_url <- "https://www.amazon.in/Apple-iPhone-Gold-32GB-Storage/product-reviews/B0725RBY9V/ref=cm_cr_arp_d_paging_btm_next_0?ie=UTF8&pageNumber"

i=1
j=1
iphone6 <- NULL
while(j>0){
  t_url <- read_html(as.character(paste(samp_url,i,sep="=")))
  review <- t_url %>%
    html_nodes(".review-text") %>%
    html_text()
  iphone6 <- c(iphone6,review)
  i <- i+1
  j=length(review)
}

length(iphone6)
write.table(iphone6,"apple.txt",row.names = F)




############################################################################

pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos.words,"wow", "kudos", "hurray") 			# including our own positive words to the existing list
neg.words = c(neg.words)
stopwdrds = readLines(file.choose())

iphone6<-readLines (file.choose())
iphone6[1:10]

iphone61<-stemDocument(iphone6)

iphone62<-Corpus(VectorSource(iphone61))
######################################################


iphone62<-tm_map(iphone62,content_transformer(stripWhitespace))
iphone62<-tm_map(iphone62,content_transformer(tolower))
iphone62<-tm_map(iphone62,content_transformer(removePunctuation))
iphone62<-tm_map(iphone62,content_transformer(removeNumbers))
iphone62<-tm_map(iphone62,removeWords,c(stopwords("english"),"amazon","rd","given","u","st","ani","onli","good","phone","product","veri","iphon","iphone","apple","use","work","dont"
                                        ,"got","get","one","appl","like","just","deliveri","day","also","can","buy","now","will","bought","better","even","test","indian","sale","festiv"
                                        ,"custom","mhz","phone","veri"
))

iphone62<-str_replace_all(tweets$text,"[^[:graph:]]", " ") 

inspect(iphone62[1:10])

iphone62TM<-TermDocumentMatrix(iphone62)

iphone62ITM <- TermDocumentMatrix(iphone62,control = list(weighting=function(x) weightTfIdf(x,normalize = T)))

inspect(iphone62ITM)

a0 <- NULL
a1 <- NULL

for (i1 in 1:ncol(iphone62TM))
{ if (sum(iphone62TM[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(iphone62ITM))
{ if (sum(iphone62ITM[, i1]) == 0) {a1 = c(a1, i1)} }



iphone62TM <- iphone62TM[,-a0]
iphone62ITM <- iphone62ITM[,-a1]


iphone62dtm0 <- t(iphone62TM)
iphone62dtm1 <- t(iphone62ITM)




makewordc(iphone62TM)
title(sub = "IPhone6 - Wordcloud")


words_bar_plot(iphone62TM)


makewordc(iphone62ITM)


words_bar_plot(iphone62ITM)


makeposwordc(iphone62TM)
title(sub = "UNIGRAM - Iphone6 POSITIVE Wordcloud")


pos_words_bar_plot(iphone62dtm0)

makeposwordc(iphone62ITM)
title(sub = "UNIGRAM - Iphone6 POSITIVE Wordcloud")

pos_words_bar_plot(iphone62dtm1)


makenegwordc(iphone62TM) 
title(sub = "UNIGRAM - Iphone6 NEGATIVE Wordcloud")


neg_words_bar_plot(iphone62dtm0)


makenegwordc(iphone62ITM) 
title(sub = "UNIGRAM - Iphone6 NEGATIVE Wordcloud ")


neg_words_bar_plot(iphone62dtm1)


library(quanteda)
library(Matrix)


iphone62dtm0_2 <- dfm(unlist(iphone62),ngrams=3,verbose = F)
iphone62tdm0_2 <- t(iphone62dtm0_2)
a0 = NULL
for (i1 in 1:ncol(iphone62dtm0_2)){ if (sum(iphone62dtm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		
if (length(a0) >0) { iphone62dtm0_2 = iphone62dtm0_2[, -a0]} else {iphone62dtm0_2 = iphone62dtm0_2};	dim(iphone62dtm0_2)	# under TF weighing
a0 <- NULL;i1 <- NULL

iphone62dtm0_2 <- t(iphone62dtm0_2)


makewordc(iphone62dtm0_2)
title(sub = "BIGRAM - Wordcloud using TF")


words_bar_plot(iphone62dtm0_2)



iphone62dtm1_2 <- tfidf(iphone62dtm0_2)
iphone62dtm1_2 <- t(iphone62dtm1_2)
a0 = NULL
for (i1 in 1:ncol(iphone62dtm1_2)){ if (sum(iphone62dtm1_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { iphone62dtm1_2 = iphone62dtm1_2[, -a0]} else {iphone62dtm1_2 = iphone62dtm1_2};	dim(iphone62dtm1_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
iphone62dtm1_2 <- t(iphone62dtm1_2)


makewordc(iphone62dtm1_2)
title(sub = "BIGRAM - Wordcloud using TFIDF")


words_bar_plot(iphone62dtm1_2)



clusdend(iphone62dtm0)
title(sub = "IPhone6 DTM Dendogram")


clusdend(iphone62dtm1)
title(sub = "IPhone6 DTM transpose Dendogram")

#################################################################################
## Emotion mining
#####################################################################################

iphone621<-iconv(realdonald$text)
mySentiment<-get_nrc_sentiment(iphone621)
SentimentScores<-data.frame(colSums(mySentiment[,]))
names(SentimentScores)<-"Score"
SentimentScores<-cbind("Sentiment"=rownames(SentimentScores),SentimentScores)

rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment),stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiment")+ylab("Score")+
  ggtitle("Sentiment Score On Iphone6")

##################################################################
## Quetion3.1 :- IMDB Reviews Analysis The Family Man
##################################################################

install.packages('rvest')

library('rvest')
library('sqldf')

url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'



library(rvest)
library(magrittr)
library(XML)
a<-10
king<-NULL
url1<-"http://www.imdb.com/title/tt9544034/reviews?start="
for(i in 0:30){
  url<-read_html(as.character(paste(url1,i*a,sep="")))
  ping<-url %>%
    html_nodes(".show-more__control") %>%
    html_text() 
  FM <- c(king,ping)
}


write.csv(FM,file="TheFamilyMan.csv",row.names = F)
getwd()

a<-10
fm2<-NULL
url2<-"http://www.imdb.com/title/tt9544034/reviews?start="
for(i in 0:30){
  url<-read_html(as.character(paste(url1,i*a,sep="")))
  ping2<-url %>%
    html_nodes(".title") %>%
    html_text() 
  fm2 <- c(fm2,ping2)
}
fm2

write.csv(FM,file="TheFamilyManTitle.csv",row.names = F)

FMReview<- merge(FM, fm2, by=0, all=TRUE)
View(FMReview)

colnames(FMReview)<-c("Rowno","Reviews1","Titles1")

View(FMReview1)

FMReview1<-FMReview

str(FMReview1)

FMReview1$Reviews<-as.character(FMReview1$Reviews)
FMReview1$Titles<-as.character(FMReview1$Titles)
FMReview$Rowno<-as.integer(FMReview$Rowno)

dataset <- matrix(sample(c(NA, 1:5), 25, replace = TRUE), 5);
data <- as.data.frame(dataset)


FMReview1<-FMReview
FMReview1 <- na.omit(FMReview1) 

FMReview1<-sqldf("select * from FMReview where Reviews1 is not null or Reviews1=''")
sqldf("select * from FMReview1 limit 5")

## Building Corpus
#FMReviewCorpus<-iconv(FMReview$text, to="utf-8")

FMReviewCorpus<-iconv(FMReview$Reviews1)
FMReviewCorpus<-Corpus(VectorSource(FMReviewCorpus))
inspect(FMReviewCorpus[1:5])
?tm_map



## Clean text

inspect(FMReviewCorpus[1:5])
FMReviewCorpus1<-FMReviewCorpus
FMReviewCorpus1<-tm_map(FMReviewCorpus1,tolower)
FMReviewCorpus1<-tm_map(FMReviewCorpus1,removePunctuation)
FMReviewCorpus1<-tm_map(FMReviewCorpus1,removeNumbers)
removeURL<-function(x) gsub('http[^[:alnum:]]+','',x)
FMReviewCorpus1<-tm_map(FMReviewCorpus1,content_transformer(removeURL))
FMReviewCorpus1<-tm_map(FMReviewCorpus1,removeWords,stopwords('english'))

FMReviewCorpus1<-tm_map(FMReviewCorpus1,removeWords,c('enuff','series'))



FMReviewCorpus1<-tm_map(FMReviewCorpus1,stripWhitespace)
inspect(FMReviewCorpus1[1:5])


FMReviewCorpus1TM<-TermDocumentMatrix(FMReviewCorpus1)

FMReviewCorpus1ITM <- TermDocumentMatrix(FMReviewCorpus1,control = list(weighting=function(x) weightTfIdf(x,normalize = T)))

inspect(FMReviewCorpus1ITM)

a0 <- NULL
a1 <- NULL

for (i1 in 1:ncol(FMReviewCorpus1TM))
{ if (sum(FMReviewCorpus1TM[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(FMReviewCorpus1ITM))
{ if (sum(FMReviewCorpus1ITM[, i1]) == 0) {a1 = c(a1, i1)} }



FMReviewCorpus1TM <- FMReviewCorpus1TM[,-a0]
FMReviewCorpus1ITM <- FMReviewCorpus1ITM[,-a1]


FMReviewCorpus1dtm0 <- t(FMReviewCorpus1TM)
FMReviewCorpus1dtm1 <- t(FMReviewCorpus1ITM)




makewordc(FMReviewCorpus1TM)
title(sub = "The Family Man - Wordcloud")


words_bar_plot(FMReviewCorpus1TM)


makewordc(FMReviewCorpus1ITM)


words_bar_plot(FMReviewCorpus1ITM)


makeposwordc(FMReviewCorpus1TM)
title(sub = "UNIGRAM - The Family Man POSITIVE Wordcloud")


pos_words_bar_plot(FMReviewCorpus1dtm0)

makeposwordc(FMReviewCorpus1ITM)
title(sub = "UNIGRAM - The Family Man POSITIVE Wordcloud")

pos_words_bar_plot(FMReviewCorpus1dtm1)

makenegwordc(FMReviewCorpus1TM)




makenegwordc(FMReviewCorpus1ITM) 



neg_words_bar_plot(FMReviewCorpus1dtm1)


## Performing sentiment analysis
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)

mySentiment<-get_nrc_sentiment(FMReviewCorpus1)
SentimentScores<-data.frame(colSums(mySentiment[,]))
names(SentimentScores)<-"Score"
SentimentScores<-cbind("Sentiment"=rownames(SentimentScores),SentimentScores)

rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment),stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiment")+ylab("Score")+
  ggtitle("Sentiment Score On The Family Man")

#########################################################
## Quetion 3.2 :- Twitter Analysis on Event "Howdy Modi"
#########################################################

# Load Requried Packages
library("SnowballC")
library("tm")
library("syuzhet")

library("twitteR")
library("ROAuth")

consumer_key <- "Enter consumer_key"
consumer_secret <- "Enter consumer_secret"
access_token <- "Enter access_token"
access_secret <- "Enter access_secret"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


tw = twitteR::searchTwitter('#HowdyMody', n = 1e4, retryOnRateLimit = 1e3)
d = twitteR::twListToDF(tw)

write.csv(d,"HowdyMody.csv",row.names = F)

pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos.words,"wow", "kudos", "hurray","wealthy") 			# including our own positive words to the existing list
neg.words = c(neg.words,'chor','bhaktards','ghatya')
stopwdrds = readLines(file.choose())

HowdyMody<-read.csv(file.choose(),header=TRUE,na.strings=c("","NA","na","Na","nA")) 
HowdyMody[1:10]

sqldf("select text from HowdyMody limit 10")
str(HowdyMody)

## Building Corpus

HowdyModyCorpus<-iconv(HowdyMody$text)
HowdyModyCorpus<-Corpus(VectorSource(HowdyModyCorpus))
inspect(HowdyModyCorpus[1:5])
?tm_map

## Clean text
removeRT<-function(x) gsub('(RT|via)((?:\\b\\w*@\\w+)+)','',x)
HowdyModyCorpus1<-tm_map(HowdyModyCorpus,content_transformer(removeRT))

removeNAME<-function(x) gsub('@\\w+','',x)
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,content_transformer(removeNAME))
removeHASH<-function(x) gsub('#\\w+','',x)
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,content_transformer(removeHASH))
inspect(HowdyModyCorpus1[1:5])
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,tolower)
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,removePunctuation)
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,removeNumbers)
removeURL<-function(x) gsub('http[^[:alnum:]]+','',x)
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,content_transformer(removeURL))
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,removeWords,stopwords('english'))
HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,removeWords,c('d.','dâ???¦','food','come','two','modi','amp','uuu','howdymody','uub','uue','uuuu','ueuu','uuue','ufu'
                                                        ,'uuuuu','uuuue','udu','uau','uudufue','uueuuu','uueu','uueuf','ucu','uuc','howdymodi','udueuu','uueuueu'
                                                        ,'ueubuu','f.','unno','uuuuuf','uauduuueuueuuuduu','ufuduueudua','uueuuuc','uuu.','narendramodi'
                                                        ,'uuu.','uuufuueu','uueueuu','uuuucufu','uduuduub','u.','ueu','ueuuuduuuue','uuueu','uufuu','uueuudufuuduue'
                                                        ,'ucuc','uueuuufuu','uueuu','uuudue','uuuuuueu','uuf','uueuuub','uauucu','ueuuufufue','uubu','uueuuufuue'
                                                        ,'uuub','ucuua','uubuu','uufufue','uufu','uuubuucubu','uub.','uueuubu','uueueuu.','ucuu','uuuueu'
                                                        ,'ucufu','u.'))



HowdyModyCorpus1<-tm_map(HowdyModyCorpus1,stripWhitespace)
inspect(HowdyModyCorpus1[1:5])

## Term Document Martrix
HowdyModytdm<-TermDocumentMatrix(HowdyModyCorpus1)
HowdyModytdm<-as.matrix(HowdyModytdm)
HowdyModytdm[1:10,1:20]

HowdyModyITM <- TermDocumentMatrix(HowdyModyCorpus1,control = list(weighting=function(x) weightTfIdf(x,normalize = T)))

a0 <- NULL
a1 <- NULL

for (i1 in 1:ncol(HowdyModytdm))
{ if (sum(HowdyModytdm[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(HowdyModyITM))
{ if (sum(HowdyModyITM[, i1]) == 0) {a1 = c(a1, i1)} }



HowdyModytdm <- HowdyModytdm[,-a0]
HowdyModyITM <- HowdyModyITM[,-a1]


HowdyModydtm0 <- t(HowdyModytdm)
HowdyModydtm1 <- t(HowdyModyITM)




makewordc(HowdyModytdm)
title(sub = "HowdyModi - Wordcloud")


words_bar_plot(HowdyModytdm)


makewordc(HowdyModyITM)


words_bar_plot(HowdyModyITM)


makeposwordc(HowdyModytdm)
title(sub = "UNIGRAM - HowdyMody POSITIVE Wordcloud")


pos_words_bar_plot(HowdyModydtm0)

makeposwordc(HowdyModyITM)
title(sub = "UNIGRAM - HowdyMody POSITIVE Wordcloud")

pos_words_bar_plot(HowdyModydtm1)


makenegwordc(HowdyModydtm0) # doubts doubt 
title(sub = "UNIGRAM - HowdyMody NEGATIVE Wordcloud")


neg_words_bar_plot(HowdyModydtm0)


makenegwordc(HowdyModyITM) 
title(sub = "UNIGRAM - Iphone6 NEGATIVE Wordcloud ")


neg_words_bar_plot(HowdyModydtm1)

## Performing sentiment analysis
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)

HowdyModyCorpus2<-iconv(HowdyMody$text)
mySentiment<-get_nrc_sentiment(HowdyModyCorpus2)
SentimentScores<-data.frame(colSums(mySentiment[,]))
names(SentimentScores)<-"Score"
SentimentScores<-cbind("Sentiment"=rownames(SentimentScores),SentimentScores)

rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment),stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiment")+ylab("Score")+
  ggtitle("Sentiment Score On HowdyModi")






