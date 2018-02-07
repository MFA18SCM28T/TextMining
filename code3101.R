library(tm)
library(RTextTools)
library(dplyr)
library(stringr)
library(NbClust)
setwd("D:/Sem2_Spring2018/ADM")
business <- read.csv  (file="yelp_academic_dataset_business.csv", header=TRUE,nrows = 320000)
describe(business)
head(business$categories1)

yelp <- read.csv(file = "yelp_academic_dataset_review_all1.csv", header = TRUE, nrows = 320000)
yelp <- yelp[,c("business_id","text")]
business$business_id <- gsub("^b'", "", business$business_id)
business$business_id <- gsub("'$", "", business$business_id)

#business.seletedCategories <- business[grepl("American",business$categories1)
#                                       | grepl("Asian",business$categories1)
 #                                      | grepl("Mini Golf", business$categories1)
  #                                     | grepl("Sports",business$categories1)
   #                                    | grepl("Thai",business$categories1),]
#filtered <- merge ( x = business.seletedCategories , y = yelp, by.x=c("business_id"), by.y=c("business_id"))   
filtered <- merge (x = business , y = yelp, by.x=c("business_id"), by.y=c("business_id"))   

filtered <- filtered[,c("business_id","text")]
ta <- data.frame(filtered, stringsAsFactors = FALSE)
corpus <- Corpus(DataframeSource(ta))
colnames(filtered) <- c("doc_id","text")
t <- data.frame(filtered, stringsAsFactors = FALSE)
corpus <- Corpus(DataframeSource(t))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, "the")
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
dtm_yelp = DocumentTermMatrix(corpus)
dtm_yelp
tdm_yelp <- TermDocumentMatrix(corpus)
tdm_yelp
m <- as.matrix(dtm_yelp)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
head(d,10)

library("RColorBrewer")
library("wordcloud")
#install.packages("wordcloud")
wordcloud(corpus, max.words = 100, random.order = FALSE, colors=brewer.pal(8,"Dark2"))

dtm_yelp_sparse <- removeSparseTerms(dtm_yelp, 0.99)
dtm_yelp_sparse
tdm_yelp_sparse <- removeSparseTerms(tdm_yelp,0.99)
tdm_yelp_sparse
rowTotals1 <- apply(dtm_yelp_sparse , 1, sum) #Find the sum of words in each Document
rowTotals1
dtm.new1   <- dtm_yelp_sparse[rowTotals1>110 ,] 
dtm.new1
yelp.tfidf<- weightTfIdf(dtm.new1)
rowTotals1 <- apply(dtm_yelp_sparse,1,sum) #Find the sum of words in each Document
dtm.new   <- dtm_yelp_sparse[rowTotals1>4.5, ] 
dtm.new
rowTotals2 <- apply(tdm_yelp_sparse,2,sum)

tdm.new   <- tdm_yelp_sparse[, rowTotals2>120.5] 
tdm.new

library(NbClust)
nb <- NbClust(data=dtm.new, distance="euclidean", method="kmeans", index = "silhouette")
nb$Best.nc
m <- as.matrix(dtm.new)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m,MARGIN = 1, FUN=function(x)sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl <- kmeans(na.omit(m_norm),2)
table(cl$cluster)
plot(prcomp(m_norm)$x,col=cl$cluster)
#plot(m_norm,col=cl$cluster)
cl$withinss
cl$tot.withinss
cl$totss
tdm_yelp <- TermDocumentMatrix(corpus)
tdm_yelp
tdms = removeSparseTerms(tdm_yelp, 0.99)
tdms
rowTot <- apply(tdms , 1, sum) #Find the sum of words in each Document
rowTot
tdm.new   <- tdms[,rowTot>4.5 ] 
tdm.new
for(dimension in c(50))
{
  sing<-svd(dtm.new1)
  D<-diag(sing$d)
  diagD<-D[1:dimension,1:dimension]
  sing$u[,1:dimension] %*% diagD
  documentVector<-sing$u[,1:dimension] %*% diagD
  vt <- t(sing$v)
  wordVector<-diagD %*% sing$v[1:dimension,]
} 
for(dimension in c(100))
{
  sing1<-svd(tdm.new)
  D1<-diag(sing1$d)
  diagD1<-D1[1:dimension,1:dimension]
  sing1$u[,1:dimension] %*% diagD1
  v1 <- t(sing1$v)
  documentVector1 <- diagD1 %*% v1[1:dimension,]
  wordVector1<- sing1$u[,1:dimension]%*% diagD1
} 
m <- as.matrix(documentVector)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m,MARGIN = 1, FUN=function(x)sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl <- kmeans(na.omit(m_norm),10)
table(cl$cluster)
plot(prcomp(m_norm)$x,col=cl$cluster)
#plot(m_norm,col=cl$cluster)
cl$withinss
cl$tot.withinss
cl$totss
sv = sort.list(abs(sing$v[, 5]), decreasing = TRUE)
dtm$dimnames$Terms[head(sv, 10)]


num=5
concept<-function(num){
  sv<-sort.list(svd(dtm.new1)$v[,num],decreasing = FALSE)
  dm<-dtm.new1$dimnames$Terms[head(sv,10)]
  dm
}
i<-(1:num)
lapply(i,concept)

num=4
concept<-function(num){
  sv<-sort.list(svd(dtm.new)$v[,num],decreasing = FALSE)
  dm<-dtm.new$dimnames$Terms[head(sv,10)]
  dm
}
i<-(1:num)
lapply(i,concept)

library("MASS")
library(lda)
install.packages("lda")  
library(topicmodels)
install.packages("topicmodels")
install.packages("tidytext")
ldanews<-LDA(dtm.new1,50,method = "Gibbs")
ldanews

lda_topics <- tidy(ldanews, matrix = "beta")
lda_topics
library(tidytext)
install.packages("tokenizers")
install.packages("magrittr")
library(magrittr)
library(dplyr)
library(ggplot2)
ldatopics<-as.matrix(topics(ldanews))
ldaterms<-as.matrix(terms(ldanews,20))
ldaterms
terms(ldanews)
x<-topics(ldanews)

ldanews <- LDA(tf,50,method = "Gibbs")
lda.topics.50 <- as.matrix(topics(ldanews))
head(lda.topics.300,20)
lda.terms <- as.matrix(terms(ldanews, ))
head(lda.terms,20)
topicProbabilities <- as.data.frame(ldanews@gamma)

topicProbability <- as.data.frame(ldanews@gamma)
m <- as.matrix(topicProbability)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5) 
m_norm <- norm_eucl(m)
cl<- kmeans(m_norm, 10) 
table(cl$cluster)
plot(prcomp(m_norm)$x, col=cl$cl)
cl$withinss
cl$tot.withinss
cl$totss
model_tfidf = models.TfidfModel(mm_bow, id2word=id2word, normalize=True)
install.packages("TfIdf")
library("TfIdf")
