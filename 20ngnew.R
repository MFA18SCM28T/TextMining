setwd("D:/Sem2_Spring2018/ADM/20news-18828")
#folderdir = c("D:/Sem2_Spring2018/ADM/20news-18828/comp.sys.ibm.pc.hardware","D:/Sem2_Spring2018/ADM/20news-18828/comp.sys.mac.hardware")
folderdir = c("alt.atheism","comp.graphics","rec.autos","misc.forsale","sci.med","talk.politics.mideast")
#folderdir = c("D:/Sem2_Spring2018/ADM/20news-18828/comp.sys.ibm.pc.hardware","D:/Sem2_Spring2018/ADM/20news-18828/comp.sys.mac.hardware","D:/Sem2_Spring2018/ADM/20news-18828/rec.sport.baseball","D:/Sem2_Spring2018/ADM/20news-18828/rec.sport.hockey","D:/Sem2_Spring2018/ADM/20news-18828/sci.med","D:/Sem2_Spring2018/ADM/20news-18828/sci.space")
library(tm)
myReader <- readTabular(mapping=list(content="text", id="id"))
alldir=DirSource(folderdir, encoding = "UTF-8", recursive=TRUE)
news <- Corpus(alldir, readerControl=list(reader=readPlain,language="en"))


#class <- c(rep("comp",982),rep("spo",994),rep("sci",990),rep("comp",961),rep("spo",999),rep("sci",987))
class <- c(rep("ath",799),rep("comp",973),rep("rec",990),rep("misc",972),rep("sci",990),rep("talk",940))
inspect(news[1:1])
#news <- cbind( news, class)
category <- vector("character",5663)
category[1:799]<- "ath"# 1#"ath"
category[800:1773]<- "comp"# 2#"comp"
category[1774:2764]<- "rec3" #3"rec"
category[2765:3737]<-"misc"# 4#"misc"
category[3738:4728]<- "sci"#5#"sci"
category[4729:5663]<- "talk"#6#"talk"

table(category,cl2$cluster)
confusionMatrix(category,cl2$cluster)
library("caret")
install.packages("caret")
ncol(news)
#category <- vector("science",982)
RemoveEmail <- function(x) {
  require(stringr)
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
}
news <- tm_map(news,content_transformer(RemoveEmail))
news <- tm_map(news, removeWords, c("Subject", "From", "To", "Expires", "Organization", "Lines", "MessageId", "Supersedes", "Summary"))
news <- tm_map(news, removeWords, "the")
news <- tm_map(news, tolower)
news <- tm_map(news, removeNumbers)
news <- tm_map(news, removeWords, stopwords("en"))
news <- tm_map(news, removeWords, stopwords(kind = "SMART"))
news <- tm_map(news, stemDocument)
news <- tm_map(news, stripWhitespace)
news <- tm_map(news, removePunctuation)
news <- tm_map(news, stripWhitespace)
dtm <- DocumentTermMatrix(news)
tdm <- TermDocumentMatrix(news)
dtm
tdm

library("RColorBrewer")
library("wordcloud")
install.packages("wordcloud")
wordcloud(news, max.words = 100, random.order = FALSE, colors=brewer.pal(8,"Dark2"))

m <- as.matrix(tdm.new2)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
head(d,10)

freq <- colSums(as.matrix(dtm))
head(freq,10)
uniqueWords = function(text) {
  text <- strsplit(text, " |,|/|;")
  text <- lapply(text,unique)
  text <- sapply(text, function(u) paste0(u, collapse = " "))
  return(text) 
}
news <- tm_map(news,content_transformer(text))
  
dtmn <- removeSparseTerms(dtm, 0.99)
dtmn
tdmn <- removeSparseTerms(tdm, 0.99)
tdmn


#remove empty docs
rowTotals1 <- apply(dtmn , 1, sum) #Find the sum of words in each Document
rowTotals1
dtm.new1   <- dtmn[rowTotals1>0 ,] 
dtm.new1

rowTotals12 <- apply(tdmn , 2, sum) #Find the sum of words in each Document
rowTotals12
tdm.new2   <- tdmn[, rowTotals12>0] 
tdm.new2

library(SnowballC)
review_dtm_tfidf <- weightTfIdf(dtmn)

#tf <- tm::weightTf(review_dtm_tfidf)
review_tdm_tfidf <-weightTfIdf(tdm.new2)
review_dtm_tfidf
library(NbClust)
nb <- NbClust(data = review_dtm_tfidf, distance = "euclidean", method = "kmeans", index = "silhouette",min.nc =6,max.nc = 10 )
nb$Best.nc
library(NbClust)
library(ggplot2)



?fviz_nbclust
m <- as.matrix(review_dtm_tfidf)
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
num=5
concept<-function(num){
  sv<-sort.list(svd(dtm.new1)$v[,num],decreasing = FALSE)
  dm<-review_dtm_tfidf$dimnames$Terms[head(sv,10)]
  dm
}
i<-(1:num)
lapply(i,concept)

for(dimension in c(50))
{
  sing<-svd(dtmn)
  D<-diag(sing$d)
  diagD<-D[1:dimension,1:dimension]
  sing$u[,1:dimension] %*% diagD
  documentVector<-sing$u[,1:dimension] %*% diagD
  vt <- t(sing$v)
  wordVector<-diagD %*% sing$v[1:dimension,]
}
for(dimension in c(200))
{
  sing1<-svd(tdm.new2)
  D1<-diag(sing1$d)
  diagD1<-D1[1:dimension,1:dimension]
  sing1$u[,1:dimension] %*% diagD1
  v1 <- t(sing1$v)
  documentVector1 <- diagD1 %*% v1[1:dimension,]
  wordVector1<- sing1$u[,1:dimension]%*% diagD1
} 
  
for(dimension in c(200))
{
  sing1<-svd(tdm.new2)
  D1<-diag(sing1$d)
  diagD1<-D1[1:dimension,1:dimension]
  sing1$u[,1:dimension] %*% diagD1
  v1 <- t(sing1$v)
  documentVector1 <- diagD1 %*% v1[1:dimension,]
  wordVector1<- sing1$u[,1:dimension]%*% diagD1
} 

#Change parameter value below for different set of data for m
m <- as.matrix(documentVector)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl2<- kmeans(na.omit(m_norm), 6)
table(cl2$cluster) # to check count of each cluster
plot(prcomp(m_norm)$x, col=cl2$cl) #PCA
cl2$withinss
cl2$tot.withinss
cl2$totss
cl2$betweenss/cl2$totss
100 - (cl2$withinss/cl2$totss) *100
sv = sort.list(abs(sing1$v[, 5]), decreasing = TRUE)
dtm$dimnames$Terms[head(sv,10)]
ct <-table(class,cl2$cluster)
#words based on concept
cl2$cluster

  #Change parameter value below for different set of data for m
  m <- as.matrix(t(documentVector1))
  rownames(m) <- 1:nrow(m)
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  m_norm <- norm_eucl(m)
  cl2<- kmeans(na.omit(m_norm), 6)
  table(cl2$cluster) # to check count of each cluster
  plot(prcomp(m_norm)$x, col=cl2$cl) #PCA
  cl2$withinss
  cl2$tot.withinss
  cl2$totss
  cl2$betweenss/cl2$totss
  100 - (cl2$withinss/cl2$totss) *100
  sv = sort.list(abs(sing1$v[, 5]), decreasing = TRUE)
  dtm$dimnames$Terms[head(sv,10)]
   ct <-table(class,cl2$cluster)
  #words based on concept
  cl2$cluster
  num=5
  concept<-function(num){
    sv<-sort.list(svd(dtm.new1)$v[,num],decreasing = FALSE)
    dm<-dtm.new1$dimnames$Terms[head(sv,10)]
    dm
  }
  i<-(1:num)
  lapply(i,concept)
  
  #LDA
library("MASS")
library(lda)
install.packages("lda")  
library(topicmodels)
install.packages("topicmodels")
install.packages("tidytext")
ldanews<-LDA(dtm.new1,6,method = "Gibbs")
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


new.df <- data.frame('response'=names(x), 'topic'=x, row.names=NULL)
count(new.df, vars='terms')

lda_top_terms <- lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lda_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ldanews <- LDA(tf,10,method = "Gibbs")
lda.topics.50 <- as.matrix(topics(ldanews))
head(lda.topics.300,20)
lda.terms <- as.matrix(terms(ldanews, ))
head(lda.terms,20)
topicProbabilities <- as.data.frame(ldanews@gamma)


ldanews<-LDA(dtm.new1,50,method = "Gibbs")

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

p=0.56
r=0.65
f1 <- 2*p*r/(p+r)
f1

