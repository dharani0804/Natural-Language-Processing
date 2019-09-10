library(corpus)
corpus<-corpus(VectorSource(amazonkindle$reviewText))
#text cleaning
View(corpus)
#convert the text to lower case
corpus <- tm_map(corpus,content_transformer(tolower))
inspect(corpus[1:20])
#remove nuymbers
corpus<- tm_map(corpus,removeNumbers)
#remove english common stopwords
corpus<- tm_map(corpus,removeWords,stopwords("english"))
#remove punctuation
corpus<-tm_map(corpus,removePunctuation)
#remove extra whitespaces
corpus<-tm_map(corpus,stripWhitespace)
#remove selected words from previous wordcloud search to get meani8ngful results
corpus<-tm_map(corpus,removeWords,c("this","the","was","and","you","but","her","this","that","she","with","book"))
#create term document matrix(matrix which describes the frequency of the terms)
tdm<-TermDocumentMatrix(corpus)
tdm<-as.matrix(tdm)
tdm[1:10, 1:20]
v<-rowSums(tdm)
v<- subset(v, v>=30)
v< -sort(rowSums(tdm), decreasing= TRUE)
barplot(rowSums(tdm), las=2, col= rainbow(50), ylab= 'Count', main = 'sentiment analysis of amazon kindle')
set.seed(222)
wordcloud(names(v), freq = v, max.words=1000, 
          random.order = FALSE, 
          min.freq =5,colors= rainbow(50),
rot.per= 0.7)
letterCloud(v, word= "amazon", size=1)
v.df<-data.frame(names(v),v)
colnames(v) <- c("word","freq")
  
# sentimental analysis 

library(syuzhet)
library(lubridate)
s <- get_nrc_sentiment(amazonkindle$reviewText)
barplot(colSums(s), las=2, col= rainbow(10), ylab= 'Count', main = 'sentiment analysis of amazon kindle')


