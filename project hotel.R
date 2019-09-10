hotel<-Hotel_Reviews
library(NLP)
library(caret)
library(tidyverse)
library(tidytext)
library(plyr)
library(dplyr)
library(sentimentr)
library(SnowballC)
library(tm)
library(RColorBrewer)
library(ROAuth)
library(wordcloud)
library(corpus)
library(ggrepel)
theme_set(theme_classic())

hotel.df <- hotel
str(hotel.df)
hotel.df <- hotel.df[1:5000, ]
hotel.df$reviews = paste (hotel.df$Negative_Review, hotel.df$Positive_Review)
set.seed(1207)
index <- 1:nrow(hotel.df)

# partition data into training and validation
training.index <- sample(index,trunc(length(index)*0.8))
training.df <- hotel.df$reviews[training.index]
validation.df <- hotel.df$reviews[-training.index]
---------------------------------------------
  #correaltion plot
  library(corrplot)
M <- cor(f[c(2,4,8,9,11,12,13,15)])
corrplot(M, method = "circle")
  
  # scoring or reviewer
 g <- ggplot(hotel.df[sample(nrow(hotel.df), 5000), ],aes(x=Reviewer_Score)) + geom_histogram(binwidth = 1)+theme_bw()+ggtitle('Distribution of reviewer socres')
plot(g)

hj


View(hotel.df)
#google map

summary(hotel.df)
library(leaflet)
library(leaflet.extras)
library(tidyr)
library(scales)
library(ggplot2)

hotel.names = hotel.df %>%
  select(Hotel_Name, Hotel_Address, lat, lng, Average_Score, Total_Number_of_Reviews,
         Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
  #Remove the 17 records without geo coordinates
  filter(lat != 0 & lng != 0) %>%
  group_by(Hotel_Name, Hotel_Address, lat, lng, Average_Score, Total_Number_of_Reviews) %>%
  summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
            Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
            Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
            Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
            Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words))
points <- cbind(hotel.names$lng,hotel.names$lat)
leaflet() %>% 
  addProviderTiles('OpenStreetMap.Mapnik',
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addMarkers(data = points,
             popup = paste0("<strong>Hotel: </strong>",
                            hotel.names$Hotel_Name,                 
                            "<br><strong>Address: </strong>", 
                            hotel.names$Hotel_Address, 
                            "<br><strong>Average Score: </strong>", 
                            hotel.names$Average_Score, 
                            "<br><strong>Number of Reviews: </strong>", 
                            hotel.names$Total_Number_of_Reviews,
                            "<br><strong>Percent Positive Review Words: </strong>",
                            hotel.names$Pos_Word_Rate),
             clusterOptions = markerClusterOptions())

colSums(sapply(hotel.df, is.na))
hotel.df%>%select(Average_Score,Hotel_Address)%>%distinct(Average_Score,Hotel_Address)%>%ggplot(aes(x=Average_Score))+
  geom_histogram(color='blue',fill='blue',alpha=0.3,bins=30)+
  xlab("Average Review Score")+ylab("Counts")

#introduce corpus
corpus<-Corpus(VectorSource(hotel.df$reviews))
#text cleaning
View(corpus)
#convert the text to lower case
corpus <- tm_map(corpus,content_transformer(tolower))
inspect(corpus[1:20])
#remove nuymbers
corpus<- tm_map(corpus,removeNumbers)
inspect(corpus[1:20])
#remove english common stopwords
corpus<- tm_map(corpus,removeWords,stopwords("english"))
inspect(corpus[1:20])
#remove punctuation
corpus<-tm_map(corpus,removePunctuation)
inspect(corpus[1:20])
#remove extra whitespaces
corpus<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:20])


#load library
library(SnowballC)
#Stem document
corpus <- tm_map(corpus,stemDocument)
writeLines(as.character(corpus[[30]]))
dtm <- DocumentTermMatrix(corpus)
dtm
inspect(dtm[1:2,1000:1005])
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
#inspect least frequently occurring terms
freq[tail(ord)]
dtmr <-DocumentTermMatrix(corpus, control=list(wordLengths=c(4, 20),
                                               bounds = list(global = c(3,27))))
dtmr
freqr <- colSums(as.matrix(dtmr))
#length should be total number of terms
length(freqr)
#create sort order (asc)
ordr <- order(freqr,decreasing=TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]
#inspect least frequently occurring terms
freqr[tail(ordr)]
findFreqTerms(dtmr,lowfreq=20)
wf=data.frame(term=names(freqr),occurrences=freqr)

# worldcloud
wordcloud(names(freq), freq = freq, max.words=1000, 
          random.order = FALSE, 
          min.freq =50,colors= rainbow(50),
          rot.per= 0.7)

library(ggplot2)
p <- ggplot(subset(wf, freqr>20), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


--------------------------------------------------------------------------------------------------------
  
  #sentimental analysis
  library(syuzhet)
library(lubridate)
review <- iconv (corpus)
s <- get_nrc_sentiment(review)
barplot(colSums(s), las=2, col= rainbow(10), ylab= 'Count', main = 'sentiment analysis of Hotel Review')
--------------------------------------------------------------------------------------------------------------
  #clustering
  v <- as.matrix(dtm)
d <- dist(v)
#run hierarchical clustering using Ward’s method
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=1)
rect.hclust(groups,2)
#k means algorithm, 2 clusters, 100 starting configurations
kfit <- kmeans(d, 5, nstart=100)
#plot – need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#kmeans – determine the optimum number of clusters (elbow method)
#look for “elbow” in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:29
for (i in 2:29) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:29, wss[2:29], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
itemfrequencyplot()
