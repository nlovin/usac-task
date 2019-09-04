library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

frn16 <- frn %>% filter(funding_year == 2016) 
dfCorpus <- VCorpus(VectorSource(frn16$narrative))

# 1. Stripping any extra white space:
dfCorpus <- tm_map(dfCorpus, stripWhitespace)
# 2. Transforming everything to lowercase
dfCorpus <- tm_map(dfCorpus, content_transformer(tolower))
# 3. Removing numbers 
dfCorpus <- tm_map(dfCorpus, removeNumbers)
# 4. Removing punctuation
dfCorpus <- tm_map(dfCorpus, removePunctuation)
# 5. Removing stop words
dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))

dfCorpus <- tm_map(dfCorpus, stemDocument)

DTM <- DocumentTermMatrix(dfCorpus)

colTotals <- slam::col_sums(DTM)
DTM2 <- DTM[,which(colTotals > 5000)]

sums <- as.data.frame(colSums(as.matrix(DTM2)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
head <- sums[1:35,]
wordcloud(words = head$term, freq = head$count, min.freq = 1000,
          max.words=44, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

rm(dfCorpus,DTM,mat, dtm, sums, head, frn16, DTM2, corpus)

