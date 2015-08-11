library(tm)
library(topicmodels)
#read data
dataframe<-read.table("./data/app_cate.log",encoding = "UTF-8",sep='\t',as.is = TRUE)
#preprocess data
names(dataframe)<-c("user_id","apps")
apps<-dataframe$apps
apps <- gsub("[[:punct:]]", " ", apps)  # replace punctuation with space
apps <- gsub("[[:cntrl:]]", " ", apps)  # replace control characters with space
apps <- gsub("^[[:space:]]+", "", apps) # remove whitespace at beginning of documents
apps <- gsub("[[:space:]]+$", "", apps) # remove whitespace at end of documents
#create corpus
corpus<-Corpus(VectorSource(apps))
#create DomcumentTermMatrix. 
#The problem is that the default behavior for the TermDocumentMatrix function from the tm pacakge is that is only tracks words that are longer than three characters. 
#So use control option to specify the wordLenths
dtm<-DocumentTermMatrix(corpus,control=list(wordLengths=c(1,Inf)))
#delete rows without terms
rowTotals <- apply(dtm , 1, sum)
dtm<- dtm[rowTotals> 0, ] 
k = 4
lda.model = LDA(dtm, k)
terms(lda.model,20)
apps.topics=posterior(lda.model,dtm)$topics
df.apps.topics=as.data.frame(apps.topics)
df.apps.topics = cbind(app=as.character(rownames(df.apps.topics)), 
                       df.apps.topics, stringsAsFactors=F)
sample(which(df.apps.topics$"1" > .6), 10)
