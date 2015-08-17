library(tm)
library(topicmodels)
#read data
dataframe<-read.table("./data/system",encoding = "UTF-8",sep='\t',as.is = TRUE)
#preprocess data
dataframe<-dataframe[!duplicated(dataframe), ]
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
dataframe<- dataframe[rowTotals> 0, ]
k = 5
lda.model = LDA(dtm, k,method = "Gibbs",control = list(iter= 200))

apps.topics=posterior(lda.model,dtm)$topics
df.apps.topics=as.data.frame(apps.topics)
df.apps.topics = cbind(userid=dataframe[,1], 
                       df.apps.topics, stringsAsFactors=F)
#get the doc and its topic
topics<- apply(df.apps.topics[,c(2:(k+1))], 1, which.max)
doc_topics<-cbind(dataframe[,1],topics) 
# predict
test<-dtm[1:100,]
test.topics <- posterior(lda.model,test)
# select the topic with the highest proportion.
(test.topics <- apply(test.topics$topics, 1, which.max))

#Get the nt highest terms for each topic.And map the terms' id to its real names.
nt<-15
apps.terms<-terms(lda.model,nt)
map<-read.table("./data/cate_id.log",encoding = "UTF-8",sep=',',as.is = TRUE)
names(map)<-c("id","name")
apps.terms<-data.frame(apps.terms)
index<-data.frame(matrix(ncol = k,nrow = nt))
for(i in 1:k)
{
  index[,i]<-match(apps.terms[,i],map$id)
}
topicmap<-NULL
for(i in 1:k)
{
  topicmap<-cbind(topicmap,map[index[,i],2])
}

clusters<-kmeans(lda.model@gamma,5)

