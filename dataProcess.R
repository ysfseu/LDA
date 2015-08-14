library(tm)
library(topicmodels)
#read data
map<-read.table("./data/cate_id.log",encoding = "UTF-8",sep=',',as.is = TRUE)
names(map)<-c("id","name")
dataframe<-read.table("./data/app_inst_filter2.log",encoding = "UTF-8",sep='\t',as.is = TRUE)
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