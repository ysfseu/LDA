library(tm)
library(topicmodels)
library(recharts)
source("dataProcess.R")
default_topic<-function(k,nt,lda){
  apps.terms<-terms(lda,nt)
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
  return(topicmap)
}
default_lda<- LDA(dtm, 5,method= "Gibbs",control = list(iter = 100))

shinyServer(
  function(input, output) {
    observeEvent(input$learn,{
      default_lda<<- LDA(dtm,input$k,method= "Gibbs",control = list(iter = input$iter))
    })
    
    topic_terms<- reactive({
      nt<-input$nt
      k<-default_lda@k
      apps.terms<-terms(default_lda,nt)
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
      return(topicmap)
    })
    output$topic_terms_table<-renderTable({
      #default lda call. k=5,nt=5,iter=100
      nt<-input$nt
      if(input$learn==0){
        default_topic_term<-default_topic(5,input$nt,default_lda)
        
      }else{
        topic_terms()
      }
      
    }, include.rownames=FALSE)
    doc_topic<-eventReactive(input$learn,{
      
      k<-default_lda@k
      apps.topics=posterior(default_lda,dtm)$topics
      df.apps.topics=as.data.frame(apps.topics)
      df.apps.topics = cbind(userid=dataframe[,1], df.apps.topics, stringsAsFactors=F)
      #get the doc and its topic
      topics<- apply(df.apps.topics[,c(2:(k+1))], 1, which.max)
      doc_topics<-cbind(dataframe[,1],topics)
    })
     topic_count <- reactive({
      if(input$learn==0){
        apps.topics=posterior(default_lda,dtm)$topics
        df.apps.topics=as.data.frame(apps.topics)
        df.apps.topics = cbind(userid=dataframe[,1], df.apps.topics, stringsAsFactors=F)
        #get the doc and its topic
        topics<- apply(df.apps.topics[,c(2:6)], 1, which.max)
        doc_topics<-cbind(dataframe[,1],topics)
      }else{
        doc_topics<-doc_topic()
      }
      topics<-data.frame(table(doc_topics[,2]))
      names(topics)<-c("topic_id","count")
      #recharts.init()	    
      #eBar(topics)
      topics%>%ggvis(x = ~topic_id,y= ~count) %>% layer_bars(fillOpacity := 0.5)%>% add_axis("x",title="Topic Number")%>%add_axis("y",title="Device Count")
    })
     topic_count%>%bind_shiny("topic_count")
     
  }
  
    
)