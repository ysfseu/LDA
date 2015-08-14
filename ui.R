library(ggvis)
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("LAD Test Platform"),
    sidebarPanel(
      sliderInput("k","Number of Topic",min = 3,max = 12,value=5), 
      selectInput("iter", "Number of Iteration", choices = list(100,200,300,400,500,600,700,800,900,1000), selected = 1),
      actionButton("learn","RUN"),
      sliderInput("nt","Top N terms for each Topic",min = 1,max = 50,value=5)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Topics-Terms",tableOutput("topic_terms_table")),
        tabPanel("Docs-Topics", ggvisOutput("topic_count"))
                 #htmlOutput("rechartPie1"))
        
      )
      
      
    )
  )
)