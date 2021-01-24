library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(DT)
library(wordcloud)
library(tm)
library(vroom) 
library(here)
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(syuzhet)

covidata <- read_csv("D:/Code/R STUDIO/PDS/finalSentimentdata2.csv")
covidata <- as.data.frame(covidata)
class(covidata)

deretan <- covidata %>% 
  filter(complete.cases(.)) %>% 
  group_by(text,sentiment) %>% 
  ungroup() %>% 
  select(text, sentiment)

anger <- sum(covidata$sentiment == "anger")
fear <- sum(covidata$sentiment == "fear")
joy <- sum(covidata$sentiment == "joy")

ui <- shinyUI(
  dashboardPagePlus(
    skin = "black-light",
    title = "Covid",
    dashboardHeaderPlus(
      
      title = "Covid Sentiment"
      
    ),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem(
          text = "Prak DS", 
          tabName = "page1",
          badgeColor = "green"
        )
      )
      
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          
          fluidRow(
            boxPlus(
              title = "", 
              closable = TRUE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "anger", 
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "fear",
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "joy",
                width = 4
              )
            )
          ),
          
          
          fluidRow(
            
            boxPlus(
              title = "DAFTAR SENTIMENT USER", 
              closable = TRUE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 3,
              height = "600px",
              
              dataTableOutput(
                outputId = "deretan"
              )
            ),
          fluidPage(
            
            boxPlus(
              title = "WordCloud", 
              closable = TRUE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 9,
              height = "600px",
              
              plotOutput(
                outputId = "graph"
              )
            ),
            boxPlus(
              title = "Grafik", 
              closable = TRUE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              height = "600px",
              
              plotOutput(
                outputId = "grafik"
              )
            )
          )
          )
        )
      )
    )
  )
)

server <- shinyServer(
  function(input, output){
    
    
    
    output$anger <- renderInfoBox({
      
      infoBox(title = "anger",
              value = anger, 
              subtitle = "Data yang mempunyai sentiment Anger", 
              color = "red",
              fill = TRUE)
      
    })
    
    output$fear <- renderInfoBox({
      
      infoBox(title = "fear",
              value = fear,
              subtitle = "Data yang mempunyai sentiment Fear",
              color = "orange",
              fill = TRUE)
      
    })
    
    output$joy <- renderInfoBox({
      
      infoBox(title = "joy", 
              value = joy,
              subtitle = "Data yang mempunyai sentiment Joy",
              color = "green",
              fill = TRUE)
      
    })
    
    
    
    output$deretan <- renderDataTable({
      datatable(deretan)
      
      
    })
    output$graph <- renderPlot({
    
    set.seed(20)
    covidata<-covidata[sample(nrow(covidata)),]
    covidata<-covidata[sample(nrow(covidata)),]
    
    corpus<-Corpus(VectorSource(covidata$text))
    corpus
    inspect(corpus[1:10])
    
    corpus.clean<-corpus%>%
      tm_map(content_transformer(tolower))%>%
      tm_map(removePunctuation)%>%
      tm_map(removeNumbers)%>%
      tm_map(removeWords,stopwords(kind="en"))%>%
      tm_map(stripWhitespace)
    dtm<-DocumentTermMatrix(corpus.clean)
    
    inspect(dtm[1:10,1:20])
    
    covidata.train<-covidata[1:50,]
    covidata.test<-covidata[51:100,]
    
    dtm.train<-dtm[1:50,]
    dtm.test<-dtm[51:100,]
    
    corpus.clean.train<-corpus.clean[1:50]
    corpus.clean.test<-corpus.clean[51:100]
    
    dim(dtm.train)
    fivefreq<-findFreqTerms(dtm.train,5)
    length(fivefreq)
    
    dtm.train.nb<-DocumentTermMatrix(corpus.clean.train,control = list(dictionary=fivefreq))
    
    dtm.test.nb<-DocumentTermMatrix(corpus.clean.test,control = list(dictionary=fivefreq))
    
    dim(dtm.test.nb)
    
    convert_count <- function(x){
      y<-ifelse(x>0,1,0)
      y<-factor(y,levels=c(0,1),labels=c("no","yes"))
      y
    }
    
    trainNB<-apply(dtm.train.nb,2,convert_count)
    testNB<-apply(dtm.test.nb,1,convert_count)
    
    wordcloud(corpus.clean,
              min.freq = 4,
              max.words=100,
              random.order=F,
              colors=brewer.pal(8,"Dark2"))
    })
    output$grafik <- renderPlot({
    
    kopit<-read.csv("D:/Code/R STUDIO/PDS/finalSentimentdata2.csv",stringsAsFactors = FALSE)

    review <-as.character(kopit$text)
    
    s<-get_nrc_sentiment(review)
    
    review_combine<-cbind(kopit$text)
    par(mar=rep(3,4))
    a<- barplot(colSums(s),col=rainbow(10),ylab='count',main='sentiment analisis')
    bagan <- a
    })
    
  }
)


shinyApp(ui = ui, server)

