library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gridExtra)
library(shiny)

ui <- shinyUI(fluidPage(
   
   titlePanel("Candlestick Charts"),
   
  sidebarLayout(
      sidebarPanel(
         sliderInput("days",
                     "Number of days:",
                     min = 5,
                     max = 100,
                     value = 10)
      ),
      
      # 
      mainPanel(
         plotOutput("Plot1"),
         plotlyOutput("Plot2"),
         tableOutput("Data")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {
  #
  myData <- reactiveVal()
  
  #
  getdata <- function(Days=input$days){
  Access_Token <- readRDS("Access_Token")
  Environment <- "https://api-fxpractice.oanda.com"
  Endpoint <- "v3/instruments"
  Instrument <- "EUR_USD"
  surl <- paste("candles?count=",Days,"&price=M&granularity=D",sep="")
  url <- paste(Environment,Endpoint,Instrument,surl,sep="/")
  r <- GET(url,
           add_headers('Content-Type'='application/json',
                       'Authorization' = paste('Bearer', Access_Token)
           )
  )
  fromJSON(toJSON(content(r))) -> data
  
  data.frame(time=as.POSIXct(fromJSON(toJSON(data$candles$time)),"%Y-%m-%dT%H:%M:%S", tz="UTC"),
             volume=fromJSON(toJSON(data$candles$volume)),
             o=as.numeric(fromJSON(toJSON(data$candles$mid$o))),
             h=as.numeric(fromJSON(toJSON(data$candles$mid$h))),
             l=as.numeric(fromJSON(toJSON(data$candles$mid$l))),
             c=as.numeric(fromJSON(toJSON(data$candles$mid$c)))) -> plotdata
  plotdata
  }
  
  
  #
  observe({
    invalidateLater(2000, session)
    isolate({    
      first_data <- getdata()
      new_data <- distinct(bind_rows(first_data,getdata()))[which(distinct(bind_rows(first_data,getdata()))==setdiff(getdata()$time,first_data)),]
      if(is.null(myData()))
        myData(first_data)
      else
        myData(new_data)
    })
  })
  
  
  #
   output$Plot1 <- renderPlot({
     plotdata <- myData()
     candlestickp <- plotdata %>%
       rowwise() %>% 
       mutate( 
         mid=(o+c)/2,
         barmax=max(o,c),
         barmin=min(o,c),
         shadowmax=max(h,l),
         shadowmin=min(h,l),
         barcol=ifelse(o<c,'up','down')
       ) %>%  
       ggplot(aes(x=time,
                  y=mid,
                  color=barcol,
                  fill=barcol)) + 
       geom_boxplot(aes(ymin=shadowmin,
                        ymax=shadowmax,
                        lower=barmin,
                        upper=barmax,
                        middle=mid), 
                    stat = "identity") + 
       theme(legend.position="none") +
       theme(axis.title = element_blank())
     
     volumep <- plotdata %>%
       rowwise() %>% 
       mutate( 
         mid=(o+c)/2,
         barmax=max(o,c),
         barmin=min(o,c),
         shadowmax=max(h,l),
         shadowmin=min(h,l),
         barcol=ifelse(o<c,'up','down')
       ) %>%  
       ggplot(aes(x=time, y=volume,color=barcol,fill=barcol)) +
       geom_bar(stat = "identity") + 
       theme(legend.position="none")
     
     #
     ggpubr::ggarrange(candlestickp,volumep, 
                       align = "v",
                       ncol = 1, nrow = 2,
                       heights = c(0.8,0.2))
   })
   
   output$Plot2 <- renderPlotly({
     plotdata <- myData()
     #plotly
     candlestickp <- plotdata %>%
       plot_ly(x = ~time, type="candlestick",
               open = ~o, close = ~c,
               high = ~h, low = ~l) %>%
       layout(title = "Basic Candlestick Chart",
              xaxis = list(rangeslider = list(visible = F)))
     
     volumep <- plotdata %>%
       rowwise() %>% 
       mutate( 
         mid=(o+c)/2,
         barmax=max(o,c),
         barmin=min(o,c),
         shadowmax=max(h,l),
         shadowmin=min(h,l),
         barcol=ifelse(o<c,'up','down')
       ) %>%
       plot_ly(x=~time, y=~volume, type='bar', name = "",
               color = ~barcol, colors = c("#FF4136","#3D9970")) %>%
       layout(yaxis = list(title = "Volume"))
     
     subplot(candlestickp, 
             volumep, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
       layout(showlegend = F)
   })
   
   output$Data <- renderTable({
     myData()
     time<-data.frame(time=strftime(myData()$time,"%Y-%m-%d %H:%M:%S", tz="UTC"))
     c(time,myData() %>% select(c(2:6)))
   })
   
   
})

# Run the application 
shinyApp(ui = ui, server = server)

