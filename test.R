# Load R packages
library(shiny)
library(shinythemes)
#library(leaflet)
#library(geojsonio)
library(DT)
library(dygraphs)
library(xts)
library(htmltools)
library(dplyr) 


### START: IMPORT DATA

#applications <- read.csv("C:/Users/Lukas/Documents/gitroot/dasb_team08/data/AsylgesuchePerNation1986.csv", sep =";", encoding="UTF-8")
applications <- read.csv("data/AsylgesuchePerNation1986.csv", sep=";", encoding="UTF-8")

### END: IMPORT DATA


### things for me
# select & filter for tab 5 - cause it's not working anywhere else
# sx_1986 <- select(applications, Country:X1986)
# fx_1986 <- filter(sx_1986, X1986 > 0)



### START: Define UI
##  START: fluidPage
ui <- fluidPage(
  ## Tab 5
  tabPanel("Top and Bottom Countries", 
           
  # 1. Row
    fluidRow(column(1,),column(10,
        # Add slider input named 'year' to select years (1986 - 2020)
        sliderInput(inputId = 'year', label = 'Select Year', min = 1986, max = 2020, value = 1986, sep = "", width = '100%'),
        
        # Output in Table
        # tableOutput("somebs2")
      ),
      column(1,)),
  
  # 2. Row
    fluidRow(column(1,),column(5,
             "Top application countries",
             tableOutput('somebs3')
      ),column(5,
             "Least application countries",
             #tableOutput('somebs3')
      ),column(1,)),),
) 
##  END: fluidPage
### END: Define UI


### START: server function  
server <- function(input, output) {
  # example
  output$somebs3 <- renderTable({
    # edit the dataset to fit the table
    #applications <- read.csv("C:/Users/Lukas/Documents/gitroot/dasb_team08/data/AsylgesuchePerNation1986.csv", sep =";", encoding="UTF-8")
    A <- applications
    colnames(A) <- c("Code","Country","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
    A[A$Code  == "Stateless", "Country"]<- "Stateless"
    A[A$Code  == "Without Nationality", "Country"]<- "Without Nationality"
    A[A$Code  == "State unknown", "Country"]<- "State unknown"
    A[A$Code  == "Without declaration", "Country"]<- "Without declaration"
    A$Code <- NULL
    A
    #B <- select(A, Country:X1986)
    #C <- filter(B, X1986 > 0 & Country != 0 & Country != 24 & Country != 5)
    #D <- C[with(C,order(-X1986)),]
    #E <- D[1:10,]
    
  } )
  
  output$table <- renderDataTable({ 
    df <- data.frame(A = 1:20, B = 20:1)
    
    df <- df[order(df[[input$ordering]]), ]
    
    df_new <- rbind(head(df,1), tail(df,1))
    df_new
  },
  options = list(pageLength = 10))
  
  
  data <- reactive ({
    rnorm(input$year)
  })
  #top
  #output$dt_1986 <- renderDT(fx_1986)
  
  #least
  output$hist <- renderPlot({
    title <- "some bs"
    hist(rnorm(data()), main = title)
  })
  
  output$stats <- renderPrint({
    summary(rnorm(data()))
  })
  
  output$somebs2 <- renderTable(iris)
  
} 
### END: server function


### START: Create Shiny object
shinyApp(ui = ui, server = server)
### END: Create Shiny object