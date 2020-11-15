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

applications <- read.csv("data/AsylgesuchePerNation1986.csv", sep=";", encoding="UTF-8")

### END: IMPORT DATA


### things for me
# select & filter for tab 5 - cause it's not working anywhere else
sx_1986 <- select(applications, Country:X1986)
fx_1986 <- filter(sx_1986, X1986 > 0)



### START: Define UI
##  START: fluidPage
ui <- fluidPage(
  #shinythemes::themeSelector(),
   
     # Tab 5
    tabPanel(
      "Top and Bottom Countries", 
      fluidRow(
        column(1,
        ),
        column(10,
               # Add slider input named 'year' to select years (1986 - 2020)
               sliderInput(inputId = 'year', label = 'Select Year', 
                           min = 1986, max = 2020, value = 1986, 
                              sep = "", width = '100%'),
                
               tableOutput("somebs2")
        ),
        column(1,
        )
      ),
      fluidRow(
        column(1,
        ),
        column(5,
               "Top application countries",
               DTOutput(outputId = "dt_1986")
        ),
        column(5,
               "Least application countries",
               plotOutput("hist"),
               #View(fx_1986)
        ),
        column(1,
        )
      ),
    ),
    
) 
##  END: fluidPage
### END: Define UI


### START: server function  
server <- function(input, output) {
  data <- reactive ({
    rnorm(input$year)
  })
  #top
  output$dt_1986 <- renderDT(fx_1986)
  
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