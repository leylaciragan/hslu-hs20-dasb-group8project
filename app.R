
####################################
# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html
####################################


# Load R packages
library(shiny)
library(shinythemes)

# import data
applications = read.csv("AsylgesuchePerNation1986.xlsx")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Applications for asylum in Switzerland",
                  tabPanel("By Year: Map View",
                           sidebarPanel(
                             tags$h3("Input:"),
                             sliderInput("slider", "Choose year:", 1986, 2020, 1997),
                             
                            # sidebarPanel
                          # pickerInput("region_select", "Country:",   
                           #            choices = as.character(applications[order(-applications$Country),]$Country), 
                            #           options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             #          selected = as.character(applications[order(-applications$Country),]$Country)[1:10],
                              #         multiple = TRUE), 
                           ),
                           mainPanel(
                             h1("Map overview"),
                             
                             h4("Number of applications by country of origin"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("By country: Histogram", "This panel is intentionally left blank"),
                  tabPanel("Data",
                           numericInput("maxrows", "Rows to show", 25),
                           verbatimTextOutput("rawtable"),
                           downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                           "Data published by ", tags$a(href="https://www.sem.admin.ch/sem/fr/home/publiservice/statistik/asylstatistik/uebersichten.html", 
                                                                              "Federal statistical office of Switzerland.")
                  )
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("refugee_data", applications$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(applications %>% select(c(Country, 1986, 1987)), file)
    }
  )
  
  output$rawtable <- renderPrint({
  orig <- options(width = 1000)
  print(tail(applications %>% select(c(Country, 1986, 1987)), input$maxrows), row.names = FALSE)
  options(orig)
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)