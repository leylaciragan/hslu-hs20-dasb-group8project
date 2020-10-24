
####################################
# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html
####################################


# Load R packages
library(shiny)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(DT)
library(dygraphs)
library(xts)
library(htmltools)

# import data
applications = read.csv("data/AsylgesuchePerNation1986.csv", sep=";")
countries <- geojson_read("data/countries.geo.json", what = "sp")
map <- leaflet(countries)


# Define UI
ui <- fluidPage(
  shinythemes::themeSelector(),
  
  navbarPage(
    
    #Tab 1
    "Applications for asylum in Switzerland",
    tabPanel(
      "By Year: Map View",
          tags$h3("Input:"),
          sliderInput("slider", "Choose year:", 1986, 2020, 1997),
          # sidebarPanel
          # pickerInput(
          #"region_select",
          #"Country:",
          # choices = as.character(applications[order(-applications$Country),]$Country),
          # options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
          # selected = as.character(applications[order(-applications$Country),]$Country)[1:10],
          # multiple = TRUE),
          h1("Map overview"),
          h4("Number of applications by country of origin"),
        leafletOutput(outputId = "map")
    ),
    
  # Tab 2
  tabPanel(
    "By country: Histogram", 
    "This panel is intentionally left blank: it should contain a histogram or timetrend produced by Input: country, range slider: year",
    sidebarLayout(
      sidebarPanel(
        p("some timetrend stuff explanation")
      ),
      mainPanel(
        dygraphOutput(outputId = "timetrend")
      )
    )
  ),
  
  # Tab 3
  tabPanel(
    "Correlation of two countries", 
    "This panel is intentionally left blank: it should contain a correlation plot/table produced by Input: country1, country 2, Input: year",
    sidebarLayout(
      sidebarPanel(
        
      ),
      mainPanel(
        
      )
    )
    ),


  # Tab 4
  tabPanel(
    "Forecast", 
    "This panel is intentionally left blank: it should contain a forecast produced by Input: country. Sorry: even if this is a really stupid research question, I think we should try.....",
    sidebarLayout(
      sidebarPanel(
        
      ),
      mainPanel(
        
      )
    )
  ),
  
  # Tab 5
  tabPanel(
    "Top and Bottom n Countries", 
    "This panel is intentionally left blank: it should contain a List produced by Input: top countries, Input: Bottom countries, range slider: year",
    sidebarLayout(
      sidebarPanel(
        
      ),
      mainPanel(
        
      )
    )
  ),
  
  # Tab 6
  tabPanel(
    "Data",
    "On this page you can check the original data used in this project.",
    DTOutput(outputId = "datatable"),
    verbatimTextOutput("rawtable"),
    downloadButton("downloadCsv", 
                   "Download as CSV"),
    tags$br(),
    tags$br(),
    "Data published by ",
    tags$a(href="https://www.sem.admin.ch/sem/fr/home/publiservice/statistik/asylstatistik/uebersichten.html",
           "Federal statistical office of Switzerland.")
           )
  ) # end navbarPage
  ) # end fluidPage


# Define server function  
server <- function(input, output) {
  
  # output tab 1: map with Leaflet
  
  
  output$map <- renderLeaflet({
    leaflet(countries) %>%
      setView(lng = 8.55, lat = 30, zoom = 2) %>%
      # Base group maps
      addTiles(group = "OSM") %>%
      addProviderTiles("Stamen.TonerLite",
                       group = "Toner", 
                       options = providerTileOptions(minzoom = 1, maxzoom = 10)) %>%
      addProviderTiles("Esri.WorldTopoMap",    
                       group = "Topo") %>% 
      # Overlay groups for map
      addPolygons(
        group = "Countries", 
        data = countries, 
        label = ~htmlEscape(formal_en), # TODO add some styling and more fields from countries.json like: pop_est, gdp_est, economy, income_grp
        weight = 1, 
        color = "White", 
        opacity = 1, 
        fillColor = "Red", # TODO should be depending on number of applications
        fillOpacity = 0.1) %>%
      addLayersControl(
        baseGroups = c("OSM", "Toner", "Topo"),
        overlayGroups = c("Countries"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  # TODO: output tab 2: timetrend with dygraphs

  # TODO: output tab 3: correlation
  
  # TODO: output tab 4: forecast
  
  # TODO: output tab 5: top & bottom countries
  
    # TODO what is this doing?
  # output$txtout <- renderText({
  #   paste( input$txt1, input$txt2, sep = " " )
  # })
  
  # output tab 6: data table with DT
  output$datatable <- renderDT(applications)
  
  # output tab 6: download data as csv: TODO does this work?
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("refugee_data", applications$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(applications %>% select(c(Country, 1986, 1987)), file)
    }
  )

  # TODO keep either this table (not working yet) or datatable with renderDT
  # output$rawtable <- renderPrint({
  # orig <- options(width = 1000)
  # print(tail(applications %>% select(c(Country, 1986, 1987)), input$maxrows), row.names = FALSE)
  # options(orig)
  # })
} # end server


# Create Shiny object
shinyApp(ui = ui, server = server)