
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
    "Correlation of country and conflict", 
    "This panel is intentionally left blank: it should contain a correlation plot/table produced by Input: country, conflict, Input: year",
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
    "Data and Data Cleaning",
    "The following data was used in our project:",
    tags$br(),
    tags$br(),
    tags$b("1. Asylgesuche nach Nationen (1986 bis 2020). "), 
    "Data published by ",
    tags$a(href="https://www.sem.admin.ch/sem/fr/home/publiservice/statistik/asylstatistik/uebersichten.html",
           "Federal statistical office of Switzerland."),
    tags$p("To this data set we added a column with the countries postal codes, so we could map the data with other datasets."),
    tags$br(),
    tags$br(),
    DTOutput(outputId = "datatable"),
    verbatimTextOutput("rawtable"),
    downloadButton("downloadCsv", 
                   "Download as CSV"),
    tags$br(),
    tags$br(),
    tags$b("2. countries.geo.json. "), 
    "Data published by ",
    tags$a(href="https://github.com/johan/world.geo.json/blob/master/countries.geo.json",
           "Johan on GitHub."),
    tags$p("We cleaned out all redundant and unnecessary data from this dataset (e.g. redundant country names, codes, etc."),
    tags$br(),
    tags$br()
           ), # end tab 6
  
  # Tab 7
  tabPanel(
    "About this project", 
    
    # Dear team, feel free to change the texts, I'm feeling a little cheesy today (Leyla)
    tags$p("With COVID19 and US elections predominant in the media in 2020 we asked ourselves, what had happened to the refugees in this world. After camp Moria burning down in autumn 2020, we decided to focus our attention to this 'forgotten' topic."),
    tags$p("Narrowing down the area of our research question, we looked for numbers on asylum seekers to Switzerland. With datasets on asylum seekers in Switzerland and others on world conflicts, we try to visualize the bare numbers, but also to investigate possible conncections."),
    tags$p(
    tags$b("Contributors:"),
    tags$li("Leyla, Ciragan"),
    tags$li("Tariq Ghazzawi"),
    tags$li("Lukas Ingold"),
    tags$li("Gabriela Moos"),
    tags$li("Urs Stadelmann")
    ),
    tags$h4("We will not forget."), 
    img(src = "moria.jpeg", width = "844px", height = "475px"),
    tags$legend("Migrants flee the flames at Moria camp | Photo: Picture-alliance/dpa/S.Baltagiannis", 
                tags$a(href="https://www.infomigrants.net/en/post/27165/fresh-fires-burn-at-greece-s-largest-refugee-camp-moria")
                )
  ) # end tab 7
  ) # end navbarPage
  ) # end fluidPage


# Define server function  
server <- function(input, output) {
  
  # color palette
  pal <- colorNumeric(
    palette = "YlGnBu", # other palettes for example: GnBu, OrRd, YlOrBr, see https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=5
    domain = countries$pop_est)
  
  pal2 <- colorNumeric(
    palette = "GnBu", # other palettes for example: GnBu, OrRd, YlOrBr, see https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=5
    domain = countries$gdp_md_est)
  
  labels <- sprintf(
    "<strong>Country: %s</strong><br/><br/>
    Postal Code: %s<br/>
    Population: %g<br/>
    GDP: %g<br/>
    Economical Status: %s",
    countries$formal_en, countries$postal, countries$pop_est, countries$gdp_md_est, countries$economy
  ) %>% 
    lapply(htmltools::HTML)
  
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
      

      # Overlay groups for map$
      # TODO add an overlay for number of applications
      addPolygons(
        group = "Countries: Population", 
        data = countries, 
        label = labels, 
        weight = 1, 
        color = "White", 
        opacity = 1, 
        fillColor = ~pal(countries$pop_est), 
        fillOpacity = 0.7) %>%
      addPolygons(
        group = "Countries: GDP", 
        data = countries, 
        label = labels, 
        weight = 1, 
        color = "White", 
        opacity = 1, 
        fillColor = ~pal2(countries$gdp_md_est), 
        fillOpacity = 0.7) %>%
      addLayersControl(
        baseGroups = c("OSM", "Toner", "Topo"),
        overlayGroups = c("Countries: Population", "Countries: GDP"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  # TODO: output tab 2: timetrend with dygraphs

  # TODO: output tab 3: correlation
  
  # TODO: output tab 4: forecast
  
  # TODO: output tab 5: top & bottom countries
  

  # output tab 6: data table with DT
  output$datatable <- renderDT(applications)
  
  # output tab 6: download data as csv: TODO is this from an example? Who fixes it?
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("refugee_data", applications$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(applications %>% select(c(Country, 1986, 1987)), file)
    }
  )

} # end server


# Create Shiny object
shinyApp(ui = ui, server = server)