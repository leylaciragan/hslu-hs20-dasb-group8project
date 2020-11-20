# Load R packages
library(shiny)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(DT)
library(dygraphs)
library(xts)
library(htmltools)
library(dplyr)
library(magrittr)
library(data.table)


# import data. Disable 'check.names' to prevent shiny from adding an X prefix.
applications <- read.csv("data/AsylgesuchePerNation1986.csv", sep=";", encoding="UTF-8",  header = TRUE, check.names = FALSE)
# applis_trnspsd is used for time series tab
applis_trnspsd <- read.csv("data/appli_transposed.csv", sep=";", encoding="UTF-8",  header = TRUE, check.names = FALSE)

# Countries and map are used on map tab
countries <- geojson_read("data/countries.geo.json", what = "sp") # does not make nested data flat
countries_flat <- fromJSON("data/countries.geo.json") # reads nested data flat, but still only two columns see https://hendrikvanb.gitlab.io/2018/07/nested_data-json_to_tibble/#:~:text=To%20summarise%3A%20the%20basic%20steps,empty)
map <- leaflet(countries)

# Define UI
ui <- fluidPage(
  #shinythemes::themeSelector(),
  
  navbarPage(
    
    #Tab 1
    "Applications for asylum in Switzerland",
    tabPanel(
      "By Year: Map View",
          # Add slider input named 'year' to select years (1986 - 2020)
          sliderInput(inputId = 'application_year', label = 'Select Year', min = 1986, max = 2020, value = 1986, sep = "", width = '100%'),
          h1("Map overview"),
          h4("Number of applications by country of origin"),
        leafletOutput(outputId = "map"),
    ),
    
  # Tab 2
  tabPanel(
    "By country: Timetrend", 
    "See how the number of applications has changed over time.",
   
    sidebarLayout(
      sidebarPanel(
        
       # Select country of origin to plot
        selectInput(inputId = "Country", 
                    label = "Choose country", 
                    c(sort(applications$Country)), 
                    selected = "Georgia", multiple = TRUE),
      ),
      mainPanel(
        #plotOutput as dygraph
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
  ### START: Tab 5
  tabPanel("Top and Bottom Countries", 
           
           # 1. Row
           fluidRow(column(1,),column(10,
                                      # Add slider input named 'year' to select years (1986 - 2020)
                                      sliderInput(inputId = 'year', label = 'Select Year', min = 1986, max = 2020, value = 1986, sep = "", width = '100%'),
           ),
           column(1,)),
           
           # 2. Row
           fluidRow(column(1,),column(5,
                                      "Top application countries",
                                      tableOutput('top10')
           ),column(5,
                    "Least application countries",
                    tableOutput('flop10')
           ),column(1,)),),
  ### END: Tab 5
  
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
  
   # output tab 1: map with Leaflet
  
  # prepare and filter application data for map
  # TODO change data to the flat countries_flat list
  applications_tb <- as_tibble(applications) # this is the wrong data set, had some trouble with the nested json
  applications_tb2 <- as_tibble(countries)
  colnames(applications_tb) <- c("Code","Country","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
  #applications_filtered <- select(applications_tb, Country, toString(input@application_year))
  
  # return only data selected by reactive input
  # TODO change data to the flat countries_flat list
  numbers_selected <- reactive({
    numbers_selected <- select(applications_tb, Country, toString(input$application_year))
    names(numbers_selected)[2] <- "year_selected" #change column name to a fixed name 
    print(glimpse(numbers_selected)) # debug only: prints the reactive output to console -> correctly prints out the values of the selected year
    return(numbers_selected)
  })
  
  output$map <- renderLeaflet({
    # color palette
    pal <- colorNumeric(
      palette = "YlGnBu", # other palettes for example: GnBu, OrRd, YlOrBr, see https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=5
      domain = countries$pop_est)
    
    pal2 <- colorNumeric(
      palette = "GnBu", 
      domain = countries$gdp_md_est)
    
    pal3 <- colorBin(
      palette = "Reds",
      bins = c(0, 250, 1000, 5000, 20000, 25000),
      domain = numbers_selected()$year_selected)
    
    labels <- sprintf(
      "<strong>Country: %s</strong><br/><br/>
    Postal Code: %s<br/>
    Population: %g<br/>
    GDP: %g<br/>
    Economical Status: %s<br/>",
      countries$formal_en, countries$postal, countries$pop_est, countries$gdp_md_est, countries$economy
    ) %>% 
      lapply(htmltools::HTML)
    
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
      # TODO how can I prevent re-loading the ones for gdp and population?
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
      addPolygons(
        group = "Countries: Applications", 
        data = countries, 
        label = labels, 
        weight = 1, 
        color = "White", 
        opacity = 1, 
        fillColor = ~pal3(numbers_selected()$year_selected),
        fillOpacity = 0.7) %>%
      addLayersControl(
        baseGroups = c("OSM", "Toner", "Topo"),
        overlayGroups = c("Countries: Population", "Countries: GDP", "Countries: Applications"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  
  # output tab 2: time trend with dygraphs
  # # Subset data for time series
  selected_countries <- reactive({
    
   # convert to data table to ease later conversion to xts, as described here:
   # https://stackoverflow.com/questions/4297231/converting-a-data-frame-to-xts
   tbl = as.data.table(applis_trnspsd)
   tbl <- select(tbl, Time, input$Country)
   #convert data table to xts format as described here:
   # https://stackoverflow.com/questions/23224142/converting-data-frame-to-xts-order-by-requires-an-appropriate-time-based-object
   qxts <- xts(tbl[, -1], order.by=as.POSIXct(tbl$`Time`))
   qxts
  })
  
  # rendering our dygrpah
  ## code copied from here:
  ## https://rstudio.github.io/dygraphs/index.html
  output$timetrend <- renderDygraph({
    dygraph(selected_countries(), main = "No. of applications by country of origin") %>%
    dyRangeSelector(height = 20) %>%
    dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
  })

  # output$lineplot <- renderPlot({
  #   color = "#434343"
  #   par(mar = c(4, 4, 1, 1))
  #   plot(x = selected_countries()$Country, y = selected_countries()$close, type = "l",
  #        xlab = "Date", ylab = "Number of applications", col = color, fg = color, col.lab = color, col.axis = color)
  # })
  
  # TODO: output tab 3: correlation
  
  # TODO: output tab 4: forecast
  
  ### START: Output Tab 5
  output$top10 <- renderTable({
    # edit the dataset to fit the table
    A <- applications
    colnames(A) <- c("Code","Country","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
    A[A$Code  == "Stateless", "Country"]<- "Stateless"
    A[A$Code  == "Without Nationality", "Country"]<- "Without Nationality"
    A[A$Code  == "State unknown", "Country"]<- "State unknown"
    A[A$Code  == "Without declaration", "Country"]<- "Without declaration"
    #delete Code column
    A$Code <- NULL
    #select Input country
    B <- select(A, Country, toString(input$year))
    #filter all with 0 values
    C <- filter(B, B[toString(input$year)] > "0")
    #order
    D <- C[order(-C[toString(input$year)]),]
    E <- D[1:10,]
    E
  })
  
  output$flop10 <- renderTable({
    # edit the dataset to fit the table
    A <- applications
    colnames(A) <- c("Code","Country","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
    A[A$Code  == "Stateless", "Country"]<- "Stateless"
    A[A$Code  == "Without Nationality", "Country"]<- "Without Nationality"
    A[A$Code  == "State unknown", "Country"]<- "State unknown"
    A[A$Code  == "Without declaration", "Country"]<- "Without declaration"
    #delete Code column
    A$Code <- NULL
    #select Input country
    B <- select(A, Country, toString(input$year))
    #filter all with 0 values
    C <- filter(B, B[toString(input$year)] > "0")
    #oder
    D <- C[order(C[toString(input$year)]),]
    E <- D[1:10,]
    E
  })
  
  data <- reactive ({
    rnorm(input$year)
  })
  
  ### END: Output Tab 5

  # output tab 6: data table with DT
  output$datatable <- renderDT(applications)
  
  # TODO is this from an example? Who fixes it?
  # output tab 6: download data as csv 
  # output$downloadCsv <- downloadHandler(
  #   filename = function() {
  #     paste("refugee_data", applications$date[1], ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(applications %>% select(c(Country, 1986, 1987)), file)
  #   }
  # )

  ### END: Output Tab 6
  
} # end server


# Create Shiny object
shinyApp(ui = ui, server = server)
