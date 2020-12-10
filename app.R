# Load R packages
library(shiny)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(jsonlite)
library(DT)
library(dygraphs)
library(xts)
library(htmltools)
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(reshape2)

# import data. Disable 'check.names' to prevent shiny from adding an X prefix.
applications <- read.csv("data/AsylgesuchePerNation1986.csv", sep=";", encoding="UTF-8",  header = TRUE, check.names = FALSE)
# applis_trnspsd is used for time series tab
applis_trnspsd <- read.csv("data/appli_transposed.csv", sep=";", encoding="UTF-8",  header = TRUE, check.names = FALSE)
# happiness is used for tab 3
happiness_combined <- read.csv("data/happy_transposed.csv", sep=";", encoding="UTF-8", header=TRUE, check.names = FALSE)
happy_countries <- read.csv("data/Happiness/combined_cleaned.csv", sep=",", encoding="UTF-8", header=TRUE, check.names = FALSE)
happiness_correlation <- read.csv("data/Happiness/Correlation/Useable/CSV_File_2019.csv", sep=",", encoding="UTF-8", header = TRUE, check.names = FALSE)
happy_countries2019 <- read.csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/happy_2019.csv", sep=",", encoding="UTF-8", header = TRUE, check.names = FALSE)
happiness_combined_ascending <- read.csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/Correlation/Useable/Excel_Ascending2.csv", sep=",", encoding="UTF-8", header=TRUE, check.names = FALSE)

# Countries and map are used on map tab
countries <- geojson_read("data/countries.geo.json", what = "sp") # for country polygons, gdp_md_est and pop_est
countries_csv <- read.csv("data/countries.csv", sep=";", encoding="UTF-8", header = TRUE, check.names = FALSE) # for application numbers per year
map <- leaflet(countries)

# Define UI
ui <- fluidPage(
  #shinythemes::themeSelector(),
  
  navbarPage(
    
    ### START: Tab 1
    "Applications for asylum in Switzerland",
    tabPanel(
      "Map overview",
      h3("Number of applications by year and country of origin"),
      p("This map shows asylum application numbers to Switzerland. Please select a year from the slider to get the respective application numbers for each country."),
      sliderInput(inputId = 'application_year', label = 'Select Year', min = 1986, max = 2020, value = 1986, sep = "", width = '100%'),
      leafletOutput(outputId = "map"),
      tags$br(),
      tags$br(),
      tags$h4("GDP or Population in 2016"),
      tags$p("Our countries.geo.json file (see tab Data and Data cleaning) contained some additional properties like GDP, info on economics - but only for the year 2016. This map is  for illustration purposes only and shows only GDP and population number in the year 2016."),
      leafletOutput(outputId = "map2")
    ),
    ### END: Tab 1
    
  ### START: Tab 2
  tabPanel(
    "Time trend",
    h1("Numbers of applications by country of origin, from 1986 to 2020"),
    p("Exploring the data by comparing the time trends of different countries might reveal some surprising insights."),
    p("Whether a comparison is interesting depends on one's expectation, of course. 
      We listed some combinations that seemed interesting to us. Your mileage may vary."),
    strong("Syria, Afghanistan and Eritrea"),
    p("A comparison between Syria, Afghanistan and Eritrea shows a dramatic increase in numbers 
    of applications in 2015. This was expected for Syria. 
      What was surprising to us, was the drastic drop in applications after 2015. 
      Another surprise was to see that so many more people applied in 2015 from Eritrea and Afghanistan than from Syria."),
    strong("North Macedonia, Albania, Bosnia & Herzegovina, Serbia"),
    p("Some of us were rather surprised to see that so many more people had applied from Serbia than from Bosnia & Herzegovina."),
    strong("Ready to explore some more?"),
    p("Below, a few combinations we found interesting."),
    p("For best effects, start with the left most country when adding them to the plot."),
    tags$ul(
      tags$li("India, Pakistan, Bangladesh. Then, add Sri Lanka"),
      tags$li("Iraq, Iran"),
      tags$li("Lithuania, Moldova, Belarus, Ukraine"),
      tags$li("Morocco, Algeria, Tunesia"),
      ),
    tags$br(),
    tags$br(),
   
    sidebarLayout(
      sidebarPanel(
        
       # Select country of origin to plot
        selectInput(inputId = "Country", 
                    label = "Choose countries", 
                    c(sort(applications$Country)), 
                    selected = "Afghanistan", multiple = TRUE),
      ),
      
      mainPanel(
        #plotOutput as dygraph
        dygraphOutput(outputId = "timetrend"),
        tags$br(),
        tags$br(),
        p("Data source: Federal statistical office of Switzerland."),
      )
    )
  ),
  ### END: Tab 2
  
  ### START: Tab 3
  tabPanel(
    "Correlation of migration and happiness",
    "See how happiness-factors influence migration.",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    
    fluidRow(
      column(8,column(12),
             h4("Top 5 Countries World Happiness Report 2019 + their number of Migration applications to Switzerland"),
             hr(),
             tableOutput('hi5'),
             hr(), 
             br(),
             br(),
             br(),
             br(),
             h4("Bottom 5 Countries World Happiness Report 2019 + their number of Migration applications to Switzerland"),
             hr(),
             tableOutput('lo5'),
      ),
      column(4,column(12),
             h4("correlation Happiness-Factors and Migration 2019"),
             hr(),
             plotOutput("correlation_heat_map_numbered"),
             h4("Findings from Correlation"),
             hr(),
             h5("-obiously very strong (negative) correlation between Rank and Score."),
             h5("-small negative correlation between better ranking and Number of Migration-Applications. Remember: the smaller a countries rank, the better."),
             h5("-strongest (0.35 and 0.31) correlation between Migration and Freedom to make life choices and Social suppor"),
             h5("-strangely no correlation between Rank and Generosity."),
             h5(" very strongly no correlation")
      ),
    ),
 
  
        #plot a heatmap
        #plotOutput(outputId = "correlation_heat_map_numbered"),
      
        # Select country of origin to plot
        selectInput(inputId = "Country2", 
                    label = "Choose country", 
                    c(sort(happy_countries$Country)), 
                    selected = "Afghanistan", multiple = FALSE),
        
        #plotOutput as dygraph
        dygraphOutput(outputId = "timetrend2"),
        #plotOutput as dygraph
        dygraphOutput(outputId = "happinesstrend"),
  ),
  ### END: Tab 3
  
  ### START: Tab 4
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
  ### END: Tab 4
  
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
  
  ### START: Tab 6
  tabPanel(
    "Data and Data Cleaning",
    "The following data was used in our project:",
    tags$br(),
    tags$br(),
    tags$b("1. Asylgesuche nach Nationen (1986 bis 2020). See table below."), 
    "Data published by ",
    tags$a(href="https://www.sem.admin.ch/sem/fr/home/publiservice/statistik/asylstatistik/uebersichten.html",
           "Federal statistical office of Switzerland."),
    tags$p("To this data set we added a column with the countries postal codes, so we could map the data with other datasets."),
    tags$br(),
    tags$br(),
    DTOutput(outputId = "datatable"),
    verbatimTextOutput("rawtable"),
    downloadButton(outputId="downloadCsv",label="Download as CSV"),
    tags$br(),
    tags$br(),
    tags$b("2. countries.geo.json. "), 
    "Data published by ",
    tags$a(href="https://github.com/johan/world.geo.json/blob/master/countries.geo.json",
           "Johan on GitHub."),
    tags$p("The original data contains country polygons and some additional properties as per 2016. We cleaned out all redundant and unnecessary data from this dataset (e.g. columns with redundant country names, codes, etc. With Python (writeCountToGeoJson.py) we added the numbers from AsylgesuchePerNation1986 as new properties. As this is a nested JSON file, we converted the GeoJSON file to a csv. In a fourth step we replaced NA values in the csv with 0: Because not every country had applications, it this case it is ok to just replace it with 0 - the number was 0."),
    tags$br(),
    tags$br()
           ), 
  ### END: tab 6
  
  ### START: Tab 7
  tabPanel(
    "About this project", 
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
  ) # END: tab 7
  ) # END navbarPage
  ) # END fluidPage


# Define server function  
server <- function(input, output) {
  
   # output tab 1: map with Leaflet
  applications_tb2 <- as_tibble(countries_csv)

  # return only data selected by reactive input
  numbers_selected <- reactive({
    numbers_selected <- select(applications_tb2, Name, toString(input$application_year))
    names(numbers_selected)[2] <- "year_selected" #change column name to a fixed name 
    # print(glimpse(numbers_selected)) # debug only: prints the reactive output to console -> correctly prints out the values of the selected year
    return(numbers_selected)
  })
  
  output$map <- renderLeaflet({
    # color palette
    pal <- colorNumeric(
      palette = "Reds",
      domain = numbers_selected()$year_selected
            )
    
    labels <- sprintf(
      "<strong>Country: %s</strong><br/><br/>
      <strong>Applications: %g</strong><br/><br/>
    Postal Code: %s<br/>
    Population: %g<br/>
    GDP: %g<br/>
    Economical Status: %s<br/>",
      countries$formal_en, numbers_selected()$year_selected, countries$postal, countries$pop_est, countries$gdp_md_est, countries$economy 
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
      

      # make it an overlay for the map
      addPolygons(
        group = "Countries: Applications", 
        data = countries, 
        label = labels, 
        weight = 1, 
        color = "White", 
        opacity = 1, 
        fillColor = ~pal(numbers_selected()$year_selected), 
        fillOpacity = 0.7) %>%
      addLayersControl(
        baseGroups = c("OSM", "Toner", "Topo"),
        overlayGroups = c("Countries: Applications"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  output$map2 <- renderLeaflet({
    # color palettes
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = countries$pop_est)
    
    pal2 <- colorNumeric(
      palette = "GnBu", 
      domain = countries$gdp_md_est)
    
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
  ### END output tab 1
  
  # output tab 2: time trend with dygraphs
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
  ### End output tab 2
  
  # TODO: output tab 3: correlation
  
  #get the input
  # output tab 3: time trend with dygraphs
  selected_country <- reactive({
    
    
    # convert to data table to ease later conversion to xts, as described here:
    # https://stackoverflow.com/questions/4297231/converting-a-data-frame-to-xts
    tbl = as.data.table(happiness_combined)
    tbl <- select(tbl, Year, input$Country2)
    #convert data table to xts format as described here:
    # https://stackoverflow.com/questions/23224142/converting-data-frame-to-xts-order-by-requires-an-appropriate-time-based-object
    qxts <- xts(tbl[, -1], order.by=as.POSIXct(tbl$`Year`))
    qxts
  })
  
  #rendering the dygraph for happiness
  output$happinesstrend <- renderDygraph({
    dygraph(selected_country(), main = "Happiness-Score from 2016-2019") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
  })
  
  
  selected_countries2 <- reactive({
    
    # convert to data table to ease later conversion to xts, as described here:
    # https://stackoverflow.com/questions/4297231/converting-a-data-frame-to-xts
    tbl3 = as.data.table(applis_trnspsd)
    tbl3 <- select(tbl3, Time, input$Country2)
    #convert data table to xts format as described here:
    # https://stackoverflow.com/questions/23224142/converting-data-frame-to-xts-order-by-requires-an-appropriate-time-based-object
    qxts3 <- xts(tbl3[, -1], order.by=as.POSIXct(tbl3$`Time`))
    qxts3
  })
  
  # rendering our dygrpah
  ## code copied from here:
  ## https://rstudio.github.io/dygraphs/index.html
  output$timetrend2 <- renderDygraph({
    dygraph(selected_countries2(), main = "No. of applications from 2016-2019") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
  })
  
  #happy-table
  output$hi5 <- renderTable({
    head(happiness_combined_ascending)
  })
  
  #unhappy-table
  output$lo5 <- renderTable({
    tail(happiness_combined_ascending)
  })
  
  #correlation 
  output$correlation_heat_map_numbered <- renderPlot({
  cor_data <- happiness_correlation[,3:length(happiness_correlation)]
  cormat <- round(cor(cor_data),2)
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cormat)
  upper_tri
  
  # Melt the correlation matrix
  library(reshape2)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Heatmap
  library(ggplot2)
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  # Print the heatmap
  correlation_heat_map_numbered <- ggheatmap
  
  ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
  
  })
  ### End output tab 3
  
  # TODO: output tab 4: forecast
  
  ### START: Output Tab 5
  output$top10 <- renderTable({
    # edit the dataset to fit the table
    A <- applications
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

  # output tab 6: 
  # data table with DT
    output$datatable <- renderDT(applications)
    
    # Download functionality
    output$downloadCsv <- downloadHandler(
        filename = function() {
        paste("Asylumdata.csv", sep=";")
      },
      content = function(file) {
        write.csv(applications, file)
    })
  ### END: Output Tab 6
  
} ### end server


# Create Shiny object
shinyApp(ui = ui, server = server)
