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
library(readr)
library(forecast)
library(fpp2)
library(TTR)
library(fUnitRoots)
library(ggfortify)
library(timeSeries)
library(timeDate)

# import data. Disable 'check.names' to prevent shiny from adding an X prefix.
applications <- read.csv("data/AsylgesuchePerNation1986.csv", sep=";", encoding="UTF-8",  header = TRUE, check.names = FALSE)
# applis_trnspsd is used for time series tab
applis_trnspsd <- read.csv("data/appli_transposed.csv", sep=";", encoding="UTF-8",  header = TRUE, check.names = FALSE)

# happiness is used for tab 3
happiness_combined <- read.csv("data/happy_transposed.csv", sep=";", encoding="UTF-8", header=TRUE, check.names = FALSE)
happy_countries <- read.csv("data/happy_combined_cleaned.csv", sep=",", encoding="UTF-8", header=TRUE, check.names = FALSE)
happiness_correlation <- read.csv("data/happy_correlation.csv", sep=",", encoding="UTF-8", header = TRUE, check.names = FALSE)
happiness_combined_ascending <- read.csv("data/happy_combined_ascending.csv", sep=",", encoding="UTF-8", header=TRUE, check.names = FALSE)

# Countries and map are used on map tab
countries <- geojson_read("data/countries.geo.json", what = "sp") # for country polygons, gdp_md_est and pop_est
countries_csv <- read.csv("data/countries.csv", sep=";", encoding="UTF-8", header = TRUE, check.names = FALSE) # for application numbers per year
map <- leaflet(countries)

# import forecast data
forecast_numbers <- read.csv("data/forecast_data_final.csv")


# Define UI
ui <- fluidPage(
  #shinythemes::themeSelector(),
  
  navbarPage(
    
    ### START: Tab 1
    "Applications for asylum in Switzerland",
    tabPanel(
      "Map overview",
      h3("Number of applications by year and country of origin"),
      p("Browse the map over the given time span by selecting a year from the slider: The darker the colour the higher the application numbers. Mouse over the country to get a label with the exact number of applications and some more info."),
      p("Some interesting first impressions: "),
      tags$ul(
        tags$li("Applications from Turkey were predominant between 1986-1990. Whether this was due to the military coup in 1980 or (and) due to massive conflicts between Turkey and PKK is of course not explained by the data."),
        tags$li("In the 1990s the most applications come from Serbia and Bosnia-Herzegowina, but also from Romania, Iraq and Algeria. 
                This made one realize the own Eurocentrism - not knowing anything about politics in Algeria."),
        tags$li("You can also see that after the year 2000 the list of application countries broadens in general."),
      ),
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
    h3("Numbers of applications by country of origin, from 1986 to 2020"),
    p("Exploring the data by comparing the time trends of different countries might reveal some surprising insights."),
    p("Whether a comparison is interesting depends on one's expectation, of course. 
      We listed some combinations that seemed interesting to us. Your mileage may vary."),
    strong("Syria, Afghanistan and Eritrea"),
    p("A comparison between Syria, Afghanistan and Eritrea shows a dramatic increase in numbers 
    of applications in 2015. This was expected for Syria.",tags$br()," 
      What took us by surprise, was the drastic drop in applications after 2015. 
      Another surprise was to see that so many more people applied in 2015 from Eritrea and Afghanistan than from Syria."),
    strong("North Macedonia, Albania, Bosnia & Herzegovina, Serbia"),
    p("Some of us were rather surprised to see that so many more people had applied from Serbia than from Bosnia & Herzegovina."),
    
   
    tags$br(),
   
    sidebarLayout(
      sidebarPanel(
        strong("Ready to explore?"),
        p("Below, a few combinations we found interesting."),
        p("For best effects, start with the left most country when adding them to the plot."),
        tags$ul(
          tags$li("India, Pakistan, Bangladesh. Then, add Sri Lanka"),
          tags$li("Iraq, Iran"),
          tags$li("Lithuania, Moldova, Belarus, Ukraine"),
          tags$li("Morocco, Algeria, Tunesia"),
        ),
        tags$br(),
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
    "The World Happiness Report ranks countries by their happiness levels, based on certain
variables (see Tab Data and data cleaning).",
    tags$br(),
    tags$br(),
    "The two tables show the top and bottom five countries
from the year 2019 with an extra column showing their number of migration applications to Switzerland. 
In 2019 Switzerland was ranked number four in the World Happiness Report and was removed from the table below, 
since there are no migration applications to Switzerland from Switzerland. ",
    tags$br(),
    tags$br(),
    "You can compare the number of applications by country to Switzerland in the years 2016 to 2020 and it's level of 
Happiness during 2016 to 2019 further below.",
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
             h4("Correlation Happiness-Variables and Migration 2019"),
             hr(),
             plotOutput("correlation_heat_map_numbered"),
             h4("findings from Correlation"),
             hr(),
             h5("-obiously very strong (negative) correlation between Rank and Score."),
             h5("-small negative correlation between better ranking and number of Migration-Applications. Remember: the smaller a countries rank, the better."),
             h5("-strongest (0.35 and 0.31) correlation between Migration and Freedom to make life choices and Social support"),
             h5("-strangely no correlation between Rank and Generosity."),
      ),
    ),
 
  
        #plot a heatmap
        #plotOutput(outputId = "correlation_heat_map_numbered"),
    tags$br(),
    tags$br(),
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
    "For the forecasting in this tab, ARIMA modelling was used as it is designed for the use with time series.", 
    tags$br(),
    tags$br(),
    sidebarLayout(
      sidebarPanel(
        strong("What application numbers can Switzerland expect?"),
        tags$br(),
        p("The first graph shows the application numbers over time. As in the timetrend tab, you can see some sudden drops in the numbers, especially towards 2020. "),
        p("In the second section, you can find an ACF plot, an autocorrelation function that shows how serial correlation (=autocorrelation) in data changes over time. "),
        p("The third section contains a graph of the forecast: You can see, the forecasted numbers stays at 21000. "),
        p("In the fourth section, we print the respective models forecast numbers to a table, the rows start in 2021 and go to 2032. "),
        
         ),
      mainPanel(
        plotOutput(outputId = 'applications'),
        plotOutput(outputId = 'residuals'),
        plotOutput(outputId = 'forecast'),
        tableOutput(outputId = 'forecast_table')
        
      )
    )
  ),
  ### END: Tab 4
  
  ### START: Tab 5
  tabPanel("Top and bottom countries", 
           
           # 1. Row
           fluidRow(column(12,
                  # Add slider input named 'year' to select years (1986 - 2020)
                  sliderInput(inputId = 'year', label = 'Select Year', min = 1986, max = 2020, value = 1986, sep = "", width = '100%'),
           ),),
           # 2. Row
           fluidRow(column(3,
                    "Top applications (year)",
                    tableOutput('top10')
           ),column(3,
                    "Top applications (alltime)",
                    tableOutput('sumtop10')
           ),column(3,
                    "Least applications (year)",
                    tableOutput('flop10')
           ),column(3,
                    "Least applications (alltime)",
                    tableOutput('sumflop10')
           )
           ),
          # 3. Row
          fluidRow(column(1,),column(9,
                    p("This page shows the top and bottom countries by number of applications (15 entries).
                    Both segments are built the same way. The left side shows the updated version of the year you selected.
                    The right side of each segment shows the sum of the top ten countries over the entire time span from 1986 to 2020."),
          ),column(1,)),
  ),
          ### END: Tab 5
  
  ### START: Tab 6
  tabPanel(
    "Data and data cleaning",
    "The following data was used for this project:",
    tags$br(),
    tags$br(),
    tabsetPanel(
      tabPanel("Applications for asylum",
               tags$br(),
               tags$b("Applications for asylum by country (1986 to June 2020)."), 
               "Data published by the ",
               tags$a(href="https://www.sem.admin.ch/sem/fr/home/publiservice/statistik/asylstatistik/uebersichten.html",
                      "Federal statistical office of Switzerland"), ". Last accessed: Sept 12, 2020.",
               tags$p("The following transformations were performed on the data set:"),
               tags$ol(
                 tags$li("Quality Assessment: The data was analyzed for null values and any unreasonable numbers (exorbitantly high or negative). We found no anomalies, but a very high occurance of zeros. However, this seems plausible in this context (see quality_assessment/applications-profile.html)."),
                 tags$li("As numbers contained some commas to separate thousands, they were converted to integer."),
                 tags$li("As our application is in English, but the statistical office does not provide data in English, the country names were manually translated from German into English."),
                 tags$li("In order to map this data with other datasets, this data set was enriched with an additional column containing the three-letter country code according to ISO 3166-1 alpha-3."),
                 tags$li("For usage in the time trend graph, the table was transposed to have the countries as columns instead of rows (see scripts/transposeAppli.py)."),
                 tags$li("Additionally, a timestamp was added to comply with the format requirements of the time trend graph (see data/appli_transposed.csv, scripts/transposeAppli.py)."),
               ),
               tags$b("Note: "),
               tags$p("Regarding step 4: There does not appear to be a three-letter country code for East Germany and Czechoslovakia. The 4-letter ISO 3166-3 codes DDDE and CSHH were used instead."),
               tags$p("Regarding step 6: As the timestamp was added for mere technical reasons, it is not provided in the data presented here for download.
                      However, as a result of this timestamp, the resolution of the time trend graph may seem higher than it actually is.", tags$br(),
                      "While on zoom, the graph labels single months, whereas the original data only contains one value per year."),
               tags$br(),
               tags$br(),
               DTOutput(outputId = "datatable"),
               verbatimTextOutput("rawtable"),
               downloadButton(outputId="downloadCsv",label="Download as CSV"),
               tags$br(),
               tags$br(),
      ),
      tabPanel("Map data",
               tags$br(),
               tags$b("countries.geo.json. "), 
               "Data published by ",
               tags$a(href="https://github.com/johan/world.geo.json/blob/master/countries.geo.json",
                      "Johan on GitHub. Last accessed: Oct 09, 2020."),
               tags$p("The original data contains country polygons and some additional properties as per 2016."),
               tags$br(),
               tags$p("The following transformations were performed on this data set:"),
               tags$ol(
                 tags$li("Data cleaning: Removal of redundant data, e.g. entries of redundant country names, codes, etc."),
                 tags$li("Enrich data set with data from asylum applications data (source: AsylgesuchePerNation1986.csv, script: scripts/writeCountToGeoJson.py). "),
                 tags$li("To simplify reading out the data, the (nested) GeoJSON was flattened to a csv format."),
                 tags$li("As not every country listed in the data set actually had applications, NA values in the csv were replaced with 0."),
               ),
               tags$b("Note: "),
               tags$p("The map shows borders as of 2016. This means that former countries such as Czechoslovakia cannot be seen."),
               tags$br(),
               tags$br(),
               DTOutput(outputId = "mapdata"),
              #verbatimTextOutput("rawtable"),
               downloadButton(outputId="mapdata_csv",label="Download as CSV"),
               tags$br(),
               tags$br(),
               
      ),
      tabPanel("World Happiness Report",
               tags$br(),
               tags$b("World Happiness Report (2016-2019)."),
               "Data is published by",
               tags$a(href="https://worldhappiness.report/ed/2020/",
                      "the Sustainable Development Solutions Network."),
               tags$br(),
               tags$br(),
               "The World Happiness Report is an annual publication of the United Nations Sustainable Development Solutions Network. It contains articles, and rankings of national happiness based on respondent ratings of their own lives, which the report also correlates with various life factors. Source: ",
               tags$a(href="https://en.wikipedia.org/wiki/World_Happiness_Report",
                      "Wikipedia: World Happiness Report"),
               tags$br(),
               tags$br(),
               tags$b("Methods and philosophy"),
               "Data is collected from people in over 150 countries. Each variable measured reveals a populated-weighted average score on a scale running from 0 to 10 that is tracked over time and compared against other countries. These variables currently include:",
               tags$ul(
                 tags$li("real GDP per capita"),
                 tags$li("social support"),
                 tags$li("healthy life expectancy"),
                 tags$li("freedom to make life choices"),
                 tags$li("generosity"),
                 tags$li("perceptions of corruption")
               ),
               "Each country is also compared against a hypothetical nation called Dystopia. Dystopia represents the lowest national averages for each key variable and is, along with residual error, used as a regression benchmark. The six metrics are used to explain the estimated extent to which each of these factors contribute to increasing life satisfaction when compared to the hypothetical nation of Dystopia, but they themselves do not have an impact on the total score reported for each country.",
               tags$br(),
               tags$br(),
               "Some countries were deleted from the data as they do not appear in all reports from 2016 to 2019. Switzerland was removed as there are no asylum applications to Switzerland from Switzerland.",
               "For the Year 2019, the number of asylum applications by country was added to see how strong the correlation between this number and the happiness factors is. The number of applications from each country to Switzerland is from the Federal statistical office of Switzerland.",
               tags$br(),
               tags$br(),
               tags$b("World Happiness Report 2019 with extra column for the asylum applications to Switzerland"),
               tags$br(),
               tags$br(),
               #tableOutput('WorldHappinessReport2019'),
               DTOutput(outputId = "WorldHappinessReport2019"),
               downloadButton(outputId="WorldHappinessReport2019_csv",label="Download as CSV"),
               tags$br(),
               tags$br(),
      )
    )
  ),
  
  ### END: tab 6
  
  ### START: Tab 7
  
  tabPanel("About this project", 
           tags$br(),
           
           tags$p("This shiny application has been developed 
           as part of the Data Science Basics class at Lucerne University of Applied Sciences and Arts in Rotkreuz."),
           
           tags$b("Date of submission:"),
           tags$p("December 15, 2020"),
           tags$br(),
           tags$br(),
           tags$br(),
           tabsetPanel(
                       tabPanel("Motivation",
                                tags$br(),
                                tags$br(),
                                tags$p("With COVID19 and US elections predominant in the media in 2020, we wondered what had happened to the refugees in this world."),
                                tags$p("After camp Moria burning down in autumn 2020, we decided to focus our attention to this 'forgotten' topic."),
                                tags$p("Narrowing down the area of our research question, we looked for numbers on asylum seekers in Switzerland."), 
                                tags$p("With data sets on asylum seekers in Switzerland, 
                                       we try to visualize the bare numbers, but also to investigate possible connections."),
                                ),
                       tabPanel("Team",
                                tags$br(),
                                tags$br(),
                                
                                tags$p("The makers are all students of the BSC in Information Technology program."),
                                tags$b("Contributors:"),
                                tags$ul(
                                  tags$li("Leyla Ciragan"),
                                  tags$li("Tariq Ghazzawi"),
                                  tags$li("Lukas Ingold"),
                                  tags$li("Gabriela Moos"),
                                  tags$li("Urs Stadelmann")
                                  ),
                                tags$b("Professor:"),
                                tags$p("Dr. Luca Mazzola"),
                        )
                                
            )
  ) 
         
           
  # END: tab 7
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
  
  #output tab 3: correlation
  
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
    tbl3 = as.data.table(tail(applis_trnspsd, 6))
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
    dygraph(selected_countries2(), main = "No. of applications from 2016-2020") %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
  })
  
  #happy-table
  output$hi5 <- renderTable({
    head(happiness_combined_ascending, 4)
  })
  
  #unhappy-table
  output$lo5 <- renderTable({
    tail(happiness_combined_ascending, 5)
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
  
  ### START: output tab 4: forecast
  # setting time scale of the data and declaring its start, end and frequency
  Y <- ts(forecast_numbers[,2],start=c(1996,1), end = c(2020,1),frequency = 1)
  # Fitting the data inot an ARIMA model
  fit_arima <- auto.arima(Y)
  # Using the ARIMA to plot a forecast of the data
  fcst <- forecast(fit_arima,h=12)
  
  output$applications <- renderPlot({
    ###  Plotting the original data
    autoplot(Y)+ 
      ggtitle("Asylum applications to Switzerland") +
      ylab("Applications")+
      xlab("Year")
  })
  
  output$residuals <- renderPlot({
    #print(summary(fit_arima))
    checkresiduals(fit_arima)
  })
  
  output$forecast <- renderPlot({
    autoplot(fcst) + ggtitle("Asylum applications to Switzerland Forecast") +
      ylab("Applications") +
      xlab("Year")
  })
  
  output$forecast_table <- renderTable({
    print(summary(fcst))
  })
  
  ### End output tab 4
  
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
    C <- dplyr::filter(B, B[toString(input$year)] > 0)
    #order
    D <- C[order(-C[toString(input$year)]),]
    E <- D[1:15,]
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
    C <- dplyr::filter(B, B[toString(input$year)] > 0)
    #order
    D <- C[order(C[toString(input$year)]),]
    E <- D[1:15,]
    E
  })
  
  output$sumtop10 <- renderTable({
    # edit the dataset to fit the table
    A <- applications
    A[A$Code  == "Stateless", "Country"]<- "Stateless"
    A[A$Code  == "Without Nationality", "Country"]<- "Without Nationality"
    A[A$Code  == "State unknown", "Country"]<- "State unknown"
    A[A$Code  == "Without declaration", "Country"]<- "Without declaration"
    #delete Code column
    A$Code <- NULL
    #subset only years
    B1 <- A[,c("1986","1987":"2020")]
    Sum <- rowSums(B1)
    Country <- A[,c("Country")]
    C <- cbind(Country,Sum)
    #order
    D <- C[order(-Sum),]
    E <- D[1:15,]
    E
  })
  
  output$sumflop10 <- renderTable({
    # edit the dataset to fit the table
    A <- applications
    A[A$Code  == "Stateless", "Country"]<- "Stateless"
    A[A$Code  == "Without Nationality", "Country"]<- "Without Nationality"
    A[A$Code  == "State unknown", "Country"]<- "State unknown"
    A[A$Code  == "Without declaration", "Country"]<- "Without declaration"
    #delete Code column
    A$Code <- NULL
    #subset only years
    B1 <- A[,c("1986","1987":"2020")]
    Sum <- rowSums(B1)
    Country <- A[,c("Country")]
    C <- cbind(Country,Sum)
    #order
    D <- C[order(Sum),]
    E <- D[1:15,]
    E
  })
  
  data <- reactive ({
    rnorm(input$year)
  })
  ### END: Output Tab 5

  # output tab 6: 
  # table of applications data with DT
    output$datatable <- renderDT(applications,options = list(scrollX = TRUE,paging = TRUE))
    
    # Download functionality
    output$downloadCsv <- downloadHandler(
        filename = function() {
        paste("asylum_applications.csv", sep=";")
      },
      content = function(file) {
        write.csv(applications, file)
    })
    
  # table of map data with DT
    output$mapdata <- renderDT(countries_csv,options = list(scrollX = TRUE,paging = TRUE))
    
    # Download functionality
    output$mapdata_csv <- downloadHandler(
      filename = function() {
        paste("map_data.csv", sep=";")
      },
      content = function(file) {
        write.csv(countries_csv, file)
      })
  
  # table of happiness data with DT
  output$WorldHappinessReport2019 <- renderDT(happiness_correlation,options = list(scrollX = TRUE,paging = TRUE))
  
  # Download functionality
  output$WorldHappinessReport2019_csv <- downloadHandler(
    filename = function() {
      paste("happiness_data.csv", sep=";")
    },
    content = function(file) {
      write.csv(happiness_correlation, file)
    })
  ### END: Output Tab 6
  
} ### end server


# Create Shiny object
shinyApp(ui = ui, server = server)
