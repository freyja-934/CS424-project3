#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
################### key: 305d59068af82054adce3f22e00d7495


library(shiny)
library(DT)
library(AotClient) 
library(darksky)
#library("ropenaq")
library(lubridate)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(qdapTools)
library(data.table)
library(jsonlite)
pollutants_list = list("NO2", "Ozone", "CO", "H2S", "SO2", "PM10", "PM25", "Temperature", "Humidity", "Light")
Sys.setenv(DARKSKY_API_KEY = '305d59068af82054adce3f22e00d7495')
# Define UI for application that draws a histogram
ui <- dashboardPage(
  ################################################################################ THE COLOR AND LENGTH OF THE TITLE FOR THE SIDEBAR ################################################################################
  skin = "yellow",
  dashboardHeader(title = "CS 424 PROJECT 3-Group 2", titleWidth = 350 ),
  
  ######################################## CREATE DROP DOWN MENUS IN SIDEBAR + NEW TAB CONTAINING RESOURCES ######################################## 
  dashboardSidebar(sidebarMenu(disable = FALSE, collapsed = FALSE,  style = "margin-top:500px",
                               uiOutput("nodeOutput"),
                               uiOutput("node1Output"),
                               uiOutput("node2Output"),
                               selectInput("pollutants", "Pollutants", pollutants_list),
                               menuItem("Node Map", tabName="map", icon = icon("dashboard")),
                               menuItem("Comparison", tabName="compare", icon = icon("dashboard")),
                               menuItem("Resources", tabName="resources", icon = icon("bullet")),
                               menuItem("test", tabName="boxes", icon = icon("bullet")),
                               menuItem("compare2", tabName="compare2", icon = icon("bullet")),
                               checkboxInput("NO2", "NO2", TRUE),
                               checkboxInput("OZONE", "OZONE", TRUE),
                               checkboxInput("CO", "CO", TRUE),
                               checkboxInput("H2S", "H2S", FALSE),
                               checkboxInput("SO2", "SO2", FALSE),
                               checkboxInput("PM10", "PM10", FALSE),
                               checkboxInput("PM25", "PM25", FALSE),
                               checkboxInput("TEMPERATURE", "TEMPERATURE", FALSE),
                               checkboxInput("HUMIDITY", "HUMIDITY", FALSE),
                               checkboxInput("INTENSITY", "INTENSITY", FALSE)
                              
                              
  )),
  ######################################## THE MAIN BODDY OF THE WEB APP ########################################
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(column(12,
                    leafletOutput("mymap", height = 1200)
                    )),
        fluidRow(
             column(12, h4(textOutput("Node Data")),
                    tabsetPanel(type = "tabs",
                      tabPanel("Current",plotOutput("node_data")),
                      tabPanel("24 Hours", plotOutput("node_data24")),
                      tabPanel("7 Days",plotOutput("node_data7")),
                      position = "left"
                    )))
             ),
      tabItem(
        tabName = "compare",
        fluidRow(column(6,h4(textOutput("Node 1")),
              
               tabsetPanel(type = "tabs",
                 tabPanel("Current",plotOutput("node1_cur")),
                 tabPanel("24 Hours", plotOutput("node1_24")),
                 tabPanel("7 Days",tableOutput("node1_7")),
                 position = "below"
               )
             ),
             column(6,h4(textOutput("Node 2")),
                    tabsetPanel( type = "tabs",
                      tabPanel("Current",plotOutput("node2_cur")),
                      tabPanel("24 Hours", plotOutput("node2_24")),
                      tabPanel("7 Days",plotOutput("node2_7")),
                      position = "left"
                    )
             )
    )),
    tabItem(
      tabName = "resources",
      h2("Resources used in this project:"),
      h5("All data used is from here: https://aqs.epa.gov/aqsweb/airdata/download_files.html"),
      h5("Base Code and Code influence from Professor Andy Johnson, https://www.evl.uic.edu/aej/424/ (week 2)"),
      h6("librarys used: shiny, shinydashboard, ggplot2, lubridate, DT, jpeg, grid, leaflet, scales, reshape, tidyr, readr"),
      h5("Techniques and methods adapted from:"),
      h6("  *https://rstudio.github.io/shinydashboard/appearance.html"),
      h6("  *reshape and melt function from https://www.statmethods.net/management/reshape.html"),
      h6("  *https://stat.ethz.ch/R-manual/R-devel/library/base/html/toString.html"),
      h6("  *https://stat.ethz.ch/R-manual/R-devel/library/base/html/strsplit.html "),
      h6("  *https://stat.ethz.ch/R-manual/R-devel/library/base/html/sort.html"),
      h6("  *https://stackoverflow.com/questions/52544228/r-shiny-display-static-text-outside-sidebar-panel"),
      h6("  *https://stackoverflow.com/questions/12280571/how-can-i-remove-rows-containing-0-of-certain-columns-while-keeping-the-rows-i"),
      h6("  *https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/position_stack"),
      h6("  *http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization"),
      h6("  *https://stackoverflow.com/questions/26869141/conditionally-hiding-data-labels-in-ggplot2-graph"),
      h6("  *https://www.displayr.com/how-to-make-a-pie-chart-in-r/"),
      h6("  *https://stackoverflow.com/questions/38126212/how-to-check-if-data-table-has-empty-rows"),
      h6("  *http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/"),
      h6("  *https://rpubs.com/euclid/343644"),
      h6("  *http://shiny.rstudio.com/articles/shinyapps.html"),
      h6("  *https://shiny.rstudio.com/reference/shiny/0.14/updateSelectInput.html"),
      h6("  *https://gist.github.com/aagarw30/d08c5fb1794cf9b58fa38342db97b697"),
      h6("  *http://shiny.rstudio.com/articles/shinyapps.html"),
      h6("  *student who was in office hours on Friday - never got your name"),
      h6("  *http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/"),
      h6("  *https://stackoverflow.com/questions/33266157/how-to-add-more-whitespace-to-the-main-panel-in-shiny-dashboard"),
      h6("  *professor Andy Johnson")
      
    ),
    tabItem(
      tabName = "boxes",
      verbatimTextOutput("NO2"),
      verbatimTextOutput("OZONE"),
      verbatimTextOutput("CO"),
      verbatimTextOutput("H2S"),
      verbatimTextOutput("SO2"),
      verbatimTextOutput("PM10"),
      verbatimTextOutput("PM25"),
      verbatimTextOutput("TEMPERATURE"),
      verbatimTextOutput("HUMIDITY"),
      verbatimTextOutput("INTENSITY")
      
      
    ),
    tabItem(
      tabName = "compare2",
               box( title = "NODE 1 - NO2", solidHeader = TRUE, status = "primary",width = 2, dataTableOutput("NO2_1")),
               box( title = "NODE 1 - OZONE", solidHeader = TRUE, status = "primary", width = 2,dataTableOutput("OZONE_1")),
               box( title = "NODE 1 - CO", solidHeader = TRUE, status = "primary",width = 2,dataTableOutput("CO_1")),
               box( title = "NODE 1 - H2S", solidHeader = TRUE, status = "primary",width = 2,dataTableOutput("H2S_1")),
               box( title = "NODE 1 - SO2", solidHeader = TRUE, status = "primary",width = 2, dataTableOutput("SO2_1")),
               box( title = "NODE 1 - PM10", solidHeader = TRUE, status = "primary",width = 1, dataTableOutput("PM10_1")),
               box( title = "NODE 1 - PM25", solidHeader = TRUE, status = "primary",width = 1, dataTableOutput("PM25_1")),
      
               box( title = "NODE 2 - NO2", solidHeader = TRUE, status = "primary" ,width = 2, dataTableOutput("NO2_2")),
               box( title = "NODE 2 - OZONE", solidHeader = TRUE, status = "primary", width = 2,dataTableOutput("OZONE_2")),
               box( title = "NODE 2 - CO", solidHeader = TRUE, status = "primary", width = 2,dataTableOutput("CO_2")),
               box( title = "NODE 2 - H2S", solidHeader = TRUE, status = "primary", width = 2,dataTableOutput("H2S_2")),
               box( title = "NODE 2 - SO2", solidHeader = TRUE, status = "primary", width = 2,dataTableOutput("SO2_2")),
               box( title = "NODE 2 - PM10", solidHeader = TRUE, status = "primary", width = 1,dataTableOutput("PM10_2")),
               box( title = "NODE 2 - PM25", solidHeader = TRUE, status = "primary", width = 1,dataTableOutput("PM25_2"))
    )
    )
  ))

# Nodes - Column Names
# "vsn"               "location.type"     "location.geometry" "description"       "address" 
# vsn location.type location.geometry.type location.geometry.crs.type location.geometry.crs.name location.geometry.coordinates

# Observations - Column Names
# "value"             "uom"               "timestamp"         "sensor_path"       "node_vsn"          "location.type"     "location.geometry"

# Projects - Column Names
# "slug"        "name"        "hull"        "archive_url"

# Sensors - Column Names
# "uom"        "path"       "min"        "max"        "data_sheet"






























# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  autoInvalidate <- reactiveTimer(1000*60)
  
  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    
  })
  

  
  
  getNodeData2 <- function(vsn, d,h){
    url <- "https://api.arrayofthings.org/api/observations?location=chicago&node="
    url <- paste(url, vsn,"&timestamp=","ge:2018-08-01T00:00:00&size=50000", sep="")
    s <- download.file(url, "obs.html", quiet = FALSE) 
    t = fromJSON("obs.html")
    u = t$data
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="UTC")
    int <- interval(gmtTime - hours(h) - days(d), gmtTime)
    path_list = pollutantPaths()
    path_list = pollutantPaths()

    if(length(u) == 0){
      return (u)
    }
    return(select(subset(u, is.element(sensor_path, path_list) & as.POSIXlt(timestamp, tz="UTC", "%Y-%m-%dT%H:%M") %within% int), 'node_vsn', 'sensor_path', 'timestamp', 'value'))
  }
  
  no2_path = "chemsense.no2.concentration"
  ozone_path = "chemsense.o3.concentration"
  co_path = "chemsense.co.concentration"
  h2s_path = "chemsense.h2s.concentration"
  so2_path = "chemsense.so2.concentration"
  pm10_path = "alphasense.opc_n2.pm10"
  pm25_path = "alphasense.opc_n2.pm2_5"
  temperature_path = "chemsense.at0.temperature"
  temperature_paths = list("lightsense.hih6130.temperature", 
                           "metsense.tmp112.temperature", 
                           "metsense.tsys01.temperature", 
                           "metsense.pr103j2.temperature",
                           "metsense.htu21d.temperature",
                           "metsense.bmp180.temperature",
                           "lightsense.tmp421.temperature",
                           "chemsense.lps25h.temperature",
                           "chemsense.at0.temperature",
                           "chemsense.at1.temperature",
                           "chemsense.at2.temperature",
                           "chemsense.at3.temperature")
  humidity_path = "chemsense.sht25.humidity"
  humidity_paths = list("metsense.hih4030.humidity",
                        "metsense.htu21d.humidity",
                        "chemsense.sht25.humidity")
  intensity_path = "chemsense.si1145.visible_light_intensity"
  
  ################### check boxes ##############
  
  output$NO2 <- renderText({ input$NO2 })
  output$OZONE <- renderText({ input$OZONE })
  output$CO <- renderText({ input$CO })
  output$H2S <- renderText({ input$H2S })
  output$SO2 <- renderText({ input$SO2 })
  output$PM10 <- renderText({ input$PM10 })
  output$PM25 <- renderText({ input$PM25 })
  output$TEMPERATURE <- renderText({ input$TEMPERATURE })
  output$HUMIDITY <- renderText({ input$HUMIDITY })
  output$INTENSITY <- renderText({ input$INTENSITY })
  
  # Use these for the check boxes
  no2_IsSelected <- reactive({
    if(input$NO2 == FALSE){
      no2_IsSelected <- FALSE
    }else{
      no2_IsSelected <- TRUE
    }
  })
  ozone_IsSelected <- reactive({
    if(input$OZONE == FALSE){
      ozone_IsSelected <- FALSE
    }else{
      ozone_IsSelected <- TRUE
    }
  })
  co_IsSelected <- reactive({
    if(input$CO == FALSE){
      co_IsSelected <- FALSE
    }else{
      co_IsSelected <- TRUE
    }
  })
  h2s_IsSelected <- reactive({
    if(input$H2S == FALSE){
      no2_IsSelected <- FALSE
    }else{
      no2_IsSelected <- TRUE
    }
  })
  so2_IsSelected <- reactive({
    if(input$SO2 == FALSE){
      so2_IsSelected <- FALSE
    }else{
      so2_IsSelected <- TRUE
    }
  })
  pm10_IsSelected <- reactive({
    if(input$PM10 == FALSE){
      pm10_IsSelected <- FALSE
    }else{
      pm10_IsSelected <- TRUE
    }
  })
  pm25_IsSelected <- reactive({
    if(input$PM25 == FALSE){
      pm25_IsSelected <- FALSE
    }else{
      pm25_IsSelected <- TRUE
    }
  })
  tempertature_IsSelected <- reactive({
    if(input$TEMPERATURE == FALSE){
      temperature_IsSelected <- FALSE
    }else{
      temperature_IsSelected <- TRUE
    }
  })
  humidity_IsSelected <- reactive({
    if(input$HUMIDITY == FALSE){
      humidity_IsSelected <- FALSE
    }else{
      humidity_IsSelected <- TRUE
    }
  })
  intensity_IsSelected <- reactive({
    if(input$INTENSITY == FALSE){
      intensity_IsSelected <- FALSE
    }else{
      intensity_IsSelected <- TRUE
    }
  })
  
  
  
  getData <- function(vsn, d,h, path){
    if(no2_IsSelected() == FALSE & path == no2_path){
      return(list())}
    if(!ozone_IsSelected() & path == ozone_path){return(list())}
    if(!co_IsSelected() & path == co_path){return(list())}
    if(!h2s_IsSelected() & path == h2s_path){return(list())}
    if(!so2_IsSelected() & path == so2_path){return(list())}
    if(!pm10_IsSelected() & path == pm10_path){return(list())}
    if(!pm25_IsSelected() & path == pm25_path){return(list())}
    if(!tempertature_IsSelected() & path == temperature_path){return(list())}
    if(!humidity_IsSelected() & path == humidity_path){return(list())}
    if(!intensity_IsSelected() & path == intensity_path){return(list())}

    sz<-"200"
    if(d == 7){
      sz <- "100000"
    }
    if(d == 1){
      sz <- "20000"
    }
    if(h == 1){
      sz <- "500"
    }
    # url <- "https://api.arrayofthings.org/api/observations?location=chicago&node="
    # url <- paste(url, vsn,"&timestamp=","ge:2018-08-01T00:00:00&sensor=", path,"&size=",size,sep="")
    # s <- download.file(url, "obs.html", quiet = FALSE) 
    # t = fromJSON("obs.html")
    # u = t$data
    u <- ls.observations(filters=list(project='chicago', sensor=path, node=vsn,size=sz))
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="UTC")
    int <- interval(gmtTime - hours(h) - days(d), gmtTime)
    path_list = pollutantPaths()
    path_list = pollutantPaths()

    if(length(u) == 0){
      return (u)
    }
    return(select(subset(u, as.POSIXlt(timestamp, tz="UTC", "%Y-%m-%dT%H:%M") %within% int), 'node_vsn', 'sensor_path', 'timestamp', 'value'))
  }
  
  #########################   first node
  
  output$NO2_1 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node1Input) 
    data <- getData(input$node1Input, 0, 1, no2_path)
    str(data)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node1Input)
    }
    else{
    data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE, width = 200 )))
  
  output$OZONE_1 <- DT::renderDataTable( 
    DT::datatable({ 
    req(input$node1Input)
    data <- getData(input$node1Input, 0,1, ozone_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node1Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$CO_1 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node1Input)
    data <- getData(input$node1Input, 0,1, co_path) 
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node1Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$H2S_1 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node1Input)
    data <- getData(input$node1Input, 0,1, h2s_path) 
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node1Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$SO2_1 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node1Input)
    data <- getData(input$node1Input, 0,1, so2_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node1Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$PM10_1 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node1Input)
    data <- getData(input$node1Input, 0,1, pm10_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node1Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$PM25_1 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node1Input)
    data <- getData(input$node1Input, 0,1, pm25_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node1Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  ########################### second node
  
  output$NO2_2 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node2Input) 
    data <- getData(input$node2Input, 0, 1, no2_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node2Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$OZONE_2 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node2Input)
    data <- getData(input$node2Input, 0,1, ozone_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node2Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$CO_2 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node2Input)
    data <- getData(input$node2Input, 0,1, co_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node2Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$H2S_2 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node2Input)
    data <- getData(input$node2Input, 0,1, h2s_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node2Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$SO2_2 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node2Input)
    data <- getData(input$node2Input, 0,1, so2_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node2Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$PM10_2 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node2Input)
    data <- getData(input$node2Input, 0,1, pm10_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node2Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  output$PM25_2 <- DT::renderDataTable(
    DT::datatable({ 
    req(input$node2Input)
    data <- getData(input$node2Input, 0,1, pm25_path)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),input$node2Input)
    }
    else{
      data}
  },options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE )))
  
  getPollutantPaths <- function(){
    pathList = list()
    if(no2_IsSelected()){pathList = c(pathList, no2_path)}
    if(ozone_IsSelected()){pathList = c(pathList, ozone_path)}
    if(co_IsSelected()){pathList = c(pathList, co_path)}
    if(h2s_IsSelected()){pathList = c(pathList, h2s_path)}
    if(so2_IsSelected()){pathList = c(pathList, so2_path)}
    if(pm10_IsSelected()){pathList = c(pathList, pm10_path)}
    if(pm25_IsSelected()){pathList = c(pathList, pm25_path)}
    if(tempertature_IsSelected()){pathList = c(pathList, temperature_path)}
    if(humidity_IsSelected()){pathList = c(pathList, humidity_path)}
    if(intensity_IsSelected()){pathList = c(pathList, intensity_path)}
    return(pathList)
  }
  
  
 ################################################### origional 
  # getPollutantPaths <- function(){
  #   pathList = list()
  #   if(no2_IsSelected){pathList = c(pathList, no2_path)}
  #   if(ozone_IsSelected){pathList = c(pathList, ozone_path)}
  #   if(co_IsSelected){pathList = c(pathList, co_path)}
  #   if(h2s_IsSelected){pathList = c(pathList, h2s_path)}
  #   if(so2_IsSelected){pathList = c(pathList, so2_path)}
  #   if(pm10_IsSelected){pathList = c(pathList, pm10_paths)}
  #   if(pm25_IsSelected){pathList = c(pathList, pm25_paths)}
  #   if(tempertature_IsSelected){pathList = c(pathList, temperature_paths)}
  #   if(humidity_IsSelected){pathList = c(pathList, humidity_paths)}
  #   if(intensity_IsSelected){pathList = c(pathList, intensity_path)}
  #   return(pathList)
  # }
  # 
  pollutantPaths <- reactive({
    req(no2_IsSelected)
    req(co_IsSelected)
    req(ozone_IsSelected)
    req(so2_IsSelected)
    req(pm10_IsSelected)
    req(pm25_IsSelected)
    req(h2s_IsSelected)
    req(tempertature_IsSelected)
    req(humidity_IsSelected)
    req(intensity_IsSelected)

    
    getPollutantPaths()
  })
  

  #& as_datetime(timestamp) %within% int
  forecast <- get_forecast_for(41.870, -87.647, "2019-01-01T12:00:00-0600")
  
  print(forecast)
  theAData <- ls.observations(filters=list(project='chicago', sensor="chemsense.co.concentration",  size=1000))

  
  observe({
    autoInvalidate()
    theAData <- ls.observations(filters=list(project='chicago', size=1000))
  })
  
  getNodes <- function(path, d, h){
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="UTC")
    int <- interval(gmtTime - hours(h) - days(d), gmtTime)
    path_list = pollutantPaths()
    c = select(subset(theAData, is.element(sensor_path, path_list) & as.POSIXlt(timestamp, tz="UTC", "%Y-%m-%dT%H:%M") %within% int), 'node_vsn', 'sensor_path', 'timestamp', 'value')
    return (c)
  }

  getNodesNow <- function(path, a){
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="UTC")
    int <- interval(gmtTime - hours(1), gmtTime)
    c = select(subset(theAData, as.POSIXlt(timestamp, tz="UTC", "%Y-%m-%dT%H:%M") %within% int), 'node_vsn')
    return (c)
  }
  
  # Returns all nodes and locations of currently selected items
  getNodeLocations <- function(){
    c <- getNodes("metsense.tsys01.temperature", 0, 1)
    nodes <- unique(c$node_vsn)
    node_addresses <- subset(ls.nodes(filters=list(project='chicago')), (vsn %in% nodes))

    node_a <- select(node_addresses, unique('vsn'), 'address', 'location.geometry')
    locations <- select(node_a, 'vsn', 'address')
    locations$coordinates <- select(node_a$location.geometry, 'coordinates')
    dt <- locations$coordinates
    res <- dt %>%
      rowwise %>%
      mutate(Lat = as.numeric(coordinates[1]), Lon = as.numeric(coordinates[2])) %>%
      ungroup %>%
      select(-coordinates)
    data <- cbind(locations, res)
    locations <- data
    return (locations)
  }
  
  nodeLocations <- reactive({
    autoInvalidate()
    getNodeLocations()
  })
  
  getAllNodes <- function(){
    return (select(ls.nodes(filters=list(project='chicago')), 'vsn'))
  }
  
# Node 1 Input List
  output$node1Output <- renderUI({
    dt <- getAllNodes()
    selectInput("node1Input", "Node 1",
                dt, selected = "072")
  })  
  
# Node 2 Input List
  output$node2Output <- renderUI({
    dt <- getAllNodes()
    selectInput("node2Input", "Node 2",
                dt, selected = "072")
  })  

  

  

  ## !!!!!!!!! Turn these into histograms !!!!!!!!!
    output$node1_cur <- renderPlot({
      req(input$node1Input)
      req(no2_IsSelected)
      req(co_IsSelected)
      req(ozone_IsSelected)
      req(so2_IsSelected)
      req(pm10_IsSelected)
      req(pm25_IsSelected)
      req(h2s_IsSelected)
      req(tempertature_IsSelected)
      req(humidity_IsSelected)
      req(intensity_IsSelected)
    

      no2_data <- getData(input$node1Input, 0, 1, no2_path)
      co_data <- getData(input$node1Input, 0, 1, co_path)
      h2s_data <- getData(input$node1Input, 0, 1, h2s_path)
      so2_data <- getData(input$node1Input, 0, 1, so2_path)
      pm10_data <- getData(input$node1Input, 0, 1, pm10_path)
      pm25_data <- getData(input$node1Input, 0, 1, pm25_path)
      temperature_data <- getData(input$node1Input, 0, 1, temperature_path)
      humidity_data <- getData(input$node1Input, 0, 1, humidity_path)
      intensity_data <- getData(input$node1Input, 0, 1, intensity_path)
      
      if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
        stop(paste("No data avaliavle for node: "),input$node1Input)
      }
      else{
        myplot <- ggplot() 
        if(length(no2_data) >0){
          no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        }
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(temperature_data) > 0){
          temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
        }
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
        myplot
      }
    })
    output$node1_24 <- renderPlot({
      req(input$node1Input)
      req(no2_IsSelected)
      req(co_IsSelected)
      req(ozone_IsSelected)
      req(so2_IsSelected)
      req(pm10_IsSelected)
      req(pm25_IsSelected)
      req(h2s_IsSelected)
      req(tempertature_IsSelected)
      req(humidity_IsSelected)
      req(intensity_IsSelected)
      
      no2_data <- getData(input$node1Input, 1, 0, no2_path)
      co_data <- getData(input$node1Input, 1, 0, co_path)
      h2s_data <- getData(input$node1Input, 1, 0, h2s_path)
      so2_data <- getData(input$node1Input, 1, 0, so2_path)
      pm10_data <- getData(input$node1Input, 1, 0, pm10_path)
      pm25_data <- getData(input$node1Input, 1, 0, pm25_path)
      temperature_data <- getData(input$node1Input, 1, 0, temperature_path)
      humidity_data <- getData(input$node1Input, 1, 0, humidity_path)
      intensity_data <- getData(input$node1Input, 1, 0, intensity_path)
      
      if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
        stop(paste("No data avaliavle for node: "),input$node1Input)
      }
      else{
        no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
        myplot <- ggplot() +
          geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(temperature_data) > 0){
          temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
        }
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
        myplot
      }
    })
    output$node1_7 <- renderPlot({
      req(input$node1Input)
      req(no2_IsSelected)
      req(co_IsSelected)
      req(ozone_IsSelected)
      req(so2_IsSelected)
      req(pm10_IsSelected)
      req(pm25_IsSelected)
      req(h2s_IsSelected)
      req(tempertature_IsSelected)
      req(humidity_IsSelected)
      req(intensity_IsSelected)
      
      no2_data <- getData(input$node1Input, 7, 0, no2_path)
      co_data <- getData(input$node1Input, 7, 0, co_path)
      h2s_data <- getData(input$node1Input, 7, 0, h2s_path)
      so2_data <- getData(input$node1Input, 7, 0, so2_path)
      pm10_data <- getData(input$node1Input, 7, 0, pm10_path)
      pm25_data <- getData(input$node1Input, 7, 0, pm25_path)
      temperature_data <- getData(input$node1Input, 7, 0, temperature_path)
      humidity_data <- getData(input$node1Input, 7, 0, humidity_path)
      intensity_data <- getData(input$node1Input, 7, 0, intensity_path)
      
      if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
        stop(paste("No data avaliavle for node: "),input$node1Input)
      }
      else{
        no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
        myplot <- ggplot() +
          geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(temperature_data) > 0){
          temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
        }
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
        myplot
      }
      
    })
    
  ## !!!!!!!!! Turn these into histograms !!!!!!!!!
    output$node2_cur <- renderPlot({
      
      req(input$node2Input)
      req(no2_IsSelected)
      req(co_IsSelected)
      req(ozone_IsSelected)
      req(so2_IsSelected)
      req(pm10_IsSelected)
      req(pm25_IsSelected)
      req(h2s_IsSelected)
      req(tempertature_IsSelected)
      req(humidity_IsSelected)
      req(intensity_IsSelected)
      
      no2_data <- getData(input$node2Input, 0, 1, no2_path)
      co_data <- getData(input$node2Input, 0, 1, co_path)
      h2s_data <- getData(input$node2Input, 0, 1, h2s_path)
      so2_data <- getData(input$node2Input, 0, 1, so2_path)
      pm10_data <- getData(input$node2Input, 0, 1, pm10_path)
      pm25_data <- getData(input$node2Input, 0, 1, pm25_path)
      temperature_data <- getData(input$node2Input, 0, 1, temperature_path)
      humidity_data <- getData(input$node2Input, 0, 1, humidity_path)
      intensity_data <- getData(input$node2Input, 0, 1, intensity_path)
      
      if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
        stop(paste("No data avaliavle for node: "),input$node1Input)
      }
      else{
        no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
        myplot <- ggplot() +
          geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(temperature_data) > 0){
          temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
        }
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
        myplot
      }
      
    })
    output$node2_24 <- renderPlot({
      req(input$node2Input)
      req(no2_IsSelected)
      req(co_IsSelected)
      req(ozone_IsSelected)
      req(so2_IsSelected)
      req(pm10_IsSelected)
      req(pm25_IsSelected)
      req(h2s_IsSelected)
      req(tempertature_IsSelected)
      req(humidity_IsSelected)
      req(intensity_IsSelected)
      
      no2_data <- getData(input$node2Input, 1, 0, no2_path)
      co_data <- getData(input$node2Input, 1, 0, co_path)
      h2s_data <- getData(input$node2Input, 1, 0, h2s_path)
      so2_data <- getData(input$node2Input, 1, 0, so2_path)
      pm10_data <- getData(input$node2Input, 1, 0, pm10_path)
      pm25_data <- getData(input$node2Input, 1, 0, pm25_path)
      temperature_data <- getData(input$node2Input, 1, 0, temperature_path)
      humidity_data <- getData(input$node2Input, 1, 0, humidity_path)
      intensity_data <- getData(input$node2Input, 1, 0, intensity_path)
      
      if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
        stop(paste("No data avaliavle for node: "),input$node2Input)
      }
      else{
        no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
        myplot <- ggplot() +
          geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(temperature_data) > 0){
          temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
        }
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
        myplot
      }
    })
    output$node2_7 <- renderPlot({
      req(input$node2Input)
      req(no2_IsSelected)
      req(co_IsSelected)
      req(ozone_IsSelected)
      req(so2_IsSelected)
      req(pm10_IsSelected)
      req(pm25_IsSelected)
      req(h2s_IsSelected)
      req(tempertature_IsSelected)
      req(humidity_IsSelected)
      req(intensity_IsSelected)
      
      no2_data <- getData(input$node2Input, 7, 0, no2_path)
      co_data <- getData(input$node2Input, 7, 0, co_path)
      h2s_data <- getData(input$node2Input, 7, 0, h2s_path)
      so2_data <- getData(input$node2Input, 7, 0, so2_path)
      pm10_data <- getData(input$node2Input, 7, 0, pm10_path)
      pm25_data <- getData(input$node2Input, 7, 0, pm25_path)
      temperature_data <- getData(input$node2Input, 7, 0, temperature_path)
      humidity_data <- getData(input$node2Input, 7, 0, humidity_path)
      intensity_data <- getData(input$node2Input, 7, 0, intensity_path)
      
      if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
        stop(paste("No data avaliavle for node: "),input$node1Input)
      }
      else{
        no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
        myplot <- ggplot() +
          geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(temperature_data) > 0){
          temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
        }
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
        myplot
      }
    })
  
  
   res <- get_current_forecast(41.870, -87.647)
   res2 <- aq_latest(country = "US", city = "Chicago-Naperville-Joliet")

   
   
   output$mymap <- renderLeaflet({
     req(pollutantPaths)
     ds <- nodeLocations()  #displays only the current nodes with information (last 1 hour)
     leaflet(ds) %>%
       addTiles() %>%  # Add default OpenStreetMap map tiles
       addMarkers(~Lat, ~Lon, popup = ~as.character(address), label = ~as.character(vsn), layerId = ~vsn)
   })
 
   observeEvent(input$mymap_marker_click, { 
     p <- input$mymap_marker_click
     output$node_data <- renderPlot({
       req(no2_IsSelected)
       req(co_IsSelected)
       req(ozone_IsSelected)
       req(so2_IsSelected)
       req(pm10_IsSelected)
       req(pm25_IsSelected)
       req(h2s_IsSelected)
       req(tempertature_IsSelected)
       req(humidity_IsSelected)
       req(intensity_IsSelected)
       
       no2_data <- getData(p$id, 0,1, no2_path)
       co_data <- getData(p$id, 0,1, co_path)
       h2s_data <- getData(p$id, 0,1, h2s_path)
       so2_data <- getData(p$id, 0,1, so2_path)
       pm10_data <- getData(p$id, 0,1, pm10_path)
       pm25_data <- getData(p$id, 0,1, pm25_path)
       temperature_data <- getData(p$id, 0,1, temperature_path)
       humidity_data <- getData(p$id, 0,1, humidity_path)
       intensity_data <- getData(p$id, 0,1, intensity_path)
       
       if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
         stop(paste("No data avaliavle for node: "),input$node1Input)
       }
       else{
         no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
         myplot <- ggplot() +
           geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
         if(length(co_data) > 0 ){
           co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
         }
         if(length(h2s_data) > 0){
           h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
         }
         if(length(so2_data) > 0){
           so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
         }
         if(length(pm10_data) > 0){
           pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
         }
         if(length(temperature_data) > 0){
           temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
         }
         if(length(humidity_data) > 0){
           humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
         }
         if(length(intensity_data) > 0){
           intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
         }
         
         
         myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
         myplot
       }

     })
     output$node_data24 <- renderPlot({
       req(no2_IsSelected)
       req(co_IsSelected)
       req(ozone_IsSelected)
       req(so2_IsSelected)
       req(pm10_IsSelected)
       req(pm25_IsSelected)
       req(h2s_IsSelected)
       req(tempertature_IsSelected)
       req(humidity_IsSelected)
       req(intensity_IsSelected)
       
       no2_data <- getData(p$id, 1, 0, no2_path)
       co_data <- getData(p$id, 1, 0, co_path)
       h2s_data <- getData(p$id, 1, 0, h2s_path)
       so2_data <- getData(p$id, 1, 0, so2_path)
       pm10_data <- getData(p$id, 1, 0, pm10_path)
       pm25_data <- getData(p$id, 1, 0, pm25_path)
       temperature_data <- getData(p$id, 1, 0, temperature_path)
       humidity_data <- getData(p$id, 1, 0, humidity_path)
       intensity_data <- getData(p$id, 1, 0, intensity_path)
       
       if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
         stop(paste("No data avaliavle for node: "),input$node1Input)
       }
       else{
         no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
         myplot <- ggplot() +
           geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
         if(length(co_data) > 0 ){
           co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
         }
         if(length(h2s_data) > 0){
           h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
         }
         if(length(so2_data) > 0){
           so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
         }
         if(length(pm10_data) > 0){
           pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
         }
         if(length(temperature_data) > 0){
           temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
         }
         if(length(humidity_data) > 0){
           humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
         }
         if(length(intensity_data) > 0){
           intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
         }
         
         
         myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
         myplot
       }
     })
     output$node_data7 <- renderPlot({
       req(no2_IsSelected)
       req(co_IsSelected)
       req(ozone_IsSelected)
       req(so2_IsSelected)
       req(pm10_IsSelected)
       req(pm25_IsSelected)
       req(h2s_IsSelected)
       req(tempertature_IsSelected)
       req(humidity_IsSelected)
       req(intensity_IsSelected)
       
       no2_data <- getData(p$id, 7, 0, no2_path)
       co_data <- getData(p$id, 7, 0, co_path)
       h2s_data <- getData(p$id, 7, 0, h2s_path)
       so2_data <- getData(p$id, 7, 0, so2_path)
       pm10_data <- getData(p$id, 7, 0, pm10_path)
       pm25_data <- getData(p$id, 7, 0, pm25_path)
       temperature_data <- getData(p$id, 7, 0, temperature_path)
       humidity_data <- getData(p$id, 7, 0, humidity_path)
       intensity_data <- getData(p$id, 7, 0, intensity_path)
       
       if(length(no2_data) == 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0){
         stop(paste("No data avaliavle for node: "),input$node1Input)
       }
       else{
         no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
         myplot <- ggplot() +
           geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
         if(length(co_data) > 0 ){
           co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
         }
         if(length(h2s_data) > 0){
           h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
         }
         if(length(so2_data) > 0){
           so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
         }
         if(length(pm10_data) > 0){
           pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
         }
         if(length(temperature_data) > 0){
           temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
         }
         if(length(humidity_data) > 0){
           humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
         }
         if(length(intensity_data) > 0){
           intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
         }
         
         
         myplot <- myplot + geom_point() + scale_colour_manual(values=c("red", "green", "blue", "purple", "orange", "yellow", "grey"))
         myplot
       }
     })
     
   })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

