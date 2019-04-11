#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(AotClient) 
library(darksky)
library("ropenaq")
library(lubridate)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(qdapTools)
library(data.table)
pollutants_list = list("NO2", "Ozone", "CO", "H2S", "SO2", "PM10", "PM25", "Temperature", "Humidity", "Light")
# Define UI for application that draws a histogram
ui <- dashboardPage(
  ################################################################################ THE COLOR AND LENGTH OF THE TITLE FOR THE SIDEBAR ################################################################################
  skin = "yellow",
  dashboardHeader(title = "CS 424 PROJECT 2", titleWidth = 450 ),
  
  ######################################## CREATE DROP DOWN MENUS IN SIDEBAR + NEW TAB CONTAINING RESOURCES ######################################## 
  dashboardSidebar(sidebarMenu(disable = FALSE, collapsed = FALSE,
                               uiOutput("nodeOutput"),
                               uiOutput("node1Output"),
                               uiOutput("node2Output"),
                               selectInput("pollutants", "Pollutants", pollutants_list)
                              
  )),
  ######################################## THE MAIN BODDY OF THE WEB APP ########################################
  dashboardBody(
    fluidRow(leafletOutput("mymap", height = 600)),
    fluidRow(column(6,h4(textOutput("Node 1")),
               tabsetPanel(
                 tabPanel("Current",tableOutput("node1_cur")),
                 tabPanel("24 Hours", tableOutput("node1_24")),
                 tabPanel("7 Days",tableOutput("node1_7"))
               )
             ),
             column(6,h4(textOutput("Node 2")),
                    tabsetPanel(
                      tabPanel("Current",tableOutput("node2_cur")),
                      tabPanel("24 Hours", tableOutput("node2_24")),
                      tabPanel("7 Days",tableOutput("node2_7"))
                    )
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
server <- function(input, output) {
  
  autoInvalidate <- reactiveTimer(1000*60)
  
  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    print("Validated")
  })
  
  
  no2_path = "chemsense.no2.concentration"
  ozone_path = "chemsense.o3.concentration"
  co_path = "chemsense.co.concentration"
  h2s_path = "chemsense.h2s.concentration"
  so2_path = "chemsense.so2.concentration"
  pm10_paths = list("alphasense.opc_n2.pm10", "plantower.pms7003.pm10_atm")
  pm25_paths = list("alphasense.opc_n2.pm2_5", "plantower.pms7003.pm25_atm")
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
  humidity_paths = list("metsense.hih4030.humidity",
                        "metsense.htu21d.humidity",
                        "chemsense.sht25.humidity")
  intensity_path = "chemsense.si1145.visible_light_intensity "
  
  
  getNodes <- function(path, d, h){
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="GMT")
    int <- interval(gmtTime - hours(h) - days(d), gmtTime)
    print(gmtTime)
    a = ls.observations(filters=list(project='chicago', size=50))
    c = select(subset(a, sensor_path == path | (sensor_path %in% temperature_paths) & as_datetime(timestamp) %within% int), 'node_vsn', 'sensor_path', 'timestamp', 'value')
    print(c)
    return (c)
  }

  
  getNodesNow <- function(path){
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="GMT")
    int <- interval(gmtTime - hours(1), gmtTime)
    print(gmtTime)
    a = ls.observations(filters=list(project='chicago', size=50))
    c = select(subset(a, sensor_path == path | (sensor_path %in% temperature_paths) & as_datetime(timestamp) %within% int), 'node_vsn')
    print(c)
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
    return (locations)
  }
  
  # Returns Data of current node selected
  getNodeData<- function(vsn, d, h){
      nodes <- getNodes("metsense.tsys01.temperature", d, h)
      return (filter(nodes, node_vsn == vsn))
  }
  
  
  getAllNodes <- function(){
    return (select(ls.nodes(filters=list(project='chicago')), 'vsn'))
  }
  
  
  output$nodeOutput <- renderUI({
    dt <- getNodesNow("metsense.tsys01.temperature")
    selectInput("nodeInput", "Current Nodes",
                dt)
  })  
  
  output$node1Output <- renderUI({
    dt <- getNodesNow("metsense.tsys01.temperature")
    selectInput("node1Input", "Current Nodes",
                dt)
  })  
  
  output$node2Output <- renderUI({
    dt <- getNodesNow("metsense.tsys01.temperature")
    selectInput("node2Input", "Current Nodes",
                dt)
  })  
  
  #Node selected on map current data
  curNodeData <- reactive({
    req(input$nodeInput)
    autoInvalidate()
    getNodeData(input$nodeInput, 0, 1)
  })
  
  #Node selected on map last 24 hours of data
  curNodeData24 <- reactive({
    req(input$nodeInput)
    autoInvalidate()
    getNodeData(input$nodeInput, 0, 24)
  })
  
  
  #Data for node 1 selected for last 24 hours
  node1SelectedData24 <- reactive({
    req(input$node1Input)
    autoInvalidate()
    getNodeData(input$node1Input, 0, 24)
  })
  
  #Data for node 2 selected for last 24 hours
  node2SelectedData24 <- reactive({
    req(input$node2Input)
    autoInvalidate()
    getNodeData(input$node2Input, 0, 24)
  })
  
  #Data for node 1 selected for Current time
  node1SelectedDataCur <- reactive({
    req(input$node1Input)
    autoInvalidate()
    getNodeData(input$node1Input, 0, 1)
  })
  
  #Data for node 2 selected for Current time
  node2SelectedDataCur <- reactive({
    req(input$node2Input)
    autoInvalidate()
    getNodeData(input$node2Input, 0, 1)
  })
  
  #Data for node 1 selected for 7 Days
  node1SelectedData7 <- reactive({
    req(input$node1Input)
    autoInvalidate()
    getNodeData(input$node2Input, 7, 0)
  })
  
  #Data for node 2 selected for 7 Days
  node2SelectedData7 <- reactive({
    req(input$node2Input)
    autoInvalidate()
    getNodeData(input$node2Input, 7, 0)
  })
  
    
  ## !!!!!!!!! Turn these into histograms !!!!!!!!!
    output$node1_cur <- renderTable({
      node1SelectedDataCur()
    })
    output$node1_24 <- renderTable({
      node1SelectedData24()
    })
    output$node1_7 <- renderTable({
      node1SelectedData7()
    })
    
  ## !!!!!!!!! Turn these into histograms !!!!!!!!!
    output$node2_cur <- renderTable({
      node2SelectedDataCur()
    })
    output$node2_24 <- renderTable({
      node2SelectedData24()
    })
    output$node2_7 <- renderTable({
      node2SelectedData7()
    })
  
  
  
  # print(getNodes("metsense.tsys01.temperature"))
  df <- ls.sensors(filters=list(node="004"))
   # print(df)
   d <- stat.node("053")
   # print(d)
   a = ls.observations()
   b = subset(a, node_vsn == "01C")
   c = subset(b, sensor_path == "metsense.bmp180.temperature")
   
   res <- get_current_forecast(41.870, -87.647)
   res2 <- aq_latest(country = "US", city = "Chicago-Naperville-Joliet")
   print("*** Projects ***")

   output$mymap <- renderLeaflet({
     ds <- getNodeLocations()  #displays only the current nodes with information (last 1 hour)
     leaflet(ds) %>%
       addTiles() %>%  # Add default OpenStreetMap map tiles
       addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
   })
   observe({
     print(getNodes("metsense.bmp180.temperature",0, 1))
   })
    print("here")
   
    ## Data Table I am trying to separate coordinates column ###
    dt <- getNodeLocations()$coordinates
    
    
   print(dput(dt))
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

