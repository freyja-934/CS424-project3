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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

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
  
  # Returns all nodes and observation data based on selected items
  getNodes <- function(path){
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="GMT")
    int <- interval(gmtTime - hours(1), gmtTime)
    print(gmtTime)
    a = ls.observations(filters=list(project='chicago', size=500))
    c = select(subset(a, sensor_path == path | (sensor_path %in% temperature_paths) & as_datetime(timestamp) %within% int), 'node_vsn', 'sensor_path', 'timestamp', 'value')
    print(c)
    return (c)
  }
  
  # Returns all nodes and locations of currently selected items
  getNodeLocations <- function(){
    c <- getNodes("metsense.tsys01.temperature")
    nodes <- unique(c$node_vsn)
    node_addresses <- subset(ls.nodes(filters=list(project='chicago')), (vsn %in% nodes))
    node_a <- select(node_addresses, unique('vsn'), 'address', 'location.geometry')
    locations <- select(node_a, 'vsn', 'address')
    locations$coordinates <- select(node_a$location.geometry, 'coordinates')
    return (locations)
  }
  
  # Returns Data of current node selected
  getNodeData <- function(vsn){
    nodes <- getNodes("metsense.tsys01.temperature")
    return (filter(nodes, node_vsn == vsn))
  }
  
  getAllNodes <- function(){
    return (select(ls.nodes(filters=list(project='chicago')), 'vsn'))
  }
  
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

   # print(ls.nodes(filters=list(project='chicago')))
   #print(names(ls.sensors(filters=list(project='chicago'))))
   getNodes("metsense.tsys01.temperature")
   
   print(getNodeData("020"))
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

