#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
################### key: 305d59068af82054adce3f22e00d7495
########### c("#B92886", "#0088FF", "#00FFD4", "#FF7171", "#24791D", "#5F4242", "#CF65E6","#48A900", "#47768A", "#000000")


library(shiny)
library(DT)
library(AotClient) 
library(darksky)
library(ropenaq)
library(lubridate)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(qdapTools)
library(data.table)
library(jsonlite)
library(leaflet.extras)
library(magrittr)

pollutants_list = list("NO2", "Ozone", "CO", "H2S", "SO2", "PM10", "PM25", "Temperature", "Humidity", "Light")
map_list = list("Default", "Map 1", "Map 2", "Map 3")
dates_list = list("Current", "24 Hours", "7 Days")
Sys.setenv(DARKSKY_API_KEY = 'f08f162526b4e1f7b6d379ee3eb574be')
# Define UI for application that draws a histogram
ui <- dashboardPage(
  ################################################################################ THE COLOR AND LENGTH OF THE TITLE FOR THE SIDEBAR ################################################################################
  skin = "yellow",
  dashboardHeader(title = "CS 424 PROJECT 3-Group 2", titleWidth = 350 ),
  
  ######################################## CREATE DROP DOWN MENUS IN SIDEBAR + NEW TAB CONTAINING RESOURCES ######################################## 
  dashboardSidebar(sidebarMenu(disable = FALSE, collapsed = FALSE,  style = "margin-top:350px",
                               radioButtons("units", "Units",
                                            c("Metric" = "met",
                                              "Imperial" = "imp")),
                               uiOutput("nodeOutput"),
                               uiOutput("node1Output"),
                               uiOutput("node2Output"),
                               selectInput("TimeFrame", "Time Frame", c("Current", "24 Hours", "7 Days")),
                               #selectInput("Maps", "Map Color", map_list, selected = "Default"),
                               menuItem("Dashboards", icon = icon("dashboard"), startExpanded = FALSE,
                                menuSubItem("Node Map", tabName="map", icon = icon("map")),
                                #menuSubItem("Comparison", tabName="compare", icon = icon("dashboard")),
                                menuSubItem("Heat Map", tabName="heatmap", icon = icon("map")),
                                menuSubItem("Compare Nodes", tabName="compare2", icon = icon("dashboard"))),
                                #menuSubItem("Dark Sky test", tabName="darkskyT", icon = icon("dashboard")),
                                #menuSubItem("OpenAQ test", tabName="openAQT", icon = icon("dashboard"))),
                               menuItem("Choose AoT Data", icon = icon("dashboard"), startExpanded = FALSE,
                                        checkboxInput("NO2", "NO2", TRUE),
                                        checkboxInput("OZONE", "OZONE", TRUE),
                                        checkboxInput("CO", "CO", TRUE),
                                        checkboxInput("H2S", "H2S", FALSE),
                                        checkboxInput("SO2", "SO2", FALSE),
                                        checkboxInput("PM10", "PM10", FALSE),
                                        checkboxInput("PM25", "PM25", FALSE),
                                        checkboxInput("TEMPERATURE", "TEMPERATURE", FALSE),
                                        checkboxInput("HUMIDITY", "HUMIDITY", FALSE),
                                        checkboxInput("INTENSITY", "INTENSITY", FALSE)),
                               menuItem("Choose Dark Sky Data", icon = icon("dashboard"), startExpanded = FALSE,
                                        checkboxInput("TEMPERATURE_DS", "TEMPERATURE", TRUE),
                                        checkboxInput("HUMIDITY_DS", "HUMIDITY", FALSE),
                                        checkboxInput("WINDSPEED", "WIND SPEED", TRUE),
                                        checkboxInput("WINDBEARING", "WIND BEARING", FALSE),
                                        checkboxInput("CLOUDCOVER", "CLOUD COVER", FALSE),
                                        checkboxInput("VISIBILITY", "VISIBILITY", FALSE),
                                        checkboxInput("PRESSURE", "PRESSURE", FALSE),
                                        checkboxInput("OZONE_DS", "OZONE", FALSE),
                                        checkboxInput("SUMMARY", "SUMMARY", FALSE)
                                        ),
                               menuItem("Choose OpenAQ Data", icon = icon("dashboard"), startExpanded = FALSE,
                                        checkboxInput("PM25_AQ", "PM25", TRUE),
                                        checkboxInput("PM10_AQ", "PM10", FALSE),
                                        checkboxInput("SO2_AQ", "SO2", FALSE),
                                        checkboxInput("NO2_AQ", "NO2", FALSE),
                                        checkboxInput("O3_AQ", "O3", TRUE),
                                        checkboxInput("CO_AQ", "CO", FALSE),
                                        checkboxInput("BC_AQ", "BC", FALSE)),
                               menuItem("Resources", tabName="resources", icon = icon("bullet"))
                               
                               
                               #temperature, humidity, wind speed, wind bearing, cloud cover, visibility, pressure, ozone, summary
                              
                              
  )),
  ######################################## THE MAIN BODDY OF THE WEB APP ########################################
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(column(6,
                        htmlOutput("title2"),
                        leafletOutput("mymap", height = 800),
                        box(title = "NoDE AOT DATA TABLE", solidHeader = TRUE, status = "primary",dataTableOutput("node_table_data"), width = 4),
                        box(title = "NoDE DARK SKY DATA TABLE", solidHeader = TRUE, status = "primary",dataTableOutput("node_DS_table_data"), width = 6),
                        box(title = "NoDE OPENAQ TABLE", solidHeader = TRUE, status = "primary",dataTableOutput("node_AQ_table_data"), width = 2)
                        
        ),
        column(6,
               box(title = "AOT NODE DATA", solidHeader = TRUE, status = "primary",plotOutput("node_data"),width = 12),
               box(title = "NoDE DARK SKY DATA", solidHeader = TRUE, status = "primary",plotOutput("node_DS_data"), width = 12),
               box(title = "NoDE OPENAQ DATA", solidHeader = TRUE, status = "primary",plotOutput("node_AQ_data"), width = 12)
        )
        
        )
      ),
        
    #   tabItem(
    #     tabName = "compare",
    #      fluidRow(
    #        # column(6,h4(textOutput("Node 1")),
    #     #       
    #     #        tabsetPanel(type = "tabs",
    #     #          tabPanel("Current",plotOutput("node1_cur")),
    #     #          tabPanel("24 Hours", plotOutput("node1_24")),
    #     #          tabPanel("7 Days",tableOutput("node1_7")),
    #     #          position = "below"
    #     #        )
    #     #      ),
    #          column(6,h4(textOutput("Node 2")),
    #                 tabsetPanel( type = "tabs",
    #                   tabPanel("Current",plotOutput("node2_cur")),
    #                   tabPanel("24 Hours", plotOutput("node2_24")),
    #                   tabPanel("7 Days",plotOutput("node2_7")),
    #                   position = "left"
    #                 )
    #          )
    # )),
    tabItem(
      tabName = "resources",
      h1("PROJECT WEB PAGE: https://caseyleigh54.github.io/CS424-Project3-Site/ "),
      h2("Resources used in this project:"),
      h3("All data used is from here: https://api.arrayofthings.org/api/observations?location=chicago, https://darksky.net/dev,https://github.com/ropensci/ropenaq"),
      h3("Base Code and Code influence from Professor Andy Johnson, https://www.evl.uic.edu/aej/424/ (week 2), https://www.evl.uic.edu/aej/424/ (PROJECT 3, 2, 1)"),
      h4("ASSIGNMENT PROJECT PAGE: https://www.evl.uic.edu/aej/424/"),
      h4("librarys used: shiny, DT, AotClient, darksky, ropenaq, lubridate, tidyverse, dplyr, shinydashboard, leaflet, qdapTools, data.table, jsonlite, leaflet.extras, magrittr"),
      h4("Techniques and methods adapted from:"),
      h6("#####################################################3"),
      h6("  *https://medium.com/array-of-things/five-years-100-nodes-and-more-to-come-d3802653db9f"),
      h6("  *https://arrayofthings.github.io/"),
      h6("  *https://github.com/waggle-sensor/sensors/blob/develop/README.md"),
      h6("  *https://arrayofthings.docs.apiary.io/#introduction/api-endpoints"),
      h6("  *https://github.com/UrbanCCD-UChicago/aot-client-r"),
      h6("  *https://api.arrayofthings.org/api/nodes"),
      h6("  *https://aot-file-browser.plenar.io/data-sets/chicago-complete"),
      h6("  *https://CRAN.R-project.org/package=darksky"),
      h6("  *https://github.com/hrbrmstr/darksky"),
      h6("  *https://darksky.net/dev"),
      h6("  *https://github.com/ropensci/ropenaq"),
      h6("  *https://ropensci.org/tutorials/ropenaq_tutorial/"),
      h6("  *https://groups.google.com/forum/#!topic/shiny-discuss/ugNEaHizlck"),
      h6("  *https://shiny.rstudio.com/reference/shiny/1.0.2/radioButtons.html"),
      h6("  *https://stackoverflow.com/questions/23279550/select-every-nth-row-from-dataframe"),
      h6("  *http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization"),
      h6("  *https://stackoverflow.com/questions/28008177/r-language-how-to-print-the-first-or-last-rows-of-a-data-set"),
      h6("  *https://stackoverflow.com/questions/35990183/how-can-i-control-the-font-size-of-an-infobox-in-a-shiny-dashboard "),
      h6("  *https://datascienceplus.com/visualising-thefts-using-heatmaps-in-ggplot2/"),
      h6("  *https://shiny.rstudio.com/reference/shiny/0.11.1/htmlOutput.html"),
      h6("  *https://stackoverflow.com/questions/23279550/select-every-nth-row-from-dataframe"),
      h6("  *https://stackoverflow.com/questions/39814314/use-dynamic-radiobuttons-in-shiny"),
      h6("  *https://shiny.rstudio.com/reference/shiny/1.0.4/renderUI.html"),
      h6("  *https://stackoverflow.com/questions/26869141/conditionally-hiding-data-labels-in-ggplot2-graph"),
      h6("  *https://shiny.rstudio.com/reference/shiny/1.0.1/updateRadioButtons.html"),
      h6("  *https://shiny.rstudio.com/reference/shiny/1.0.2/radioButtons.html"),
      h6("  *http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/"),
      h6("  *https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated"),
      h6("  *https://stackoverflow.com/questions/50808905/render-title-in-r-shiny-box-dynamically"),
      h6("  *https://shiny.rstudio.com/reference/shiny/0.14/updateSelectInput.html"),
      h6("  *https://www.maketecheasier.com/rename-files-in-linux/"),
      h6("  *http://shiny.rstudio.com/articles/shinyapps.html"),
      h6("  *https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated"),
      h6("  *http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/"),
      h6("  *https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/"),
      h6("  *https://stackoverflow.com/questions/33266157/how-to-add-more-whitespace-to-the-main-panel-in-shiny-dashboard"),
      h6("  *https://davidmathlogic.com/colorblind/#%23B92886-%230088FF-%2300FFD4-%23FF7171-%2324791D-%235F4242-%23CF65E6-%2348A900-%2347768A-%23000000"),
      h6("  *and finally Professor Andy Johnson and Sai Priya Jyothula"),
      h3("PROJECT LINKS:"),
      h4("Project 1: https://nlaczn2.shinyapps.io/assignment1/"),
      h4("Project 2: http://shiny.evl.uic.edu:3838/g2/project2/"),
      h4("Project 3: https://caseyleigh54.github.io/CS424-Project3-Site/")
      
    ),
    tabItem(
      tabName = "heatmap",
      # 
      leafletOutput("heatmap", height = 1000),
      radioButtons("dataType", choices = c("AOT"= "AOT_HM", "DARK SKY"= "DARK_SKY_HM", "OPEN_AQ" = "OPENAQ_HM"), label = "CHOOSE A DATA SET",inline = TRUE),
      uiOutput("contents"),
      uiOutput("contents2")

    ),
    tabItem(
      tabName = "compare2", fluidRow(
          box(title = textOutput("titleNode1"), width= 12, plotOutput("node1_cur"), height = 450),
          box(title = textOutput("titleNode2") ,width= 12, plotOutput("node2_cur"), height = 450),
          
          box(title = textOutput("titleNode1T"), width= 6, tabsetPanel( type = "tabs",
                       tabPanel("NO2",dataTableOutput("NO2_1")),
                       tabPanel("CO", dataTableOutput("CO_1")),
                       tabPanel("H2S", dataTableOutput("H2S_1")),
                       tabPanel("SO2", dataTableOutput("SO2_1") ),
                       tabPanel("PM10", dataTableOutput("PM10_1")),
                       tabPanel("PM25",  dataTableOutput("PM25_1")),
                       tabPanel("OZONE", dataTableOutput("OZONE_1")),
                       tabPanel("TEMPERATURE", dataTableOutput("TEMPERATURE_1")),
                       tabPanel("HUMIDITY", dataTableOutput("HUMIDITY_1")),
                       tabPanel("INTENSITY", dataTableOutput("INTENSITY_1")))),
          
          box(title = textOutput("titleNode2T"), width= 6, tabsetPanel( type = "tabs",
                       tabPanel("NO2",dataTableOutput("NO2_2")),
                       tabPanel("CO", dataTableOutput("CO_2")),
                       tabPanel("H2S", dataTableOutput("H2S_2")),
                       tabPanel("SO2", dataTableOutput("SO2_2") ),
                       tabPanel("PM10", dataTableOutput("PM10_2")),
                       tabPanel("PM25",  dataTableOutput("PM25_2")),
                       tabPanel("OZONE", dataTableOutput("OZONE_2")),
                       tabPanel("TEMPERATURE", dataTableOutput("TEMPERATURE_2")),
                       tabPanel("HUMIDITY", dataTableOutput("HUMIDITY_2")),
                       tabPanel("INTENSITY", dataTableOutput("INTENSITY_2"))))
                  
    
    )
    ),
    
    ################# Dark Sky ######################
    tabItem(
      tabName = "darkskyT",
      verbatimTextOutput("TEMPERATURE_DS"),
      verbatimTextOutput("HUMIDITY_DS"),
      verbatimTextOutput("WINDSPEED"),
      verbatimTextOutput("WINDBEARING"),
      verbatimTextOutput("CLOUDCOVER"),
      verbatimTextOutput("VISIBILITY"),
      verbatimTextOutput("PRESSURE"),
      verbatimTextOutput("OZONE_DS"),
      verbatimTextOutput("SUMMARY")
    ),
    tabItem(
      tabName = "openAQT",
      verbatimTextOutput("PM25_AQ"),
      verbatimTextOutput("PM10_AQ"),
      verbatimTextOutput("SO2_AQ"),
      verbatimTextOutput("NO2_AQ"),
      verbatimTextOutput("O3_AQ"),
      verbatimTextOutput("CO_AQ"),
      verbatimTextOutput("BC_AQ")
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
 
  
  ################################ RADIO BUTTONS FOR HEAT MAP ###################################3
  output$contents <- renderUI({
    if(input$dataType == "AOT_HM"){
      radioButtons("aotData", choices = c("SO2" = "AOT_SO2_HM",
                                          "H2S" = "AOT_H2S_HM",
                                          "O3" = "AOT_O3_HM",
                                          "NO2" = "AOT_NO2_HM",
                                          "CO" = "AOT_CO_HM",
                                          "PM25" = "AOT_PM25_HM",
                                          "PM10" = "AOT_PM10_HM",
                                          "TEMPERATURE" = "AOT_TEMPERATURE_HM",
                                          "HUMIDITY" = "AOT_HUMIDITY_HM",
                                          "LIGHT INTENSITY" = "AOT_LIGHT_INTENSITY_HM"),
                   label = "CHOOSE A PARAMETER", inline = TRUE)
    } else if(input$dataType == "DARK_SKY_HM"){
      radioButtons("dsData", choices = c("DARK SKY TEMPERATURE" = "DS_TEMPERATURE_HM",
                                         "DARK SKY HUMIDITY" = "DS_HUMIDITY_HM",
                                         "WIND SPEED" = "DS_WIND_SPEED_HM",
                                         "WIND BEARING" = "DS_WIND_BEARING_HM",
                                         "CLOUD COVER" = "DS_CLOUD_COVER_HM",
                                         "VISIBILITY" = "DS_VISIBILITY_HM",
                                         "PRESSURE" = "DS_PRESSURE_HM",
                                         "OZONE" = "DS_OZONE_HM",
                                         "SUMMARY" = "DS_SUMMARY_HM"),
                   label = "CHOOSE A PARAMETER", inline = TRUE)
    }
    else{
      radioButtons("aqData", choices = c("SO2" = "AQ_SO2_HM",
                                          "O3" = "AQ_O3_HM",
                                          "NO2" = "AQ_NO2_HM",
                                          "CO" = "AQ_CO_HM",
                                          "PM25" = "AQ_PM25_HM",
                                          "PM10" = "AQ_PM10_HM",
                                          "BC" = "AQ_BC_HM"),
                   label = "CHOOSE A PARAMETER", inline = TRUE)
    }
  })
  
  output$contents2 <- renderUI({
      radioButtons("aotType", choices = c("MIN" = "MIN_HM",
                                          "MAX" = "MAX_HM",
                                          "AVERAGE" = "MEAN_HM"), 
                   label = "CHOOSE A DATA TYPE", inline = TRUE)
   
  })


  
  getNodeData2 <- function(vsn, d,h, path){
    if(d==0){
      size="1000"
    }else if(d==1){
      size="20000"
    }else{
      size = "50000"
    }
    # url <- "https://api.arrayofthings.org/api/observations?location=chicago&node="
    # url <- paste(url, vsn,"&timestamp=","ge:2018-08-01T00:00:00&size=",size,"&sensor=",path, sep="")
    # s <- download.file(url, "obs.html", quiet = FALSE) 
    # t = fromJSON("obs.html")
    # u = t$data
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="UTC")
    int <- interval(gmtTime - hours(h) - days(d), gmtTime)

    
    if(length(u) == 0){
      return (u)
    }
    return(select(u,'node_vsn', 'sensor_path', 'timestamp', 'value'))
  }
 
  
  no2_path = "chemsense.no2.concentration"
  ozone_path = "chemsense.o3.concentration"
  co_path = "chemsense.co.concentration"
  h2s_path = "chemsense.h2s.concentration"
  so2_path = "chemsense.so2.concentration"
  pm10_path = "alphasense.opc_n2.pm10" #"alphasense.opc_n2.pm10"
  pm25_path =  "plantower.pms7003.pm25_atm" #"alphasense.opc_n2.pm2_5"
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
 # humidity_path = "chemsense.sht25.humidity"
  humidity_path = "metsense.hih4030.humidity"
  humidity_paths = list("metsense.hih4030.humidity",
                        "metsense.htu21d.humidity",
                        "chemsense.sht25.humidity")
  intensity_path = "chemsense.si1145.visible_light_intensity"
  
  
  ################################################################## heat map #############
  
#   output$heatmap <- renderLeaflet({
#     map  <- leaflet() %>% 
#       addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
#       addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
#       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
#       addHeatmap(lng = 41, lat = 57, group = "HeatMap", blur = 11, max = 0.10, radius = 10) %>%
#       addHeatmap(lng = 40, lat = 57, group = "HeatMap", blur = 11, max = 0.10, radius = 10) %>%
#       addHeatmap(lng = 39, lat = 57, group = "HeatMap", blur = 11, max = 0.10, radius = 10) %>%
#       addHeatmap(lng = 38, lat = 57, group = "HeatMap", blur = 11, max = 0.10, radius = 10) %>%
#       addHeatmap(lng = 38, lat = 57, group = "HeatMap", blur = 11, max = 0.10, radius = 10) %>%
#       addHeatmap(lng = 38, lat = 57, group = "HeatMap", blur = 11, max = 0.10, radius = 10) %>%
#       setView(41, 50, zoom = 4) %>% 
#       addLayersControl(position = "bottomleft", baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"), options = layersControlOptions(collapsed = FALSE))
#   
#   map
# })
#   
  
  ############################################################ Aot check boxes ##############
  
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
      h2s_IsSelected <- FALSE
    }else{
      h2s_IsSelected <- TRUE
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

  
  ################################################################# Dark Sky check boxes ##############
  
  # verbatimTextOutput("TEMPERATURE_DS"),
  # verbatimTextOutput("HUMIDITY_DS"),
  # verbatimTextOutput("WINDSPEED"),
  # verbatimTextOutput("WINDBEARING"),
  # verbatimTextOutput("CLOUDCOVER"),
  # verbatimTextOutput("VISIBILITY"),
  # verbatimTextOutput("PRESSURE"),
  # verbatimTextOutput("OZONE_DS"),
  # verbatimTextOutput("SUMMARY")
  
  output$TEMPERATURE_DS <- renderText({ input$TEMPERATURE_DS })
  output$HUMIDITY_DS <- renderText({ input$HUMIDITY_DS })
  output$WINDSPEED <- renderText({ input$WINDSPEED })
  output$WINDBEARING <- renderText({ input$WINDBEARING })
  output$CLOUDCOVER <- renderText({ input$CLOUDCOVER })
  output$VISIBILITY <- renderText({ input$VISIBILITY })
  output$PRESSURE <- renderText({ input$PRESSURE })
  output$OZONE_DS <- renderText({ input$OZONE_DS})
  output$SUMMARY <- renderText({ input$SUMMARY })
  
  # Use these for the check boxes
  TEMPERATURE_DS_IsSelected <- reactive({
    if(input$TEMPERATURE_DS == FALSE){
      TEMPERATURE_DS_IsSelected <- FALSE
    }else{
      TEMPERATURE_DS_IsSelected <- TRUE
    }
  })
  HUMIDITY_DS_IsSelected <- reactive({
    if(input$HUMIDITY_DS == FALSE){
      HUMIDITY_DS_IsSelected <- FALSE
    }else{
      HUMIDITY_DS_IsSelected <- TRUE
    }
  })
  WINDSPEED_IsSelected <- reactive({
    if(input$WINDSPEED == FALSE){
      WINDSPEED_IsSelected <- FALSE
    }else{
      WINDSPEED_IsSelected <- TRUE
    }
  })
  WINDBEARING_IsSelected <- reactive({
    if(input$WINDBEARING == FALSE){
      WINDBEARING_IsSelected <- FALSE
    }else{
      WINDBEARING_IsSelected <- TRUE
    }
  })
  CLOUDCOVER_IsSelected <- reactive({
    if(input$CLOUDCOVER == FALSE){
      CLOUDCOVER_IsSelected <- FALSE
    }else{
      CLOUDCOVER_IsSelected <- TRUE
    }
  })
  VISIBILITY_IsSelected <- reactive({
    if(input$VISIBILITY == FALSE){
      VISIBILITY_IsSelected <- FALSE
    }else{
      VISIBILITY_IsSelected <- TRUE
    }
  })
  PRESSURE_IsSelected <- reactive({
    if(input$PRESSURE == FALSE){
      PRESSURE_IsSelected <- FALSE
    }else{
      PRESSURE_IsSelected <- TRUE
    }
  })
  OZONE_DS_IsSelected <- reactive({
    if(input$OZONE_DS == FALSE){
      OZONE_DS_IsSelected <- FALSE
    }else{
      OZONE_DS_IsSelected <- TRUE
    }
  })
  SUMMARY_IsSelected <- reactive({
    if(input$SUMMARY == FALSE){
      SUMMARY_IsSelected <- FALSE
    }else{
      SUMMARY_IsSelected <- TRUE
    }
  })
  
  
  ######################################################################### OPEN AQ CHECKBOXES
  
  output$PM25_AQ <- renderText({ input$PM25_AQ })
  output$PM10_AQ <- renderText({ input$PM10_AQ })
  output$SO2_AQ <- renderText({ input$SO2_AQ })
  output$NO2_AQ <- renderText({ input$NO2_AQ })
  output$O3_AQ <- renderText({ input$O3_AQ })
  output$CO_AQ <- renderText({ input$CO_AQ })
  output$BC_AQ <- renderText({ input$BC_AQ })
  
  # Use these for the check boxes
  PM25_AQ_IsSelected <- reactive({
    if(input$PM25_AQ == FALSE){
      PM25_AQ_IsSelected <- FALSE
    }else{
      PM25_AQ_IsSelected <- TRUE
    }
  })
  PM10_AQ_IsSelected <- reactive({
    if(input$PM10_AQ == FALSE){
      PM10_AQ_IsSelected <- FALSE
    }else{
      PM10_AQ_IsSelected <- TRUE
    }
  })
  SO2_AQ_IsSelected <- reactive({
    if(input$SO2_AQ == FALSE){
      SO2_AQ_IsSelected <- FALSE
    }else{
      SO2_AQ_IsSelected <- TRUE
    }
  })
  NO2_AQ_IsSelected <- reactive({
    if(input$NO2_AQ == FALSE){
      NO2_AQ_IsSelected <- FALSE
    }else{
      NO2_AQ_IsSelected <- TRUE
    }
  })
  O3_AQ_IsSelected <- reactive({
    if(input$O3_AQ == FALSE){
      O3_AQ_IsSelected <- FALSE
    }else{
      O3_AQ_IsSelected <- TRUE
    }
  })
  CO_AQ_IsSelected <- reactive({
    if(input$CO_AQ == FALSE){
      CO_AQ_IsSelected <- FALSE
    }else{
      CO_AQ_IsSelected <- TRUE
    }
  })
  BC_AQ_IsSelected <- reactive({
    if(input$BC_AQ == FALSE){
      BC_AQ_IsSelected <- FALSE
    }else{
      BC_AQ_IsSelected <- TRUE
    }
  })
  
  
  
  # Coordinates of every county in illinois
  county_coordinates <- read.table(file= "county_coordinates.csv",sep = ",", header = TRUE)
  
  getForecastData <- function(lat, lon, d, h){
    
    if(d == 7){
      dt <- res7 <- seq(Sys.Date()-7, Sys.Date(), "1 day") %>% 
        map(~get_forecast_for(lon, lat, .x)) %>% 
        map_df("hourly")
    }else if(d == 1){
      dt <- res7 <- seq(Sys.Date()-1, Sys.Date(), "1 day") %>% 
        map(~get_forecast_for(lon, lat, .x)) %>% 
        map_df("hourly")
    }else if(d == 0){
      #dt <- get_current_forecast(lon, lat)$current
      longdata <- get_current_forecast(lon, lat)$hourly
      longdata<- tail(longdata,3)
       dt <- longdata
    }
   
    dt$lat <- lat
    dt$lon <- lon
    #select(dt, 'time', 'temperature', 'humidity', 'pressure', 'windSpeed', 'visibility', 'ozone', 'lat', 'lon')
    select(dt, 'time', 'temperature', 'humidity', 'pressure', 'windSpeed', 'visibility', 'ozone', 'summary','windBearing','cloudCover' , 'lat', 'lon')
    }
  
  
  n_locations <- reactive({
    loc <- read.csv("county_coordinates.csv", header = TRUE)
    loc <- select(loc, 'lon', 'lat')
    loc
  })
  # getCurForecast(41.83107, -87.61730)
  getForecast <- function(d,h){
    loc <- n_locations()
    do.call(rbind, apply(loc, 1, function(z) getForecastData(z[1], z[2], d, h)))
  }
  
  get_forecast <- reactive({
    req(input$TimeFrame)
    autoInvalidate()
    if(input$TimeFrame == "Current"){
      day = 0
      hour = 1

    }else if(input$TimeFrame == "24 Hours"){

      day = 1
      hour = 0

    }else if(input$TimeFrame == "7 Days"){
      day = 7
      hour = 0

    }
    getForecast(day,hour)
  })
  
  
  
  weather_data <- read.csv("weather_data.csv", header = TRUE)
  
  getHeatMapData <- function(d,h, path){
    
    sz<-"200"
    if(d == 7){
      sz <- "20000"
    }
    if(d == 1){
      sz <- "5000"
    }
    if(h == 1){
      sz <- "500"
    }
    u = tryCatch({
      ls.observations(filters=list(project='chicago',sensor=path, size=sz))
    }, 
    error=function(cond) {
      
      print(cond)
      # Choose a return value in case of error
      return(list())
    },
    warning=function(cond) {
      
      
      print(cond)
      # Choose a return value in case of warning
      return(list())
    }
    )
    
    
    path_list = pollutantPaths()
    
    if(length(u) == 0){
      return (u)
    }
    return(select(u,'node_vsn', 'value', 'location.geometry'))
  }
  
  getParamData_ <- function(param){
    req(input$TimeFrame)
    if(input$TimeFrame == "Current"){
      cur_date <- Sys.Date()
      to_date <- Sys.Date() 
      
    }else if(input$TimeFrame == "24 Hours"){
      cur_date <- Sys.Date()
      to_date <- Sys.Date() - days(1)
      
    }else if(input$TimeFrame == "7 Days"){
      cur_date <- Sys.Date()
      to_date <- Sys.Date() - days(7)
      
    }
    result = tryCatch({
      cur_date <- Sys.Date()
      select(aq_measurements(city = "Chicago-Naperville-Joliet", date_from = toString(to_date), date_to = toString(cur_date), parameter = param), 'value', 'latitude', 'longitude')
    }, 
    error=function(cond) {
      
      print(cond)
      # Choose a return value in case of error
      return(list())
    },
    warning=function(cond) {
      
      
      print(cond)
      # Choose a return value in case of warning
      return(list())
    }
    )}
  

  
  output$heatmap <- renderLeaflet({
    req(input$dataType)
    req(input$aotData)
    req(input$TimeFrame)
    req(get_forecast)
    autoInvalidate()
    
    day = 0
    hour = 0
    
    if(input$TimeFrame == "Current"){
      day = 0
      hour = 1
    }else if(input$TimeFrame == "24 Hours"){

      day = 1
      hour = 0
    }else if(input$TimeFrame == "7 Days"){
      day = 7
      hour = 0
    }
    
    
    if(input$dataType == "AOT_HM"){
      path = ""
      if(input$aotData == 'AOT_SO2_HM'){
        path = so2_path
      }else if(input$aotData == 'AOT_H2S_HM'){
        path = h2s_path
      }else if(input$aotData == 'AOT_O3_HM'){
        path = ozone_path
      }else if(input$aotData == 'AOT_NO2_HM'){
        path = no2_path
      }else if(input$aotData == 'AOT_CO_HM'){
        path = co_path
      }else if(input$aotData == 'AOT_PM10_HM'){
        path = pm10_path
      }else if(input$aotData == 'AOT_TEMPERATURE_HM'){
        path = temperature_path
      }else if(input$aotData == 'AOT_HUMIDITY_HM'){
        path = humidity_path
      }else if(input$aotData == 'AOT_LIGHT_INTENSITY_HM'){
        path = intensity_path
      }
      datar <- getHeatMapData(day,hour, path)
      if(length(datar) == 0){
        leaflet(dt) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
          setView( -87.57535, 41.72246, 11 ) 
      }else{
        loc <- select(datar$location.geometry, 'coordinates')
        dt <- loc
        res <- dt %>%
          rowwise %>%
          mutate(Lat = as.numeric(coordinates[1]), Lon = as.numeric(coordinates[2])) %>%
          ungroup %>%
          select(-coordinates)
        data <- cbind(datar, res)
        locations <- select(data, 'value', 'node_vsn', 'Lat', 'Lon')
        
        if(input$aotType == 'MEAN_HM'){
          dt <- locations %>% group_by(node_vsn, Lat, Lon) %>% summarise(value = mean(value))
          
        }
        else if(input$aotType == 'MIN_HM'){
          dt <- locations %>% group_by(node_vsn, Lat, Lon) %>% summarise(value = min(value))
        }
        else if(input$aotType == 'MAX_HM'){
          
          dt <- locations %>% group_by(node_vsn, Lat, Lon) %>% summarise(value = max(value)) 
        }
      
        leaflet(dt) %>% addTiles() %>% addProviderTiles(providers$CartoDB.DarkMatter, group = "Normal Tiles") %>%
          setView( -87.57535, 41.72246, 11 ) %>%
          addHeatmap(lng = ~Lat, lat = ~Lon, intensity = ~value*10,
                     blur = 20, max = 0.05, radius = 15)
      }
      
    }else if(input$dataType == "DARK_SKY_HM"){
      ds <- get_forecast()
      if(input$dsData == 'DS_TEMPERATURE_HM'){

          ds <- select(ds, 'lat', 'lon', 'temperature')
          ds$value <- ds$temperature
      }else if(input$dsData == 'DS_HUMIDITY_HM'){
          ds <- select(ds, 'lat', 'lon', 'humidity')
          ds$value <- ds$humidity
  
      }else if(input$dsData == 'DS_WIND_SPEED_HM'){
          ds <- select(ds, 'lat', 'lon', 'windSpeed')
          ds$value <- ds$windSpeed
 
      }else if(input$dsData == 'DS_CLOUD_COVER_HM'){
          ds <- select(ds, 'lat', 'lon', 'cloudCover')
          ds$value <- ds$cloudCover
        
      }else if(input$dsData == 'DS_VISIBILITY_HM'){
          ds <- select(ds, 'lat', 'lon', 'visibility')
          ds$value <- ds$visibility
        
        
      }else if(input$dsData == 'DS_PRESSURE_HM'){
       
          ds <- select(ds, 'lat', 'lon', 'pressure')
          ds$value <- ds$pressure
        
      }else if(input$dsData == 'DS_OZONE_HM'){
          ds <- select(ds, 'lat', 'lon', 'ozone')
          ds$value <- ds$ozone
        
      }else {
        
        ds <- select(ds, 'lat', 'lon', 'windBearing')
        ds$value <- ds$windBearing
        
      }
     
      if(input$aotType == 'MEAN_HM'){
        dt <- ds %>% group_by(lat, lon) %>% summarise(value = mean(value))
      }
      else if(input$aotType == 'MIN_HM'){
        dt <- ds %>% group_by(lat, lon) %>% summarise(value = min(value))
      }
      else if(input$aotType == 'MAX_HM'){
        
        dt <- ds %>% group_by(lat, lon) %>% summarise(value = max(value)) 
      }

      leaflet(dt) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView( -87.57535, 41.72246, 11 ) %>%
        addHeatmap(lng = ~lat, lat = ~lon, intensity = ~value,
                   blur = 20, max = 0.05, radius = 15)
    }
    else{
      if(input$aqData == "AQ_SO2_HM"){
        dt <- getParamData_("so2")
      }
      if(input$aqData == "AQ_O3_HM"){
        dt <- getParamData_("o3")
      }
      if(input$aqData == "AQ_NO2_HM"){
        dt <- getParamData_("no2")
      }
      if(input$aqData == "AQ_CO_HM"){
        dt <- getParamData_("co")
      }
      if(input$aqData == "AQ_PM25_HM"){

        dt <- getParamData_("pm25")
      }
      if(input$aqData == "AQ_PM10_HM"){
        dt <- getParamData_("pm10")
      }
      if(input$aqData == "AQ_BC_HM"){
        dt <- getParamData_("bc")
      }
      if(input$aotType == 'MEAN_HM'){
        dt <- dt %>% group_by(latitude, longitude) %>% summarise(value = mean(value))
      }
      else if(input$aotType == 'MIN_HM'){
        dt <- dt %>% group_by(latitude, longitude) %>% summarise(value = min(value))
      }
      else if(input$aotType == 'MAX_HM'){
        
        dt <- dt %>% group_by(latitude, longitude) %>% summarise(value = max(value)) 
      }
      leaflet(dt) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView( -87.57535, 41.72246, 9 ) %>%
        addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~value*50,
                   blur = 20,radius = 25)
    }
    
    
  })
  
  #current weather data in Chicago (24 hours) for selected pollutants 
  # weather_data <- getCurForecast(41.870, -87.647)
  
  
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
      sz <- "20000"
    }
    if(d == 1){
      sz <- "9000"
    }
    if(h == 1){
      sz <- "500"
    }
    #url <- "https://api.arrayofthings.org/api/observations?location=chicago&node="
    #url <- paste(url, vsn,"&timestamp=","le:2018-08-01T00:00:00&sensor=", path,"&size=",sz,sep="")
    # s <- download.file(url, "/var/tmp/obs", quiet = FALSE)
    # t = fromJSON("/var/tmp/obs")
    # # u = t$data
    #s <- download.file(url, "obs.html", quiet = FALSE)
    #t = fromJSON("obs.html")
    currentTime = Sys.time();
    gmtTime = as.POSIXlt(currentTime, tz="UTC")
    
    if(d == 7 & h ==0){
      # u1 <- ls.observations(filters=list(project='chicago', sensor=path, node=vsn,size=sz))
      u = tryCatch({
        ls.observations(filters=list(project='chicago', sensor=path, node=vsn,size=sz))
      }, 
      error=function(cond) {
        
        print(cond)
        # Choose a return value in case of error
        return(list())
      },
      warning=function(cond) {
        
        
        print(cond)
        # Choose a return value in case of warning
        return(list())
      }
      )
     
    }
    else if(d==1){
      u = tryCatch({
      ls.observations(filters=list(project='chicago', sensor=path, node=vsn,size=sz, timestamp=toString(strftime(gmtTime - days(1), format="ge:%Y-%m-%dT%H:%M:%S"))))
      }, 
      error=function(cond) {
        
        print(cond)
        # Choose a return value in case of error
        return(list())
      },
      warning=function(cond) {
        
        
        print(cond)
        # Choose a return value in case of warning
        return(list())
      }
      )
      }
    else{
      u = tryCatch({
        ls.observations(filters=list(project='chicago', sensor=path, node=vsn,size=sz, timestamp=toString(strftime(gmtTime - hours(1), format="ge:%Y-%m-%dT%H:%M:%S"))))
      }, 
      error=function(cond) {
        
        print(cond)
        # Choose a return value in case of error
        return(list())
      },
      warning=function(cond) {
        
        
        print(cond)
        # Choose a return value in case of warning
        return(list())
      }
      )
      
      }

    
    if(length(u) == 0){
      return (u)
    }
    return(select(u,'node_vsn', 'sensor_path', 'timestamp', 'value'))
  }
  
  #View(ls.observations(filters=list(project='chicago', sensor="alphasense.opc_n2.pm10", node="072")))
  #########################   first node

  getPollutantData <- function(inp, time, path){
    day = 0
    hour = 0
    
    if(time == "Current"){
      day = 0
      hour = 1
    }else if(time == "24 Hours"){
      
      day = 1
      hour = 0
    }else if(time == "7 Days"){
      day = 7
      hour = 0
    } 
    data <- getData(inp, day, hour, path)
    str(data)
    Timestamp <- data$timestamp
    Value <- data$value
    data <- cbind(Timestamp,Value)
    if(length(data) == 0 ){
      stop(paste("No data avaliavle for node: "),inp)
    }
    else{
      data}
  }
    

  
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
    c = select(subset(theAData, is.element(sensor_path, path_list)), 'node_vsn', 'sensor_path', 'timestamp', 'value')

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
  
  getAllNodeLocations <- function(){
    
    node_addresses <- ls.nodes(filters=list(project='chicago'))
    
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
  
  allNodeLocations <- reactive({
    autoInvalidate()
    getAllNodeLocations()
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
      req(input$units)
      req(input$TimeFrame)
      autoInvalidate()
      day = 0
      hour = 0
      skip = 0
      
      if(input$TimeFrame == "Current"){
        day = 0
        hour = 1
        #skip = 4
      }else if(input$TimeFrame == "24 Hours"){
        day = 1
        hour = 0
        skip = 49
      }else if(input$TimeFrame == "7 Days"){
        day = 7
        hour = 0
        skip = 149
      }

      no2_data <- getData(input$node1Input, day, hour, no2_path)
      ozone_data <- getData(input$node1Input, day, hour, ozone_path)
      co_data <- getData(input$node1Input, day, hour, co_path)
      h2s_data <- getData(input$node1Input, day, hour, h2s_path)
      so2_data <- getData(input$node1Input, day, hour, so2_path)
      pm10_data <- getData(input$node1Input, day, hour, pm10_path)
      pm25_data <- getData(input$node1Input, day, hour, pm25_path)
      temperature_data <- getData(input$node1Input, day, hour, temperature_path)
      humidity_data <- getData(input$node1Input, day, hour, humidity_path)
      intensity_data <- getData(input$node1Input, day, hour, intensity_path)
      
      
      
      if(length(no2_data) == 0 & length(ozone_data)== 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0 &length(humidity_data)== 0 & length(intensity_data)== 0 &length(temperature_data)== 0 ){
        stop(paste("No data avaliavle for node: "),input$node1Input)
      }
      else{
        myplot <- ggplot()
        if(length(no2_data) > 0){
          no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          no2_data<- no2_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        }
        if(length(ozone_data) > 0){
          ozone_data $timestamp <- as.POSIXct(ozone_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          ozone_data<- ozone_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=ozone_data , aes(timestamp, value, group=1, color="Ozone")) 
        }
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          co_data<- co_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          h2s_data<- h2s_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          so2_data<- so2_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          pm10_data<- pm10_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(pm25_data) > 0){
          pm25_data$timestamp <- as.POSIXct(pm25_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          pm25_data<- pm25_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=pm25_data, aes(timestamp, value, group=1, color="PM10"))
        }
        
        
        #Work here 
        if(input$units == "met"){
          if(length(temperature_data) > 0){

            temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
            temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
            myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
          }
        }
        else if(input$units == "imp"){
          if(length(temperature_data) > 0){
           ##Working Convertion
            temperature_data$TempM <- temperature_data[,'value']*9/5+32

            temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
            temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
            myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, TempM, group=1, color="Temperature"))
          }
        }
        
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          humidity_data<- humidity_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          intensity_data<- intensity_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("#B92886", "#0088FF", "#00FFD4", "#FF7171", "#24791D", "#5F4242", "#CF65E6","#48A900", "#47768A", "#000000")) + labs(color='Parameters')
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
      req(input$units)
      req(input$TimeFrame)
      autoInvalidate()
      
      day = 0
      hour = 0
      skip = 0
      
      if(input$TimeFrame == "Current"){
        day = 0
        hour = 1
        #skip = 4
      }else if(input$TimeFrame == "24 Hours"){

        day = 1
        hour = 0
        skip = 49
      }else if(input$TimeFrame == "7 Days"){
        day = 7
        hour = 0
        skip = 149
      }
      
      no2_data <- getData(input$node2Input, day, hour, no2_path)
      ozone_data <- getData(input$node1Input, day, hour, ozone_path)
      co_data <- getData(input$node2Input, day, hour, co_path)
      h2s_data <- getData(input$node2Input, day, hour, h2s_path)
      so2_data <- getData(input$node2Input, day, hour, so2_path)
      pm10_data <- getData(input$node2Input, day, hour, pm10_path)
      pm25_data <- getData(input$node2Input, day, hour, pm25_path)
      temperature_data <- getData(input$node2Input, day, hour, temperature_path)
      humidity_data <- getData(input$node2Input, day, hour, humidity_path)
      intensity_data <- getData(input$node2Input, day, hour, intensity_path)
      
      
      if(length(no2_data) == 0 & length(ozone_data)== 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0 &length(humidity_data)== 0 & length(intensity_data)== 0 &length(temperature_data)== 0 ){
        stop(paste("No data avaliavle for node: "),input$node1Input)
      }
      else{
        myplot <- ggplot()
        if(length(no2_data) > 0){
          no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          no2_data<- no2_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
        }
        if(length(ozone_data) > 0){
          ozone_data $timestamp <- as.POSIXct(ozone_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          ozone_data<- ozone_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=ozone_data , aes(timestamp, value, group=1, color="Ozone")) 
        }
        if(length(co_data) > 0 ){
          co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          co_data<- co_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
        }
        if(length(h2s_data) > 0){
          h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          h2s_data<- h2s_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
        }
        if(length(so2_data) > 0){
          so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          so2_data<- so2_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
        }
        if(length(pm10_data) > 0){
          pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          pm10_data<- pm10_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
        }
        if(length(pm25_data) > 0){
          pm25_data$timestamp <- as.POSIXct(pm25_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          pm25_data<- pm25_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=pm25_data, aes(timestamp, value, group=1, color="PM10"))
        }
        
        if(input$units == "met"){
          if(length(temperature_data) > 0){
            temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
            temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
            myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
          }
        }
        else if(input$units == "imp"){
          if(length(temperature_data) > 0){
            temperature_data$TempM <- temperature_data[,'value']*9/5+32
            temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
            temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
            myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, TempM, group=1, color="Temperature"))
          }
        }
        
        
        if(length(humidity_data) > 0){
          humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          humidity_data<- humidity_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
        }
        if(length(intensity_data) > 0){
          intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
          intensity_data<- intensity_data[c(rep(FALSE,skip),TRUE), ]
          myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
        }
        
        
        myplot <- myplot + geom_point() + scale_colour_manual(values=c("#B92886", "#0088FF", "#00FFD4", "#FF7171", "#24791D", "#5F4242", "#CF65E6","#48A900", "#47768A", "#000000")) + labs(color='Parameters')
        myplot
      }

      
    })
    

    getOpenAqData <- function(d,h){
      req(PM25_AQ_IsSelected)
      req(PM10_AQ_IsSelected)
      req(SO2_AQ_IsSelected)
      req(NO2_AQ_IsSelected)
      req(O3_AQ_IsSelected)
      req(CO_AQ_IsSelected)
      req(BC_AQ_IsSelected)
      
      poll_list = list()
      
      if(PM25_AQ_IsSelected()){
        poll_list = c(poll_list, list("pm25"))
      }
      if(PM10_AQ_IsSelected()){
        poll_list = c(poll_list, list("pm10"))
      }
      if(SO2_AQ_IsSelected()){
        poll_list = c(poll_list, list("so2"))
      }
      if(NO2_AQ_IsSelected()){
        poll_list = c(poll_list, list("no2"))
      }
      if(O3_AQ_IsSelected()){
        poll_list = c(poll_list, list("o3"))
      }
      if(CO_AQ_IsSelected()){
        poll_list = c(poll_list, list("co"))
      }
      if(BC_AQ_IsSelected()){
        poll_list = c(poll_list, list("bc"))
      }
      
      res2 <- aq_latest(country = "US", city = "Chicago-Naperville-Joliet")
      dta <- unique(select(subset(res2, is.element(parameter, poll_list)),'latitude', 'longitude', 'location'))
      dta
    }
    
    getOpenAqData_ <- function(d,h, loc){
      req(PM25_AQ_IsSelected)
      req(PM10_AQ_IsSelected)
      req(SO2_AQ_IsSelected)
      req(NO2_AQ_IsSelected)
      req(O3_AQ_IsSelected)
      req(CO_AQ_IsSelected)
      req(BC_AQ_IsSelected)
      req(input$TimeFrame)
      
      if(input$TimeFrame == "Current"){
        cur_date <- Sys.Date()
        to_date <- Sys.Date() -days(1)
        
      }else if(input$TimeFrame == "24 Hours"){

        cur_date <- Sys.Date()
        to_date <- Sys.Date() - days(1)
        
      }else if(input$TimeFrame == "7 Days"){
        cur_date <- Sys.Date()
        to_date <- Sys.Date() - days(7)
        
      }
      result = tryCatch({
        res2 <- aq_measurements(country="US", city="Chicago-Naperville-Joliet", date_from = toString(to_date), date_to = toString(cur_date), location=loc)
        
        if(input$TimeFrame == "Current"){
          res2 <- tail(res2, 4)
          
        }
        
        poll_list = list()
        if(!PM25_AQ_IsSelected()){
          res2 <- subset(res2, parameter != 'pm25')
        }
        if(!PM10_AQ_IsSelected()){
          res2 <- subset(res2, parameter != 'pm10')
        }
        if(!SO2_AQ_IsSelected()){
          res2 <- subset(res2, parameter != 'so2')
        }
        if(NO2_AQ_IsSelected()){
          res2 <- subset(res2, parameter != 'no2')
        }
        if(!O3_AQ_IsSelected()){
          res2 <- subset(res2, parameter != 'o3')
        }
        if(!CO_AQ_IsSelected()){
          res2 <- subset(res2, parameter != 'co')
        }
        if(!BC_AQ_IsSelected()){
          res2 <- subset(res2, parameter != 'bc')
        }
        
        
        res2
      }, 
      error=function(cond) {
        
        print(cond)
        # Choose a return value in case of error
        return(list())
      },
      warning=function(cond) {
        
        
        print(cond)
        # Choose a return value in case of warning
        return(list())
      }
      )
      
      
      
    }
    
    getDSData_ <- function(d,h, lon, lat){
      req(TEMPERATURE_DS_IsSelected)
      req(HUMIDITY_DS_IsSelected )
      req(WINDSPEED_IsSelected)
      req(WINDBEARING_IsSelected)
      req(CLOUDCOVER_IsSelected)
      req(VISIBILITY_IsSelected)
      req(PRESSURE_IsSelected)
      req(OZONE_DS_IsSelected)
      req(SUMMARY_IsSelected)
      req(input$units)
      res2 <- getForecastData(lon, lat, d, h)
      
      res3<- res2
      
      if(!TEMPERATURE_DS_IsSelected()){
        res2 <- select(res2,-'temperature')
      }
      else{
        if(input$units == 'met'){
          res2$temperature <- (res2$temperature-32) *5/9
        }else{
          
        }
      }
      if(!HUMIDITY_DS_IsSelected()){
        res2 <- select(res2,-'humidity')
      }
      if(!WINDSPEED_IsSelected()){
        res2 <- select(res2,-'windSpeed')
      }
      if(!WINDBEARING_IsSelected()){
        res2 <- select(res2,-'windBearing')
      }
      if(!CLOUDCOVER_IsSelected()){
        res2 <- select(res2,-'cloudCover')
      }
      if(!VISIBILITY_IsSelected()){
        res2 <- select(res2,-'visibility')
      }
      if(!PRESSURE_IsSelected()){
        res2 <- select(res2,-'pressure')
      }
      if(!OZONE_DS_IsSelected()){
        res2 <- select(res2,-'ozone')
      }
      if(!SUMMARY_IsSelected()){
        res2 <- select(res2,-'summary')
      }
      
      
      res2
    }
    
    
  
  
    #res <- get_current_forecast(41.870, -87.647)
   # res2 <- aq_latest(country = "US", city = "Chicago-Naperville-Joliet")

    
    
   
   output$mymap <- renderLeaflet({
     #req(input$Maps)
     req(pollutantPaths)
     autoInvalidate()
  
     ds <- nodeLocations()  #displays only the current nodes with information (last 1 hour)
 
     aq <- getOpenAqData() 
   
     both <- merge(ds, aq) 
     
     
     icons <- awesomeIcons(
       icon = 'ios-close',
       iconColor = 'black',
       library = 'ion',
       markerColor = "green"
     )
     
     Icon <- makeIcon(
       iconUrl = "https://b.kisscc0.com/20180705/qoq/kisscc0-google-maps-pin-google-map-maker-computer-icons-map-pin-2-5b3dc69162bb64.0320443815307751854044.png",
       iconWidth = 23, iconHeight = 38,

     )
     
   
     output$title2 <- renderText({})
     output$node_data <- renderPlot({})
     output$node_DS_data <- renderPlot({})
     output$node_DS_table_data <- DT::renderDataTable(DT::datatable({ }))
     output$node_table_data <- DT::renderDataTable(DT::datatable({ }))
     
     output$node_AQ_table_data <- DT::renderDataTable({ })
     output$node_AQ_data <- renderPlot({})
     
    leaflet(both) %>%
       addTiles() %>%  # Add default OpenStreetMap map tiles
       addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
       addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
       addProviderTiles(providers$Hydda, group = "Hydda Maptilte") %>%    #Change this to change the different map background types
       
       addMarkers(~Lat, ~Lon, popup = ~as.character(address), label = ~as.character(vsn), layerId = ~vsn)%>%
       addMarkers(~longitude, ~latitude, popup = ~as.character(location), icon = Icon, layerId = ~location, label = ~as.character(location))%>%
       addLegend("bottomright", colors= c("#0999E6","#ff0000"), labels=c("Aot", "Open_AQ"), title="Node Type")%>%
       
       addLayersControl(position = "bottomleft", baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile", "Hydda Maptilte"), options = layersControlOptions(collapsed = FALSE))
    
 
      ## Clears data on autoinvalidate 
    
    })
 
   getParamData <- function(loc, param){
     req(input$TimeFrame)
     
     if(input$TimeFrame == "Current"){
       cur_date <- Sys.Date()
       to_date <- Sys.Date()-days(1)
       
     }else if(input$TimeFrame == "24 Hours"){
       cur_date <- Sys.Date()
       to_date <- Sys.Date() - days(1)
       
     }else if(input$TimeFrame == "7 Days"){
       cur_date <- Sys.Date()
       to_date <- Sys.Date() - days(7)
       
     }
     
     result = tryCatch({
       dt <- aq_measurements(country="US", city="Chicago-Naperville-Joliet",date_from = toString(to_date), date_to = toString(cur_date), location=loc, parameter = param)
       if(input$TimeFrame == "Current"){
         dt <- tail(dt, 4)

       }
       dt
     }, 
     error=function(cond) {
       
       print(cond)
       # Choose a return value in case of error
       return(list())
     },
     warning=function(cond) {
       
       
       print(cond)
       # Choose a return value in case of warning
       return(list())
     }
     )
   }
   
   location_list <- list( "Naperville", "CHI_SP" , "Kingery Near-road #1", "Valparaiso", "ALSIP", "BRAIDWD", "CARY", "CHIWAUKEE", "CHI_COM", "CHI_SWFP", "CHI_TAFT", "CICERO", "DISPLNS", "ELGIN", "NORTHBRK", "SCHILPRK", "EVANSTON", "LISLE","Ogden Dunes", "Gary-IITRI", "East Chicago Post Of", "Hammond-141st St" , "LEMONT" ,  "ZION" )
  
   
   
   
   output$titleNode1 <- renderText({
     p <-req(input$node1Input)
     nameN<-toString(p)
     name2 <- paste("NODE CHOICE 1, NODE ", nameN,sep = " ")
     name2
   })
   output$titleNode1T <- renderText({
     p <-req(input$node1Input)
     nameN<-toString(p)
     name2 <- paste("NODE CHOICE 1, NODE ", nameN,sep = " ")
     name2
   })
   output$titleNode2 <- renderText({
     p <-req(input$node2Input)
     nameN<-toString(p)
     name2 <- paste("NODE CHOICE 2, NODE ", nameN,sep = " ")
     name2
   })
   output$titleNode2T <- renderText({
     p <-req(input$node2Input)
     nameN<-toString(p)
     name2 <- paste("NODE CHOICE 2, NODE ", nameN,sep = " ")
     name2
   })
   
   
   
   ##### Marker clicked ####
    observeEvent(input$mymap_marker_click, { 
      autoInvalidate()
     p <- input$mymap_marker_click
     
    
     
     output$title2 <- renderText({
       dt_loc <- nodeLocations()
       dt_loc <- subset(dt_loc, vsn ==p$id)
       if(is.element(p$id, location_list )){
         my_address <- "No Address Provided"
       }
       else {my_address <- toString(dt_loc$address)}
       nameN<-toString(p$id)
       nameN2<- paste("<font size='12' color=\"#000000\"><b>", "NODE ID:","<font size='12' color=\"#4286f4\"><b>", nameN, "<font size='12' color=\"#000000\"><b>", "NODE ADDRESS:", "<font size='12' color=\"#4286f4\"><b>",my_address, sep = " ")
       nameN2
     })
     
     if(is.element(p$id, location_list ) ){
       output$node_data <- renderPlot({})
       output$node_DS_data <- renderPlot({})
       output$node_DS_table_data <- DT::renderDataTable(DT::datatable({ }))
       output$node_table_data <- DT::renderDataTable(DT::datatable({ }))
       
       output$node_AQ_table_data <- DT::renderDataTable({ 
         DT::datatable({ 
           dt <- getOpenAqData_(0,1,p$id)
           if(length(dt) > 0){
             select(getOpenAqData_(0,1,p$id), 'parameter', 'value', 'dateLocal')
           }
           
         },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE ))
         
       })
       # PM25, PM10, SO2, NO2, O3, CO and BC
       output$node_AQ_data <- renderPlot({
         req(PM25_AQ_IsSelected)
         req(PM10_AQ_IsSelected)
         req(SO2_AQ_IsSelected)
         req(NO2_AQ_IsSelected)
         req(O3_AQ_IsSelected)
         req(CO_AQ_IsSelected)
         req(BC_AQ_IsSelected)

         if(PM25_AQ_IsSelected() == TRUE){
           pm25_data_ <- getParamData(p$id, "pm25")
         }else{
           pm25_data_ = list()
         }
         if(PM10_AQ_IsSelected() == TRUE){
           pm10_data_ <- getParamData(p$id, "pm10")
         }
         else{
           pm10_data_ = list()
         }
         if(SO2_AQ_IsSelected()){
           so2_data_ <- getParamData(p$id, "so2")
         }
         else{
           so2_data_ = list()
         }
         if(NO2_AQ_IsSelected()){
           no2_data_ <- getParamData(p$id, "no2")
         }
         else{
           no2_data_ = list()
         }
         if(O3_AQ_IsSelected()){
           o3_data_ <- getParamData(p$id, "o3")
         }
         else{
           o3_data_ = list()
         }
         if(CO_AQ_IsSelected()){
           co_data_ <- getParamData(p$id, "co")
         }
         else{
           co_data_ = list()
         }
         if(BC_AQ_IsSelected()){
           bc_data_ <- getParamData(p$id, "bc")
         }
         else{
           bc_data_ = list()
         }
         
         # & length(co_data_) == 0 & length(o3_data_) ==0 & length(no2_data_) == 0 & length(so2_data_) == 0 & length(pm10_data_) == 0 & length(pm25_data_) == 0
         if(length(bc_data_) == 0 & length(co_data_) == 0 & length(o3_data_) ==0 & length(no2_data_) == 0 & length(so2_data_) == 0 & length(pm10_data_) == 0 & length(pm25_data_) == 0){
           ggplot()
         }
         else{
         myplot <- ggplot()
         if(length(pm25_data_) != 0){
           pm25_data_ <- select(pm25_data_, 'value', 'dateUTC')
           myplot <- myplot +
             geom_line(data=pm25_data_ , aes(dateUTC, value, group=1, color="pm25"))
         }
         if(length(pm10_data_) != 0){
           pm10_data_ <- select(pm10_data_, 'value', 'dateUTC')
           myplot <- myplot +
             geom_line(data=pm10_data_ , aes(dateUTC, value, group=1, color="pm10"))
         }
         if(length(so2_data_) != 0){
           so2_data_ <- select(so2_data_, 'value', 'dateUTC')
           myplot <- myplot +
             geom_line(data=so2_data_ , aes(dateUTC, value, group=1, color="so2"))
         }
         if(length(no2_data_) != 0){
           no2_data_ <- select(no2_data_, 'value', 'dateUTC')
           myplot <- myplot +
             geom_line(data=no2_data_ , aes(dateUTC, value, group=1, color="no2"))
         }
         if(length(o3_data_) != 0){
           o3_data_ <- select(o3_data_, 'value', 'dateUTC')
           myplot <- myplot +
             geom_line(data=o3_data_ , aes(dateUTC, value, group=1, color="o3"))
         }
         if(length(co_data_) != 0){
           co_data_ <- select(co_data_, 'value', 'dateUTC')
           myplot <- myplot +
             geom_line(data=co_data_ , aes(dateUTC, value, group=1, color="co"))
         }
         if(length(bc_data_) != 0){
           bc_data_ <- select(bc_data_, 'value', 'dateUTC')
           myplot <- myplot +
             geom_line(data=bc_data_ , aes(dateUTC, value, group=1, color="bc"))
         }
         
         myplot <- myplot + geom_point() + scale_colour_manual(values=c("#B92886", "#0088FF", "#00FFD4", "#FF7171", "#24791D", "#5F4242", "#CF65E6")) + labs(color='Parameters')
         myplot
      }
       })
     }else{
       output$node_AQ_table_data <- DT::renderDataTable({ })
       output$node_AQ_data <- renderPlot({})
 
       output$node_DS_data <- renderPlot({
         req(TEMPERATURE_DS_IsSelected)
         req(HUMIDITY_DS_IsSelected )
         req(WINDSPEED_IsSelected)
         req(WINDBEARING_IsSelected)
         req(CLOUDCOVER_IsSelected)
         req(VISIBILITY_IsSelected)
         req(PRESSURE_IsSelected)
         req(OZONE_DS_IsSelected)
         req(SUMMARY_IsSelected)
         req(input$TimeFrame)
         
         
         day = 0
         hour = 0
         
         if(input$TimeFrame == "Current"){
           day = 0
           hour = 1
         }else if(input$TimeFrame == "24 Hours"){
           day = 1
           hour = 0
           
         }else if(input$TimeFrame == "7 Days"){
           day = 7
           hour = 0
         }
         tableDS<- getDSData_(day, hour, p$lng, p$lat)
         #view(tableDS)
         myplot <- ggplot()
         
         if(TEMPERATURE_DS_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$temperature, group=1, color="temperature"))+geom_point()
         }
         
         
         if(HUMIDITY_DS_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$humidity, group=1, color="humidity"))+geom_point()
         }
         if(WINDSPEED_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$windSpeed, group=1, color="wind speed"))+geom_point()
         }
         if(WINDBEARING_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$windBearing, group=1, color="wind bearing"))+geom_point()
         }
         if(CLOUDCOVER_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$cloudCover, group=1, color="cloud cover"))+geom_point()
         }
         if(VISIBILITY_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$visibility, group=1, color="visibility"))+geom_point()
         }
         if(PRESSURE_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$pressure, group=1, color="pressure"))+geom_point()
         }
         if(OZONE_DS_IsSelected()){
           myplot <- myplot +
             geom_line(data=tableDS , aes(x=tableDS$time, y=tableDS$ozone, group=1, color="ozone"))+geom_point()
         }
         myplot <- myplot + geom_point() + scale_colour_manual(values=c("#B92886", "#0088FF", "#00FFD4", "#FF7171", "#24791D", "#5F4242", "#CF65E6","#48A900")) + labs(color='Parameters') + xlab("timestamp") + ylab("value")
         myplot
         
       })
       
       output$node_DS_table_data <- DT::renderDataTable({ 
         DT::datatable({ 
           req(input$TimeFrame)
           req(input$units)
           day = 0
           hour = 0
           
           if(input$TimeFrame == "Current"){
             day = 0
             hour = 1
           }else if(input$TimeFrame == "24 Hours"){
             day = 1
             hour = 0
             
           }else if(input$TimeFrame == "7 Days"){
             day = 7
             hour = 0
           }
           res2<-getDSData_(day, hour, p$lng, p$lat)
           res2 <- select(res2,-'lat')
           res2 <- select(res2,-'lon')

         },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE ))
         
       })
       
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
       req(input$units)
       req(input$TimeFrame)
       
       day = 0
       hour = 0
       skip = 0
       
       if(input$TimeFrame == "Current"){
         day = 0
         hour = 1
       }else if(input$TimeFrame == "24 Hours"){
         day = 1
         hour = 0
         skip = 49
       }else if(input$TimeFrame == "7 Days"){
         day = 7
         hour = 0
         skip = 149
       }
       
       
       no2_data <- getData(p$id, day,hour, no2_path)
       ozone_data <- getData(p$id, day,hour, ozone_path)
       co_data <- getData(p$id, day,hour, co_path)
       h2s_data <- getData(p$id, day,hour, h2s_path)
       so2_data <- getData(p$id, day,hour, so2_path)
       pm10_data <- getData(p$id, day,hour, pm10_path)
       pm25_data <- getData(p$id, day,hour, pm25_path)
       temperature_data <- getData(p$id, day,hour, temperature_path)
       humidity_data <- getData(p$id, day,hour, humidity_path)
       intensity_data <- getData(p$id, day,hour, intensity_path)
       
       if(length(no2_data) == 0 & length(ozone_data)== 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0 &length(humidity_data)== 0 & length(intensity_data)== 0 &length(temperature_data)== 0 ){
         stop(paste("No data avaliavle for node: "),input$node1Input)
       }
       else{
         if(length(no2_data) > 0){
           no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           no2_data<- no2_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- ggplot() + geom_line(data=no2_data , aes(timestamp, value, group=1, color="NO2")) 
         }
         if(length(ozone_data) > 0){
           ozone_data $timestamp <- as.POSIXct(ozone_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           ozone_data<- ozone_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- ggplot() + geom_line(data=ozone_data , aes(timestamp, value, group=1, color="NO2")) 
         }
         if(length(co_data) > 0 ){
           co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           co_data<- co_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- myplot + geom_line(data=co_data, aes(timestamp, value, group=1, color="CO"))
         }
         if(length(h2s_data) > 0){
           h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           h2s_data<- h2s_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- myplot + geom_line(data=h2s_data, aes(timestamp, value, group=1, color="H2S"))
         }
         if(length(so2_data) > 0){
           so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           so2_data<- so2_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- myplot + geom_line(data=so2_data, aes(timestamp, value, group=1, color="SO2"))
         }
         if(length(pm10_data) > 0){
           pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           pm10_data<- pm10_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- myplot + geom_line(data=pm10_data, aes(timestamp, value, group=1, color="PM10"))
         }
         if(length(pm25_data) > 0){
           pm25_data$timestamp <- as.POSIXct(pm25_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           pm25_data<- pm25_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- myplot + geom_line(data=pm25_data, aes(timestamp, value, group=1, color="PM10"))
         }
         
         if(input$units == "met"){
           if(length(temperature_data) > 0){
             temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
             myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, value, group=1, color="Temperature"))
           }
         }
         else if(input$units == "imp"){
           if(length(temperature_data) > 0){
             temperature_data$TempM <- temperature_data[,'value']*9/5+32
             temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
             myplot <- myplot + geom_line(data=temperature_data, aes(timestamp, TempM, group=1, color="Temperature"))
           }
         }
         
         
         if(length(humidity_data) > 0){
           humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           humidity_data<- humidity_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- myplot + geom_line(data=humidity_data, aes(timestamp, value, group=1, color="Humidity"))
         }
         if(length(intensity_data) > 0){
           intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
           intensity_data<- intensity_data[c(rep(FALSE,skip),TRUE), ]
           myplot <- myplot + geom_line(data=intensity_data, aes(timestamp, value, group=1, color="Intensity"))
         }
         
         
         myplot <- myplot + geom_point() + scale_colour_manual(values=c("#B92886", "#0088FF", "#00FFD4", "#FF7171", "#24791D", "#5F4242", "#CF65E6","#48A900", "#47768A", "#000000")) + labs(color='Parameters')
         myplot
       }
       
     })
     
    
     
     ############################################################### AOT TABLE #########################
     output$node_table_data <- DT::renderDataTable(
       DT::datatable({
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
         req(input$units)
         req(input$TimeFrame)
         
         day = 0
         hour = 0
         skip = 0
         
         if(input$TimeFrame == "Current"){
           day = 0
           hour = 1
         }else if(input$TimeFrame == "24 Hours"){
           day = 1
           hour = 0
           skip = 49
         }else if(input$TimeFrame == "7 Days"){
           day = 7
           hour = 0
           skip = 149
         }
         
         no2_data <- getData(p$id, day,hour, no2_path)
         ozone_data <- getData(p$id, day,hour, ozone_path)
         co_data <- getData(p$id, day,hour, co_path)
         h2s_data <- getData(p$id, day,hour, h2s_path)
         so2_data <- getData(p$id, day,hour, so2_path)
         pm10_data <- getData(p$id, day,hour, pm10_path)
         pm25_data <- getData(p$id, day,hour, pm25_path)
         temperature_data <- getData(p$id, day,hour, temperature_path)
         humidity_data <- getData(p$id, day,hour, humidity_path)
         intensity_data <- getData(p$id, day,hour, intensity_path)
         
         if(length(no2_data) == 0 & length(ozone_data)== 0 & length(co_data)== 0 & length(h2s_data)== 0 & length(so2_data)== 0 & length(pm10_data)== 0 & length(pm25_data)== 0 &length(humidity_data)== 0 & length(intensity_data)== 0 &length(temperature_data)== 0 ){
           stop(paste("No data avaliavle for node: "),input$node1Input)
         }
         else{
           if(length(no2_data) > 0){
             no2_data $timestamp <- as.POSIXct(no2_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             no2_data <- no2_data[c(rep(FALSE,skip),TRUE), ]
           }
           if(length(ozone_data) > 0){
             ozone_data$timestamp <- as.POSIXct(ozone_data $timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
              ozone_data<- ozone_data[c(rep(FALSE,skip),TRUE), ]
           }
           if(length(co_data) > 0 ){
             co_data$timestamp <- as.POSIXct(co_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             co_data <- co_data[c(rep(FALSE,skip),TRUE), ]
           }
           if(length(h2s_data) > 0){
             h2s_data$timestamp <- as.POSIXct(h2s_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             h2s_data<- h2s_data[c(rep(FALSE,skip),TRUE), ]
           }
           if(length(so2_data) > 0){
             so2_data$timestamp <- as.POSIXct(so2_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             so2_data<- so2_data[c(rep(FALSE,skip),TRUE), ]
           }
           if(length(pm10_data) > 0){
             pm10_data$timestamp <- as.POSIXct(pm10_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             pm10_data<- pm10_data[c(rep(FALSE,skip),TRUE), ]
           }
           if(length(pm25_data) > 0){
             pm25_data$timestamp <- as.POSIXct(pm25_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             pm25_data<- pm25_data[c(rep(FALSE,skip),TRUE), ]
           }
           
           if(input$units == "met"){
             if(length(temperature_data) > 0){
               temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
               temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
             }
           }
           else if(input$units == "imp"){
             if(length(temperature_data) > 0){
               temperature_data$TempM <- temperature_data[,'value']*9/5+32
               temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
               temperature_data<- temperature_data[c(rep(FALSE,skip),TRUE), ]
             }
           }
           
           
           if(length(humidity_data) > 0){
             humidity_data$timestamp <- as.POSIXct(humidity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             humidity_data<- humidity_data[c(rep(FALSE,skip),TRUE), ]
           }
           if(length(intensity_data) > 0){
             intensity_data$timestamp <- as.POSIXct(intensity_data$timestamp, tz="UTC", "%Y-%m-%dT%H:%M")
             intensity_data<- intensity_data[c(rep(FALSE,skip),TRUE), ]
           }
           
           
           mytable <- rbind(no2_data,ozone_data,co_data,h2s_data,so2_data,pm10_data,pm25_data,temperature_data,humidity_data,intensity_data)
           mytable
         }
         
         
       },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
     
     
     
   }
     # output$node_AQ_table_data <- DT::renderDataTable(
     #   DT::datatable({
     #     
     #   }))

   })
  
   
   ###### COMPLETED CODE DONT TOUCH######
   
   ##### Data tables for node comparisons #######
   
   output$NO2_1 <-  DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, no2_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
   
   output$OZONE_1 <- DT::renderDataTable( 
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, ozone_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$CO_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, co_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$H2S_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, h2s_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$SO2_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, so2_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$PM10_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, pm10_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$PM25_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, pm25_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   
   
   output$TEMPERATURE_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, temperature_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
   
   output$HUMIDITY_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input) 
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, humidity_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
   output$INTENSITY_1 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node1Input) 
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, intensity_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
   
   ########################### second node
   
   output$NO2_2 <-  DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, no2_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
   
   output$OZONE_2 <- DT::renderDataTable( 
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, ozone_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$CO_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, co_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$H2S_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, h2s_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$SO2_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, so2_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$PM10_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, pm10_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   output$PM25_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, pm25_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE )))
   
   
   
   output$TEMPERATURE_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input)
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, temperature_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
   
   output$HUMIDITY_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input) 
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, humidity_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
   output$INTENSITY_2 <- DT::renderDataTable(
     DT::datatable({ 
       req(input$node2Input) 
       req(input$TimeFrame)
       getPollutantData(input$node1Input, input$TimeFrame, intensity_path)
     },options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, width = 200 )))
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

