

## Barebones Shiny web application to demonstrate Leaflet maps ##

## Plots Leaflet map w/ inputs and reactivity
## Created 2022-03-23 by Josh Cullen (joshcullen10@gmail.com)

library(tidyverse)
library(raster)
library(sf)
library(leaflet)
library(leafem)
library(viridis)
library(shiny)
library(shinyWidgets)

source("helper functions.R")  #for function addLegend_decreasing



### Load data ###

# Simulated tracks
tracks <- read.csv("Simulated tracks.csv")
tracks.sf <- tracks %>% 
  st_as_sf(., coords = c('x','y'), crs = 4326)
tracks.sf2 <- tracks.sf %>% 
  group_by(id) %>% 
  summarize(do_union = FALSE) %>% 
  st_cast("MULTILINESTRING")

# Monthly SST (2021)
sst <- read.csv("Monthly_SST_2021.csv")
sst.rast <- sst.rast2 <- sst %>% 
  group_split(month) %>% 
  purrr::map(., ~rasterFromXYZ(.[,c('x','y','sst')], crs = 4326)) %>% 
  raster::brick()

# Offshore wind leases
wind <- st_read("NE_Offshore_Wind.shp")
wind$State <- gsub(pattern = "Massachussets", "Massachusetts", wind$State)  #fix typo


# Define color palettes
tracks.pal <- colorFactor("Dark2", factor(tracks$id))
poly.pal <- colorFactor("Set3", factor(wind$State))

sst.range <- range(as.vector(values(sst.rast)), na.rm = TRUE)
rast.pal2 <- colorNumeric(palette = viridis::viridis(100, option = 'magma'),
                          domain = sst.range,
                          na.color = "transparent")



### UI ###

ui <- fluidPage(title = "Animal Movement, Offshore Wind Development, and SST",
                
                leafletOutput("mymap", width = "100%", height = "850px"),
                
                absolutePanel(class = "panel panel-default",
                              top = 300,
                              left = 25,
                              width = 250,
                              fixed = TRUE,
                              draggable = TRUE,
                              height = "auto",

                              h3("Choose which layers to map"),
                              pickerInput(inputId = "tracks",
                                          label = "Select tracks",
                                          choices = unique(tracks$id),
                                          selected = unique(tracks$id),
                                          multiple = TRUE),
                              pickerInput(inputId = "polygons",
                                          label = "Select polygons by state",
                                          choices = unique(wind$State),
                                          selected = unique(wind$State),
                                          multiple = TRUE),
                              selectInput(inputId = "raster",
                                          label = "Select month of SST",
                                          choices = month.name,
                                          selected = month.name[1])

                )  #close absolutePanel
                
)  #close fluidPage






### Server ###

server <- function(input, output, session) {
  
  #Create reactive objects based on selected input
  tracks.out <- reactive({
    tracks.sf2 %>% 
      filter(id %in% input$tracks)
  })
  
  wind.out <- reactive({
    wind %>% 
      filter(State %in% input$polygons)
  })
  
  sst.out <- reactive({
    sst.rast[[which(month.name == input$raster)]]
  })
  
  
  
  
  
  output$mymap <- renderLeaflet({
    
    
    ## Static Leaflet basemap and widgets
    leaflet() %>% 
      setView(lng = -73, lat = 41.5, zoom = 6) %>% 
      addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery") %>%
      addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map") %>%
      addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                       overlayGroups = c("SST", "Offshore Wind Leases", "Tracks"),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE),
                       position = "bottomleft") %>%
      addScaleBar(position = "bottomright") %>%
      addMeasure(position = "topleft",
                 primaryLengthUnit = "kilometers",
                 primaryAreaUnit = "hectares",
                 activeColor = "#3D535D",
                 completedColor = "#7D4479") %>%
      addMouseCoordinates()
    
  })  #close renderLeaflet
  
  
  
  ## Add reactive elements to Leaflet map
  observe({
    
    leafletProxy(mapId = "mymap") %>% 
      clearMarkers() %>% 
      clearShapes() %>% 
      clearImages() %>% 
      clearControls() %>% 
      addRasterImage(x = sst.out(),
                     colors = rast.pal2,
                     opacity = 1,
                     group = "SST") %>%
      addImageQuery(sst.out(), group = "SST") %>%  #add raster  query
      addLegend_decreasing(pal = rast.pal2,
                           values = as.vector(values(sst.rast)),
                           title = "SST (\u00B0C)",
                           decreasing = TRUE) %>%
      addPolygons(data = wind.out(),
                  color = ~poly.pal(State),
                  fillOpacity = 1,
                  stroke = FALSE,
                  label = ~paste0("State: ", State),
                  group = "Offshore Wind Leases") %>%
      addLegend(pal = poly.pal,
                values = wind.out()$State,
                title = "State",
                opacity = 1) %>%
      addPolylines(data = tracks.out(),
                   color = ~tracks.pal(id),
                   opacity = 0.75,
                   weight = 2,
                   label = ~paste0("ID: ", id),
                   group = "Tracks") %>%
      addLegend(pal = tracks.pal,
                values = tracks.out()$id,
                title = "ID",
                position = "topleft")
  
    })  #close observe
  
}  #close server function






### Run the application ###
shinyApp(ui = ui, server = server)
