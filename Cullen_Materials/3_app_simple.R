

## Barebones Shiny web application to demonstrate Leaflet maps ##

## Only plots Leaflet map; no inputs or reactivity
## Created 2022-03-23 by Josh Cullen (joshcullen10@gmail.com)


library(tidyverse)
library(raster)
library(sf)
library(leaflet)
library(leafem)
library(viridis)
library(htmltools)
library(shiny)

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
                leafletOutput("mymap", width="100%", height="850px")
              )  #close fluidPage



### Server ###

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
  leaflet() %>% 
    addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap",
                     options = tileOptions(zIndex = -10)) %>%
    addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery",
                     options = tileOptions(zIndex = -10)) %>%
    addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map",
                     options = tileOptions(zIndex = -10)) %>%
    addRasterImage(x = sst.rast[[2]],
                   colors = rast.pal2,
                   opacity = 1,
                   group = "Feb SST") %>%
    addImageQuery(sst.rast[[2]], group = "Feb SST") %>%  #add raster  query
    addRasterImage(x = sst.rast[[8]],
                   colors = rast.pal2,
                   opacity = 1,
                   group = "Aug SST") %>%
    addImageQuery(sst.rast[[8]], group = "Aug SST") %>%  #add raster  query
    addLegend_decreasing(pal = rast.pal2,
                         values = as.vector(values(sst.rast)),
                         title = "SST (\u00B0C)",
                         decreasing = TRUE) %>%
    addPolygons(data = wind,
                color = ~poly.pal(State),
                fillOpacity = 1,
                stroke = FALSE,
                label = ~paste0("State: ", State),
                group = "Offshore Wind Leases") %>%
    addLegend(pal = poly.pal,
              values = wind$State,
              title = "State",
              opacity = 1) %>%
    addPolylines(data = tracks.sf2,
                 color = ~tracks.pal(id),
                 opacity = 0.75,
                 weight = 2,
                 label = ~paste0("ID: ", id),
                 group = "Tracks") %>%
    addLegend(pal = tracks.pal,
              values = tracks.sf2$id,
              title = "ID",
              position = "topleft") %>%
    addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                     overlayGroups = c("Feb SST", "Aug SST", "Offshore Wind Leases", "Tracks"),
                     options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE),
                     position = "bottomleft") %>%
    addScaleBar(position = "bottomright") %>%
    addMeasure(position = "topleft",
               primaryLengthUnit = "kilometers",
               primaryAreaUnit = "hectares",
               activeColor = "#3D535D",
               completedColor = "#7D4479") %>%
    addMouseCoordinates()
  
  })  #close renderLeaflet
  
}  #close server function






### Run the application ###
shinyApp(ui = ui, server = server)
