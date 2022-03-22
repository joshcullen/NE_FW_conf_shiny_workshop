

################################################
### Create simulations using common distribs ###
################################################

library(tidyverse)
library(raster)
library(sf)
library(leaflet)
# library(leafem)
# library(shiny)

library(viridis)
library(htmltools)

source("Cullen_Materials/helper functions.R")



## Load data

# Simulated tracks
tracks <- read.csv("Cullen_Materials/Simulated tracks.csv")
tracks.sf <- tracks %>% 
  st_as_sf(., coords = c('x','y'), crs = 4326)

# Monthly SST (2021)
sst <- read.csv("Cullen_Materials/Monthly_SST_2021.csv")
sst.rast <- sst.rast2 <- sst %>% 
  group_split(month) %>% 
  purrr::map(., ~rasterFromXYZ(.[,c('x','y','sst')], crs = 4326)) %>% 
  raster::brick()

# Offshore wind leases
wind <- st_read("Cullen_Materials/NE_Offshore_Wind.shp")
wind$State <- gsub(pattern = "Massachussets", "Massachusetts", wind$State)  #fix typo






## Example 1: Create basemap

print(providers)

# Ocean Basemap
leaflet(tracks.sf) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addRasterImage(sst.rast[[1]]) %>% 
  addFeatures(radius = 1, opacity = 0.2)

# Satellite Imagery Basemap
leaflet(tracks.sf) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addRasterImage(sst.rast[[1]]) %>% 
  addFeatures(radius = 1, opacity = 0.2)

# OSM Basemap
leaflet(tracks.sf) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addRasterImage(sst.rast[[1]]) %>% 
  addFeatures(radius = 1, opacity = 0.2)




## Example 2: Add spatial features

############
## Points ##
############

# Add points from data.frame object w/ default settings
leaflet(tracks) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addCircleMarkers(lng = ~x,
                   lat = ~y)


# Add points from sf object w/ default settings
leaflet(tracks.sf) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addCircleMarkers()


# Add points from data.frame object w/ customized size and opacity
tracks.pal <- colorFactor("Dark2", factor(tracks$id))

leaflet(tracks) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1,
                   opacity = 0.5)


# Add points from data.frame object w/ customized colors, labels, and legend
leaflet(tracks) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1,
                   opacity = 0.5,
                   color = ~tracks.pal(id),
                   label = ~paste0("ID: ", id)) %>% 
  addLegend(pal = tracks.pal,
            values = ~id,
            title = "ID")


# Add popups instead of labels (need to click instead of hover)
leaflet(tracks) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1,
                   opacity = 0.5,
                   color = ~tracks.pal(id),
                   popup = ~paste0("ID: ", id,
                                   "<br> State: ", state)) %>% 
  addLegend(pal = tracks.pal,
            values = ~id,
            title = "ID")




###########
## Lines ##
###########

# Add lines from data.frame object w/ default settings
tracks.pal2 <- RColorBrewer::brewer.pal(n_distinct(tracks$id), "Dark2")

new.map <- leaflet() %>% 
  addProviderTiles(providers$Esri.OceanBasemap) 

for (i in 1:n_distinct(tracks$id)){
  new.map <- new.map %>%
    addPolylines(data = tracks[tracks$id == unique(tracks$id)[i],],
                 lng = ~x,
                 lat = ~y,
                 color = tracks.pal2[i])
}

new.map


# Add lines from sf object w/ default settings
tracks.sf2 <- tracks.sf %>% 
  group_by(id) %>% 
  summarize(do_union = FALSE) %>% 
  st_cast("MULTILINESTRING")

leaflet(tracks.sf2) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolylines(color = ~tracks.pal(id))


## Add lines from sf object and customize colors, size, and add legend
leaflet(tracks.sf2) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolylines(color = ~tracks.pal(id),
               opacity = 0.75,
               weight = 2,
               label = ~paste0("ID: ", id)) %>% 
  addLegend(pal = tracks.pal,
            values = ~id,
            title = "ID")




##############
## Polygons ##
##############

# Add polygon sf object w/ default settings
leaflet(wind) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons()


# Add polygon sf object and customize settings
poly.pal <- colorFactor("Set3", factor(wind$State))

leaflet(wind) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(color = ~poly.pal(State),
              fillOpacity = 1,
              stroke = FALSE,
              label = ~paste0("State: ", State)) %>% 
  addLegend(pal = poly.pal,
            values = ~State,
            title = "State",
            opacity = 1)




############
## Raster ##
############

# Add raster layer
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.OceanBasemap) %>% 
  addRasterImage(x = sst.rast[[1]])


# Add legend to raster layer
rast.pal <- colorNumeric(palette = viridis::viridis(100, option = 'magma'),
                    domain = values(sst.rast[[1]]),
                    na.color = "transparent")

leaflet() %>% 
  addProviderTiles(provider = providers$Esri.OceanBasemap) %>% 
  addRasterImage(x = sst.rast[[1]],
                 colors = rast.pal,
                 opacity = 0.8) %>% 
  addLegend(pal = rast.pal,
            values = values(sst.rast[[1]]),
            title = "SST (\u00B0C)")


# Reversed legend (and palette) for standard ordering
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.OceanBasemap) %>% 
  addRasterImage(x = sst.rast[[1]],
                 colors = rast.pal,
                 opacity = 0.8) %>% 
  addLegend_decreasing(pal = rast.pal,
                       values = values(sst.rast[[1]]),
                       title = "SST (\u00B0C)",
                       decreasing = TRUE)






## Example 3: Add multiple basemaps, scale_bar, other widgets

# Add multiple basemaps w/ widget to switch among them
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
  addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map") %>% 
  addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                   options = layersControlOptions(collapsed = TRUE))


# Add dynamic scale bar
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
  addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map") %>% 
  addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                   options = layersControlOptions(collapsed = TRUE)) %>% 
  addScaleBar(position = "bottomright")


# Add measurement tool (linear and area)
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
  addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map") %>% 
  addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                   options = layersControlOptions(collapsed = TRUE)) %>% 
  addScaleBar(position = "bottomright") %>% 
  addMeasure(position = "topleft",
             primaryLengthUnit = "kilometers",
             primaryAreaUnit = "hectares",
             activeColor = "#3D535D",
             completedColor = "#7D4479")


# Add in raster layer w/ option to turn on/off
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Ocean Basemap",
                   options = tileOptions(zIndex = -10)) %>%
  addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery",
                   options = tileOptions(zIndex = -10)) %>%
  addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map",
                   options = tileOptions(zIndex = -10)) %>% 
  addRasterImage(x = sst.rast[[1]],
                 colors = rast.pal,
                 opacity = 0.8,
                 group = "Jan SST") %>% 
  addLegend_decreasing(pal = rast.pal,
                       values = values(sst.rast[[1]]),
                       title = "SST (\u00B0C)",
                       decreasing = TRUE) %>% 
  addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                   overlayGroups = "Jan SST",
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
  addScaleBar(position = "bottomright") %>% 
  addMeasure(position = "topleft",
             primaryLengthUnit = "kilometers",
             primaryAreaUnit = "hectares",
             activeColor = "#3D535D",
             completedColor = "#7D4479")


# Add multiple raster layers w/ option to turn on/off
sst.range <- range(as.vector(values(sst.rast)), na.rm = TRUE)
rast.pal2 <- colorNumeric(palette = viridis::viridis(100, option = 'magma'),
                          domain = sst.range,
                          na.color = "transparent")

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
  addRasterImage(x = sst.rast[[8]],
                 colors = rast.pal2,
                 opacity = 1,
                 group = "Aug SST") %>% 
  addLegend_decreasing(pal = rast.pal2,
                       values = as.vector(values(sst.rast)),
                       title = "SST (\u00B0C)",
                       decreasing = TRUE) %>% 
  addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                   overlayGroups = c("Feb SST", "Aug SST"),
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
  addScaleBar(position = "bottomright") %>% 
  addMeasure(position = "topleft",
             primaryLengthUnit = "kilometers",
             primaryAreaUnit = "hectares",
             activeColor = "#3D535D",
             completedColor = "#7D4479")


# Add polygons w/ ability to turn on/off
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
  addRasterImage(x = sst.rast[[8]],
                 colors = rast.pal2,
                 opacity = 1,
                 group = "Aug SST") %>% 
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
  addLayersControl(baseGroups = c("Ocean Basemap", "World Imagery", "Open Street Map"),
                   overlayGroups = c("Feb SST", "Aug SST", "Offshore Wind Leases"),
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE),
                   position = "bottomleft") %>% 
  addScaleBar(position = "bottomright") %>% 
  addMeasure(position = "topleft",
             primaryLengthUnit = "kilometers",
             primaryAreaUnit = "hectares",
             activeColor = "#3D535D",
             completedColor = "#7D4479")


# Add lines w/ ability to turn on/off
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
  addRasterImage(x = sst.rast[[8]],
                 colors = rast.pal2,
                 opacity = 1,
                 group = "Aug SST") %>% 
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
             completedColor = "#7D4479")

