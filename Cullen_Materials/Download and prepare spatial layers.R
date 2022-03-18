
## Download environmental raster and create simulated tracks ##


library(tidyverse)
library(lubridate)
library(sf)
# library(raster)
library(rerddapXtracto)
library(cmocean)
# library(leaflet)
# library(leafem)
# library(shiny)



## Download monthly SST for 2021

xpos <- c(-77, -68) 
ypos <- c(35, 45)
tpos <- c("2021-01-16", "2021-12-16")
sstInfo <- rerddap::info('jplMURSST41mday')
sst.bbox <- rxtracto_3D(sstInfo, parameter = 'sst', xcoord = xpos, ycoord = ypos, tcoord = tpos)



# function to convert list object from rxtracto() into a data.frame for ggplot
array2df <- function(lon, lat, var, var.name, time) {
  dims <- dim(var)
  rast.df <- expand.grid(x = lon, y = lat, time = time)
  rast.df$var <- array(apply(var, 3, rbind), dims[1] * dims[2] * dims[3])
  names(rast.df)[4] <- var.name
  
  return(rast.df)
}



# Create data.frame to plot raster in ggplot
sst.rast <- array2df(lon = sst.bbox$longitude, lat = sst.bbox$latitude, var = sst.bbox$sst,
                     var.name = "sst", time = sst.bbox$time)
sst.rast$time <- as.numeric(month(sst.rast$time))
names(sst.rast)[3] <- "month"



# Plot monthly SST
ggplot() +
  geom_raster(data = sst.rast, aes(x, y, fill = sst)) +
  scale_fill_cmocean() +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  facet_wrap(~ month)


# Convert to RasterStack to reduce raster resolution
sst.rast2 <- rasterFromXYZ(sst.rast %>% 
                             filter(month == 1) %>% 
                             dplyr::select(-month), crs = 4326)



## Import and add all separate .shp as one file for ease of use
# Obtained from BOEM (https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data)

# identify all shapefiles
layers <- grep("\\.shp$", list.files("Cullen_Materials/BOEM-Renewable-Energy-Shapefiles_1/"),
               value = TRUE)

# load into single spatial object
wind <- do.call(rbind, lapply(layers, function(x) read_sf(paste0("Cullen_Materials/BOEM-Renewable-Energy-Shapefiles_1/", x))))


# plot over SST
ggplot() +
  geom_raster(data = sst.rast %>% 
                filter(month == 8), aes(x, y, fill = sst)) +
  scale_fill_cmocean() +
  geom_sf(data = wind) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf()




### Export spatial layers ###

write.csv(sst.rast, "Cullen_Materials/Monthly_SST_2021.csv", row.names = FALSE)

st_write(wind, "Cullen_Materials/NE_Offshore_Wind.shp", driver = "ESRI Shapefile")
