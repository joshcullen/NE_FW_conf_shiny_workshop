
## Download environmental raster and create simulated tracks ##


library(tidyverse)
library(lubridate)
library(sf)
library(raster)
library(rerddapXtracto)
library(cmocean)
library(circular)
library(tictoc)


source('Cullen_Materials/helper functions.R')


### Download monthly SST for 2021 ###

xpos <- c(-77, -68) 
ypos <- c(35, 45)
tpos <- c("2021-01-16", "2021-12-16")
sstInfo <- rerddap::info('jplMURSST41mday')
sst.bbox <- rxtracto_3D(sstInfo, parameter = 'sst', xcoord = xpos, ycoord = ypos, tcoord = tpos)



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


# Convert to RasterBrick to reduce raster resolution
sst.rast2 <- sst.rast %>% 
  group_split(month) %>% 
  purrr::map(., ~rasterFromXYZ(.[,c('x','y','sst')], digits = 2, crs = 4326)) %>% 
  raster::brick()

sst.coarse <- raster::aggregate(sst.rast2, fact = 4)  #convert to 4 km res


plot(sst.rast2)
plot(sst.coarse)


# Convert aggregated RasterBrick to data.frame for ggplot and export
sst.coarse.df <- sst.coarse %>% 
  as.data.frame(xy = TRUE) %>% 
  pivot_longer(cols = -c('x','y'), names_to = 'month', values_to = 'sst') %>% 
  mutate(month = str_replace(month, "sst.", ""))

# Plot coarse monthly SST
ggplot() +
  geom_raster(data = sst.coarse.df, aes(x, y, fill = sst)) +
  scale_fill_cmocean() +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  facet_wrap(~ month)






### Import and add all separate .shp as one file for ease of use ###
# Obtained from BOEM (https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data)

# only import relevant polygons
wind <- st_read("Cullen_Materials/BOEM-Renewable-Energy-Shapefiles_1/BOEMWindLeases_OutlinePolys.shp") %>% 
  st_transform(4326)


# plot over SST
ggplot() +
  geom_raster(data = sst.coarse.df %>%
                filter(month == 8), aes(x, y, fill = sst)) +
  scale_fill_cmocean() +
  geom_sf(data = wind) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf()






### Simulate tracks w/in study region ###

# choose different starting points (lon, lat) in Mid-Atlantic Bight
# using locator() function and clicking on different parts of map
sim.tracks <- data.frame(x = c(-71.50604, -75, -72, -71, -73),
                         y = c(39.71869, 37, 38.5, 40, 39)) %>% 
  st_as_sf(., coords = c('x','y'), crs = 4326) %>% 
  st_transform(3395) %>%   # transform to World Mercator projection to units are meters
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

# split sim.tracks starting locs into a list (to be used when simulating tracks)
sim.tracks.list <- split(sim.tracks, seq(nrow(sim.tracks)))




#simulate tracks
set.seed(2)

nobs<- rep(1000, each = 5)  #tracks are 1000 obs long each
SL.params<- data.frame(shape=c(0.25, 2, 10), scale = c(0.001, 0.001, 0.001))
TA.params<- data.frame(mu=c(pi, pi, 0), rho = c(0.8, 1e-12, 0.9))  #error if rho == 0

# simulate from HMM
tic()
tracks<- HMM.sim(nsim = 5,
                 nobs = nobs,
                 SL.params = SL.params,
                 TA.params = TA.params,
                 Z0 = c(0,0))
toc()
#takes ~1 sec to run


# Add starting coordinates to all obs for each track
tracks.df<- tracks %>% 
  map2(.x = ., .y = sim.tracks.list,
       ~{.x %>% 
           mutate(x = x + .y[,1],
                  y = y + .y[,2])
       }) %>% 
  bind_rows()

# Convert tracks back to Long/Lat and store as data.frame
tracks.df2 <- tracks.df %>% 
  st_as_sf(., coords = c('x','y'), crs = 3395) %>% 
  st_transform(4326) %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% 
  st_drop_geometry()



# plot simulated tracks over SST and wind lease areas
ggplot() +
  geom_raster(data = sst.coarse.df %>%
                filter(month == 8), aes(x, y, fill = sst)) +
  scale_fill_cmocean() +
  geom_sf(data = wind) +
  geom_path(data = tracks.df2, aes(x, y, group = id, color = id)) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf()




### Export spatial layers ###

# write.csv(sst.coarse.df, "Cullen_Materials/Monthly_SST_2021.csv", row.names = FALSE)

# st_write(wind, "Cullen_Materials/NE_Offshore_Wind.shp", driver = "ESRI Shapefile", append = F)

# write.csv(tracks.df2, "Cullen_Materials/Simulated tracks.csv", row.names = FALSE)
