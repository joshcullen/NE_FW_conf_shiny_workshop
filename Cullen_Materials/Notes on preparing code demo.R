

### Leaflet example for creating interactive (and reactive) maps in Shiny ###

# First, cover how to use Leaflet outside of Shiny
  # Walk through examples of how to:
     # 1) create a simple basemap
     # 2) add different features (points, lines, polygons, rasters)
          # might be good time to also reference addFeatures() from {leafem} since you don't need to pre-specify which type of feature it is
          # also mention that {terra} SpatRaster is not currently supported, but can add {Raster} or {Stars} layers
     # 3) make modifications/additions to features, such as change colors, sizes, opacity, palettes
     # 4) add multiple basemaps, scale_bar, other widgets


# Second, cover how to integrate Leaflet map into Shiny
   # Walk through examples of how to:
      # 1) Render a Leaflet map and splitting static vs dynamic components up using renderLeaflet() and leafletProxy()
      # 2) Provide example where points or tracks are filtered for a given month/year and map is updated
      # 3) Provide example where raster layer is reactively updated based upon month/year selected for input


# For data to plot, easiest would be to simulate tracks in Mid Atlantic Bight or off New England (for points/tracks), maybe some MPAs or proposed/installed offshore windfarms for polygons, and SST or ocean current velocity for spatiotemporal raster data
   # SST data can be accessed via {rerddapXtracto} and ocean current velocity can be extracted via {rwind} or {rerddapXtracto}