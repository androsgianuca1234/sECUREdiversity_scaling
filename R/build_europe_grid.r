## =============================================
## script to build spatial grid and raster to extract environmental data
## in sECURE project.
##
## Author: Reto Schmucki - retoschm@ceh.ac.uk
## Date: 18.03.2018
##
## Note: Using Europe, we use the bounding box defined by
##
## xmin:2500000
## xmax:5500000
## ymin:1400000
## ymax:5600000
##
## Projection: EPSG 3035
## ============================================

## Build raster with resolution 50 km
r <- raster::raster(xmn=2500000, ymn=1400000, xmx=5500000, ymx=5600000, res=50000)
raster::crs(r) <- sp::CRS('+init=EPSG:3035')
r <- raster::setValues(r,1:raster::ncell(r)))

## Build a corresponding grid as polygons
bbox <- raster::extent(2500000,5500000,1400000,5600000)
bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(bbox, 'SpatialPolygons')), 3035)
gr <- sf::st_make_grid(bbox_sf, cellsize = 50000, what = 'polygons')


## Write raster
raster::writeRaster(r, "output/eu_grid_50km.tif", format="GTiff", overwrite=TRUE)

## Write shapefile
sf::st_write(gr,"output/eu_grid_50km.shp")
