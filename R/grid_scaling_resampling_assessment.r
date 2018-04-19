##========================================================================
##
## Build map of grid cell with data available for scaling biodiversity
##
##
##========================================================================
R --vanilla

##load toke's data
grid_available <- data.table::fread('output/sECURE scaling selected cells.csv')
grid_available2 <- data.table::fread('output/sECURE scaling selected cells2.csv')
grid_available3 <- data.table::fread('output/sECURE scaling selected cells3.csv')


## build nested grids over Europe (resolution 200, 100, 50, 25, 10, 5km), projection EPGS:3035
g.bbox <- raster::extent(2500000,5500000,1400000,5100000)
g.bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(g.bbox, 'SpatialPolygons')), 3035)
gr.200 <- sf::st_make_grid(g.bbox_sf, cellsize = 200000, what = 'polygons')
# gr.100 <- sf::st_make_grid(g.bbox_sf, cellsize = 100000, what = 'polygons')
# gr.50 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
# gr.25 <- sf::st_make_grid(g.bbox_sf, cellsize = 25000, what = 'polygons')
gr.20 <- sf::st_make_grid(g.bbox_sf, cellsize = 20000, what = 'polygons')
# gr.10 <- sf::st_make_grid(g.bbox_sf, cellsize = 10000, what = 'polygons')
gr.5 <- sf::st_make_grid(g.bbox_sf, cellsize = 5000, what = 'polygons')


unique(unlist(grid_available[,gr_200]))

## build map
data(Europe, package = 'tmap')
sf_e <- sf::st_as_sf(Europe)
sf_e <- sf::st_transform(sf_e, crs = 3035)
bbox <- raster::extent(1480000,6534652,1241889,5756279)
bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(bbox, 'SpatialPolygons')), 3035)
sf_e_clip <- sf::st_intersection(sf_e,bbox_sf)


plot(gr.200)
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.200[unique(unlist(grid_available3[,gr_200]))],border='red',lwd=2,add=TRUE)
plot(gr.20[unique(unlist(grid_available3[,gr_20]))],border='blue',lwd=2,add=TRUE)

dev.new()
plot(gr.200,main='2')
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.200[unique(unlist(grid_available2[,gr_200]))],border='red',lwd=2,add=TRUE)
plot(gr.20[unique(unlist(grid_available2[,gr_20]))],border='blue',lwd=2,add=TRUE)

dev.new()
plot(gr.200,main=3)
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.200[unique(unlist(grid_available3[,gr_200]))],border='red',lwd=2,add=TRUE)
plot(gr.20[unique(unlist(grid_available3[,gr_20]))],border='blue',lwd=2,add=TRUE)
