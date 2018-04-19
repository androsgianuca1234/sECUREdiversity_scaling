R --vanilla

g.bbox <- raster::extent(2500000,5500000,1400000,5100000)
g.bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(g.bbox, 'SpatialPolygons')), 3035)

gr.200 <- sf::st_make_grid(g.bbox_sf, cellsize = 200000, what = 'polygons')
gr.100 <- sf::st_make_grid(g.bbox_sf, cellsize = 100000, what = 'polygons')
gr.50 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
gr.25 <- sf::st_make_grid(g.bbox_sf, cellsize = 25000, what = 'polygons')
gr.20 <- sf::st_make_grid(g.bbox_sf, cellsize = 20000, what = 'polygons')
gr.10 <- sf::st_make_grid(g.bbox_sf, cellsize = 10000, what = 'polygons')
gr.5 <- sf::st_make_grid(g.bbox_sf, cellsize = 5000, what = 'polygons')

## get centroids coordinates
gr.200_centoids <- data.frame(id = seq_along(gr.200),sf::st_coordinates(sf::st_centroid(gr.200)))



## build points
gr.200_centoids_sf <- sf::st_as_sf(gr.200_centoids, coords = c('X', "Y"), crs = 3035, agr = "constant")

## map in Europe
data(Europe, package = 'tmap')
sf_e <- sf::st_as_sf(Europe)
sf_e <- sf::st_transform(sf_e, crs = 3035)
bbox <- raster::extent(1480000,6534652,1241889,5756279)
bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(bbox, 'SpatialPolygons')), 3035)
sf_e_clip <- sf::st_intersection(sf_e,bbox_sf)

plot(bbox_sf)
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.200, col=NA, border='grey50',add=TRUE)
plot(gr.200_centoids_sf, add=TRUE)
