g.bbox <- raster::extent(2500000,5500000,1400000,5100000)
g.bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(g.bbox, 'SpatialPolygons')), 3035)

gr.200 <- sf::st_make_grid(g.bbox_sf, cellsize = 200000, what = 'polygons')
gr.100 <- sf::st_make_grid(g.bbox_sf, cellsize = 100000, what = 'polygons')
gr.50 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
gr.25 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
gr.10 <- sf::st_make_grid(g.bbox_sf, cellsize = 10000, what = 'polygons')
gr.5 <- sf::st_make_grid(g.bbox_sf, cellsize = 5000, what = 'polygons')


gr.200_centroid <- data.frame(grid_id=seq_along(gr.200),sf::st_coordinates(sf::st_centroid(gr.200)))
gr.100_centroid <- data.frame(grid_id=seq_along(gr.100),sf::st_coordinates(sf::st_centroid(gr.100)))
gr.50_centroid <- data.frame(grid_id=seq_along(gr.50),sf::st_coordinates(sf::st_centroid(gr.50)))
gr.25_centroid <- data.frame(grid_id=seq_along(gr.25),sf::st_coordinates(sf::st_centroid(gr.25)))
gr.10_centroid <- data.frame(grid_id=seq_along(gr.10),sf::st_coordinates(sf::st_centroid(gr.10)))
gr.5_centroid <- data.frame(grid_id=seq_along(gr.5),sf::st_coordinates(sf::st_centroid(gr.5)))
