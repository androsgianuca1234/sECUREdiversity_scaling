r <- raster::raster(xmn=2500000, xmx=5500000, ymn=1400000, ymx=5100000, crs=3035)
raster::res(r) <- 5000
r[] <- seq_along(1:raster::ncell(r))
raster::plot(r)


g.bbox <- raster::extent(2500000,5500000,1400000,5100000)
g.bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(g.bbox, 'SpatialPolygons')), 3035)
gr.5 <- sf::st_make_grid(g.bbox_sf, cellsize = 5000, what = 'polygons')

obj <- read.csv(blagllag)

c <- raster::extract(r,as(gr.5[unique(obj$gr5)],'Spatial'))

r[] <- NA
r[unlist(c)] <- 1

raster::plot(r)
r
