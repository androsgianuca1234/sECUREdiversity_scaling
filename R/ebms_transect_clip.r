## Extract eBMS data for the sECURE project
## DATE: 03/03/2018
## AUTHOR: Reto Schmucki retoschm@ceh.ac.uk
##
## add a clever way to clip transect to have comparable length :smile:
## ==========================================

R --vanilla

dbconnect_param <- read.csv("data\\connect_file.csv", stringsAsFactors = FALSE)

library('RPostgreSQL')

dbcon <- dbConnect(dbDriver('PostgreSQL'),
                            dbname = 'ebms_v1_2',
                            host = 'localhost',
                            port = 5432,
                            user = dbconnect_param[1,1],
                            password = dbconnect_param[2,1])

## section_walked_per_year

section_walkQ <- 'SELECT DISTINCT
                    EXTRACT(YEAR from v.visit_date) as year,
                    s.site_id,
                    s.transect_id,
                    s.section_id,
                    s.transect_length,
                    s.section_length,
                    s.monitoring_type,
                    st_X(g.centroid_geom) as x,
                    st_Y(g.centroid_geom) as y
                  FROM
                    ebms.m_site as s
                    LEFT JOIN ebms.m_visit as v ON s.transect_id = v.transect_id
                    LEFT JOIN ebms.m_site_geo as g ON s.site_id = g.site_id
                  WHERE
                    EXTRACT(YEAR from v.visit_date) >= 2010 AND
                    EXTRACT(YEAR from v.visit_date) <= 2015 AND
                    s.monitoring_type = \'2\' AND
                    g.centroid_geom IS NOT NULL
                  ORDER BY
                    s.transect_id,
                    year,
                    s.section_id;'

section_walk_dt <- data.table::data.table(dbGetQuery(dbcon,section_walkQ))
unique(substr(section_walk_dt$transect_id,1,5))
section_walk_dt[,nbr_section:=.N,by=.(transect_id,year)][,avg_section_length:=transect_length/nbr_section]
section_walk_dt[,est_section_length:=avg_section_length][!is.na(section_length),est_section_length:=section_length]
section_sf <- sf::st_as_sf(section_walk_dt[!is.na(x)],coords= c("x", "y"), crs = 3035, agr = "constant")
length(unique(section_sf$site_id))

data(Europe,package='tmap')
sf_e <- sf::st_as_sf(Europe)
sf_e <- sf::st_transform(sf_e,crs=3035)

# raster::drawExtent()

bbox <- raster::extent(1480000,6534652,1241889,5756279)
bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(bbox, 'SpatialPolygons')), 3035)
sf_e_clip <- sf::st_intersection(sf_e,bbox_sf)

## grid
g.bbox <- raster::extent(2500000,5500000,1400000,5100000)
g.bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(g.bbox, 'SpatialPolygons')), 3035)
gr <- sf::st_make_grid(g.bbox_sf, cellsize = 250000, what = 'polygons')
gr <- sf::st_make_grid(g.bbox_sf, cellsize = 10000, what = 'polygons')
dev.new()
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90')
plot(section_sf$geometry[!is.na(section_sf$est_section_length)], pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)
text(1818716,4231836,paste('section:',length(unique(section_sf$site_id[!is.na(section_sf$est_section_length)]))),pos=4)
text(1818716,3891651,paste('transect:',length(unique(section_sf$transect_id[!is.na(section_sf$est_section_length)]))),pos=4)

dev.new()
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90')
plot(section_sf$geometry[!is.na(section_sf$est_section_length)], pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)

plot(gr, col = NA, border = 'orange', add = TRUE)
plot(gr[29], col = NA, border = 'red', lwd=2, add = TRUE)
gr.section <- unlist(sf::st_intersects(gr[29],section_sf))
plot(section_sf$geometry[gr.section], pch = 20, cex = 0.8, col = 'violetred', add = TRUE)

plot(gr[79], col = NA, border = 'red', lwd=2, add = TRUE)
gr.section <- unlist(sf::st_intersects(gr[79],section_sf))
plot(section_sf$geometry[gr.section], pch = 20, cex = 0.8, col = 'blue', add = TRUE)

plot(gr[89], col = NA, border = 'red', lwd=2, add = TRUE)
gr.section <- unlist(sf::st_intersects(gr[89],section_sf))
plot(section_sf$geometry[gr.section], pch = 20, cex = 0.8, col = 'orange', add = TRUE)

plot(gr[88], col = NA, border = 'red', lwd=2, add = TRUE)
gr.section <- unlist(sf::st_intersects(gr[88],section_sf))
plot(section_sf$geometry[gr.section], pch = 20, cex = 0.8, col = 'orange', add = TRUE)


## focus on a small box (eg. 79)
cel_n <- 89
gr.section <- unlist(sf::st_intersects(gr[cel_n],section_sf))
dev.new()
plot(gr[cel_n], col = NA, border = 'red', lwd=2, type='n')
plot(sf_e_clip$geometry, col='grey90', add = TRUE)
plot(gr[cel_n], col = NA, border = 'red', lwd=2, add = TRUE)
plot(section_sf$geometry[gr.section], pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)

gr.50 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
gr.50_sub <- unlist(sf::st_intersects(sf::st_buffer(gr[cel_n],-2),gr.50))
plot(gr.50[gr.50_sub],col = NA, border = 'orange', lty=3, lwd=1, add = TRUE)

large_grid_section <- section_sf[gr.section,]
a <- sf::st_intersects(gr.50[gr.50_sub],section_sf$geometry[gr.section])

sampling_effort <- matrix(NA,nrow=dim(a)[1],ncol=3)

for (i in  seq_along(a)){
  b <- large_grid_section [unlist(a[i]),]
  sampling_effort[i,1] <- length(unique(b$transect_id))
  sampling_effort[i,2] <- length(unique(b$site_id))
  sampling_effort[i,3] <- sum(b$est_section_length[!duplicated(b$site_id)],na.rm=TRUE)
}

sampling_effort[sampling_effort==0] <- NA

subgrid <- sf::st_sf(gr.50[gr.50_sub])
subgrid$n_transect <- sqrt(sampling_effort[,1])
subgrid$n_section <- sqrt(sampling_effort[,2])
subgrid$n_total_length <- sqrt(sampling_effort[,3])

dev.new()
par(mfrow=c(2,2))
plot(subgrid, pal=rev(heat.colors(20)))
plot(gr[cel_n], col = NA, border = 'red', lwd=1)
plot(sf_e_clip$geometry, col='grey90', add = TRUE)
plot(gr[cel_n], col = NA, border = 'red', lwd=2, add = TRUE)
plot(section_sf$geometry[gr.section], pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)
plot(gr.50[gr.50_sub],col = NA, border = 'orange', lty=2, lwd=1, add = TRUE)


# transect_to_keep <- section_year_n[N >= 3, mean(N) == N, by = 'transect_id'][V1 == TRUE, unique(transect_id)]
#
# transect_length <- section_walk_dt[transect_id %in% transect_to_keep, sum(section_length), by =  c('year', 'transect_id')]
# summary(transect_length[,V1])
# summary(section_walk_dt[,section_length])

### splitter function
# this function find set of sections to merge (or a single section) to result with distance closest to the target value

transect_splitter <- function(x, clip_value){

    if(length(x)>=1){
    x <- cbind(x,seq_along(x))
    cs_x <- cumsum(x[order(x[,1],x[,2]),1])
    res <- order(x[,1],x[,2])[1:order(abs(cs_x-clip_value))[1]]
    }else{res <- NA}

  return(res)
}

### example
expl_1 <- c(70, 90, 200, 230, 70, 430, 50, 110, 230, 50)
expl_2 <- rep(50,16)
expl_3 <- c(150)
expl_4 <- NA
sum(expl_1[transect_splitter(expl_1, 80)])
sum(expl_2[transect_splitter(expl_2, 400)])
sum(expl_3[transect_splitter(expl_3,80)])
sum(expl_4[transect_splitter(expl_4,80)])
###  end



tr_length <- 250 ## target length
tolerance <- 50 ## +- tolerance

dev.new()
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90')
plot(section_sf$geometry[!is.na(section_sf$est_section_length)], pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)
text(1818716,4231836,paste('section:',length(unique(section_sf$site_id[!is.na(section_sf$est_section_length)]))),pos=4)
text(1818716,3891651,paste('transect:',length(unique(section_sf$transect_id[!is.na(section_sf$est_section_length)]))),pos=4)

b_res <- data.table::data.table()
f <- data.table::data.table()

for (cel_n in seq_along(gr)){

plot(gr, col = NA, border = 'orange')
plot(gr[cel_n], col = NA, border = 'red', lwd=2, add = TRUE)

  gr.section <- unlist(sf::st_intersects(gr[cel_n],section_sf))
  grid_data <- section_sf[gr.section,]
  if (!length(grid_data$site_id)>=1){next()}

  sf::st_geometry(grid_data) <- NULL
  grid_data <- data.table::data.table(grid_data)
  grid_data[, year_trans_sect := paste(year, transect_id, section_id, sep='_')]

  for (tr_length in c(100,150,200,250,300,350)){

    transect_length <- grid_data[, sum(est_section_length, na.rm=TRUE), by =  c('year', 'transect_id')]
    tr_k <- transect_length[V1 >= (tr_length - tolerance), transect_id]
    grid_data_gr <- grid_data[transect_id %in% tr_k, section_id[transect_splitter(est_section_length, tr_length)], by = c('year', 'transect_id')][, year_trans_sect := paste(year, transect_id, V1, sep='_')]
    b <- data.table::copy(grid_data[paste(year, transect_id, section_id, sep='_') %in% grid_data_gr$year_trans_sect,])
    b[, 'transect_split' := paste(tr_length, 'tr', 1, sep = '_')]
    b[, gr_section_length := sum(est_section_length,na.rm = TRUE), by = c('year', 'transect_id')]

    (added_to <- b[,.N] )

    i <- 2
      while(added_to > 1){
      tr_ki <- data.table::copy(transect_length[V1 >= (tr_length - tolerance), transect_id])
      c <- grid_data[!year_trans_sect %in% b$year_trans_sect,][transect_id %in% tr_ki,
                            section_id[transect_splitter(est_section_length, tr_length)],
                            by = c('year', 'transect_id')][, year_trans_sect := paste(year, transect_id, V1, sep='_')]

      d <- data.table::copy(grid_data[year_trans_sect %in% c$year_trans_sect,])
      d[, 'transect_split' := paste(tr_length, 'tr', i, sep = '_')]
      d[, gr_section_length := sum(est_section_length,na.rm = TRUE), by = c('year', 'transect_id')]
      added_to <- d[,.N]
      i <- i + 1
      print(paste(i,':', added_to))
      b <- rbind(b, d)

      }

    b_res <-rbind(b_res,b)

    e <- unique(b[,.(year,transect_id,transect_split,gr_section_length)])
    if (!length(e[abs(gr_section_length - tr_length) <= tolerance,gr_section_length])>=1){next()}
    f <- rbind(f, cbind(grid_cell = cel_n, tr_length = tr_length,e[abs(gr_section_length - tr_length) <= tolerance, sum(gr_section_length),by=year]))

    # plot(e$gr_section_length - tr_length)
    # abline(h = -tolerance, col = 'red', lty = 2)
    # abline(h = tolerance, col = 'red', lty = 2)
    # text(100, 35 * tolerance, col = 'blue', e[abs(gr_section_length - tr_length) <= tolerance, .N], pos = 4)
    # text(100, 40 * tolerance, col = 'red', e[abs(gr_section_length - tr_length) > tolerance, .N], pos = 4)
    # text(100, 30 * tolerance, col = 'magenta', paste('target length:',tr_length), pos = 4)
  }
}

saveRDS(b_res,'output/section_merging.rds')
b_res <- readRDS('output/section_merging.rds')
## Q. do we need to merge the sections or could we just have sampled section to reach a specific lenght per area. This would involve not loosing any section and give more
## opportunity to reach a specific length.
#### End

plot(
f2 <- f[,mean(V1),by=.(tr_length,grid_cell)]
f3 <- f2[,optimum:=max(V1),by=grid_cell][V1==optimum,]
  ,type='l')

dev.new()
par(mfrow=c(1,2))
for (gc in seq_along(gr)){
  if(!length(f[grid_cell==gc,V1])>=1){next}
  plot(gr, col = NA, border = 'orange')
  plot(gr[gc],col='magenta',add=TRUE)
  plot(f[grid_cell==gc,mean(V1),by=tr_length],type='l',col='violetred')
  Sys.sleep(1)
}
e <- b[,sum(section_length), by = c('year', 'transect_id', 'transect_split')]





hist(unique(e[V1 >= 100 & V1 <= 200, .(transect_id, transect_split, V1)])$V1)


# summary(d[, sum(section_length), by = c('year','transect_id')])


# hist(unique(b[, sum(section_length), by = c('year', 'transect_id')][,c('transect_id', 'V1')])$V1)
# hist(unique(d[, sum(section_length), by = c('year', 'transect_id')][,c('transect_id', 'V1')])$V1)

b[, paste0('transect_split_',tr_length) := paste0('tr_1',tr_length)]
d[, paste0('transect_split_',tr_length) := paste0('tr_b',tr_length)]

e <- rbind(b,d)

res <- e[, sum(section_length), by = c('transect_id', 'year', paste0('transect_split_',tr_length))][order(transect_id, year),][V1 >= (tr_length - 100) & V1 <= (tr_length + 100),]

length(unique(res[,-c('year')])$V1)
hist(unique(res[,-c('year')])$V1)

### MAP
section_geomQ <- 'SELECT DISTINCT
                    s.transect_id,
                    s.section_id,
                    s.section_length,
                    s.monitoring_type,
                    st_X(g.centroid_geom) as longitude,
                    st_Y(g.centroid_geom) as latitude
                  FROM
                    ebms.m_site as s
                    LEFT JOIN ebms.m_site_geo as g ON s.site_id = g.site_id
                  WHERE
                    s.section_length > 1
                  ORDER BY
                    s.transect_id,
                    s.section_id;'

section_geom_dt <- data.table::data.table(dbGetQuery(dbcon,section_geomQ ))
transect_section_geom <- section_geom_dt[monitoring_type == 2 & !is.na(latitude),][, trans_section := paste(transect_id,section_id,sep='_')]

data.table::setkey(transect_section_geom, transect_id, section_id)
data.table::setkey(b, transect_id, section_id)

f <- merge(b, transect_section_geom[,.(transect_id, section_id, longitude, latitude, trans_section)], by = c('transect_id','section_id'))

f_geo <- f[, ':='(split_transect_length = sum(section_length), latitude_c = mean(latitude), longitude_c = mean(longitude)), by = c('year','transect_id','transect_split') ]

split_geom <- unique(f_geo[,c('transect_id', 'transect_split', 'split_transect_length', 'latitude_c', 'longitude_c')])

split_geom_sf <- sf::st_as_sf(split_geom, coords = c('longitude_c','latitude_c'), crs=3035, agr = 'constant')
str_section_geom <- split_geom_sf[split_geom_sf$split_transect_length >= (tr_length - 50) & split_geom_sf$split_transect_length <= (tr_length + 50),]

plot(str_section_geom[,'geometry'])

eu_sf <-  sf::st_as_sf(rnaturalearth::ne_countries(scale = 'large', type = 'countries',
                          continent = 'Europe')
                          )
eu <- sf::st_transform(eu_sf$geometry,3035)
## plot(eu, add = TRUE)
## bbox <- raster::drawExtent()

bbox <- raster::extent(2200000,6200000,1200000,6200000)
bbox <- raster::extent(2500000,5500000,1400000,5600000)
bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(bbox, 'SpatialPolygons')), 3035)

plot(bbox_sf)
plot(eu, add = TRUE)
plot(str_section_geom[, 'geometry'], add = TRUE)

gr_10 <- sf::st_make_grid(bbox_sf, cellsize = 10000, what = 'polygons')
gr_25 <- sf::st_make_grid(bbox_sf, cellsize = 25000, what = 'polygons')
gr_50 <- sf::st_make_grid(bbox_sf, cellsize = 50000, what = 'polygons')
gr_100 <- sf::st_make_grid(bbox_sf, cellsize = 100000, what = 'polygons')


r <- raster::raster(xmn=2500000, ymn=1400000, xmx=5500000, ymx=5600000, res=50000)
raster::crs(r) <- sp::CRS('+init=EPSG:3035')
r <- raster::setValues(r,1:raster::ncell(r))


plot(bbox_sf)
plot(eu, add = TRUE)
raster::plot(r,add=TRUE)
plot(gr_50, border = 'cyan', add = TRUE)


str_section_geom$gr_100_intersects <- sf::st_intersects(str_section_geom,gr_100)
plot(gr_100[777], col = 'blue', add = TRUE)
plot(str_section_geom$geometry[str_section_geom$gr_100_intersects==777],col='red',add=TRUE)


plot(gr_10, border = 'orange', add = TRUE)
plot(gr_25, border = 'midnightblue', add = TRUE)
plot(gr_50, border = 'magenta', add = TRUE)
plot(gr_100, border = 'cyan', add = TRUE)


eu_sf <-  sf::st_as_sf(rnaturalearth::ne_countries(scale = 'large', type = 'countries',
                          continent = 'Europe')
                          )
eu <- sf::st_transform(eu_sf$geometry,3035)
plot(eu, add = TRUE)

## transect is monitoring_type 2
