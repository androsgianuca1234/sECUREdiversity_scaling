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
                    s.bms_id,
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

## estimate section length from transect where section length is not available
section_walk_dt[,nbr_section:=.N,by=.(transect_id,year)][,avg_section_length:=transect_length/nbr_section]
section_walk_dt[,est_section_length:=avg_section_length][!is.na(section_length),est_section_length:=section_length]

## remove all sections where no information about length is available
section_walk_dt.1 <-  section_walk_dt[!is.na(est_section_length),]

## build spatial object for sections
section_sf.1 <- sf::st_as_sf(section_walk_dt.1[!is.na(x)],coords= c("x", "y"), crs = 3035, agr = "constant")

## grid
g.bbox <- raster::extent(2500000,5500000,1400000,5100000)
g.bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(g.bbox, 'SpatialPolygons')), 3035)

gr.200 <- sf::st_make_grid(g.bbox_sf, cellsize = 200000, what = 'polygons')
gr.100 <- sf::st_make_grid(g.bbox_sf, cellsize = 100000, what = 'polygons')
gr.50 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
gr.25 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
gr.10 <- sf::st_make_grid(g.bbox_sf, cellsize = 10000, what = 'polygons')
gr.5 <- sf::st_make_grid(g.bbox_sf, cellsize = 5000, what = 'polygons')

## merge transect section to a target length
transect_splitter <- function(x, clip_value){

    if(length(x)>=1){
    x <- cbind(x,seq_along(x))
    cs_x <- cumsum(x[order(x[,1],x[,2]),1])
    res <- order(x[,1],x[,2])[1:order(abs(cs_x-clip_value))[1]]
    }else{res <- NA}

  return(res)
}

## define minimum length for merging sections, with tolerance around the length
tr_length <- 200 ## target length
tolerance <- 25 ## +- tolerance

b_res <- data.table::data.table()
f <- data.table::data.table()

cel_n <- 130

for (cel_n in seq_along(gr.200)){

plot(gr.200, col = NA, border = 'orange')
plot(gr.200[cel_n], col = NA, border = 'red', lwd=2, add = TRUE)

gr.200_section <- unlist(sf::st_intersects(gr.200[cel_n],section_sf.1))
grid_data <- section_sf.1[gr.200_section,]
if (!length(grid_data$site_id)>=1){next()}


  sf::st_geometry(grid_data) <- NULL
  grid_data <- data.table::data.table(grid_data)
  grid_data[, year_trans_sect := paste(year, transect_id, section_id, sep='_')]

  for (tr_length in c(tr_length)){

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

      ## get unique
      e <- unique(b[,.(year,transect_id,transect_split,gr_section_length)])

      if (!length(e[abs(gr_section_length - tr_length) <= tolerance,gr_section_length])>=1){next()}

      f <- rbind(f, cbind(grid_cell = cel_n, tr_length = tr_length,e[abs(gr_section_length - tr_length) <= tolerance, sum(gr_section_length),by=year]))

    }
  }

saveRDS(b_res,'output/section_merging.rds')

## keep merged section within 175-225m long

merged_section <- b_res[gr_section_length>=(tr_length-tolerance) & gr_section_length<=(tr_length+tolerance),]
merged_section[ ,m_site_id := paste(transect_split,transect_id,sep='_')]
merged_section <- unique(merged_section[,.(bms_id,site_id,transect_id,transect_split)])
section_walk_dt.2 <- unique(section_walk_dt.1[,.(site_id,year,est_section_length,x,y)])

data.table::setkey(merged_section,'site_id')
data.table::setkey(section_walk_dt.2,'site_id')

section_walk_dt.2 <- merge(section_walk_dt.2,merged_section)
section_walk_dt.2 <- section_walk_dt.2[,m_x := mean(x), by = .(transect_id, transect_split)]
section_walk_dt.2 <- section_walk_dt.2[,m_y := mean(y), by = .(transect_id, transect_split)]

site2merged_id <- unique(section_walk_dt.2[,.(site_id,transect_id,transect_split,est_section_length,m_x,m_y)])
site2merged_id[,merged_section_length:=sum(est_section_length),.(transect_id,transect_split)]
site2merged_id[,merged_section_id:=paste(transect_id,transect_split,sep='_')]
site2merged_id <- site2merged_id[order(merged_section_id),]
saveRDS(site2merged_id,'output/site2merged_id.rds')

section_walk_merged <- unique(section_walk_dt.2[,.(transect_id,transect_split,m_x,m_y)])
section_merged_sf <- sf::st_as_sf(section_walk_dt.2,coords= c("m_x", "m_y"), crs = 3035, agr = "constant")
section_merged_sf_u <- sf::st_as_sf(section_walk_merged,coords= c("m_x", "m_y"), crs = 3035, agr = "constant")

saveRDS(section_merged_sf,'output/section_merged_sf.rds')

## plot standardized sampling plot
section_merged_sf <- readRDS('output/section_merged_sf.rds')

data(Europe, package = 'tmap')
sf_e <- sf::st_as_sf(Europe)
sf_e <- sf::st_transform(sf_e, crs = 3035)
bbox <- raster::extent(1480000,6534652,1241889,5756279)
bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(bbox, 'SpatialPolygons')), 3035)
sf_e_clip <- sf::st_intersection(sf_e,bbox_sf)

dev.new()
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90')
plot(section_merged_sf$geometry, pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)

s_m_sf <- section_merged_sf[,c("transect_id","transect_split")]
sf::st_geometry(s_m_sf) <- NULL
s_m_dt <- data.table::data.table(s_m_sf)

text(1818716,4231836,paste('section:',unique(s_m_dt)[,.N]),pos=4)
text(1818716,3891651,paste('transect:',length(unique(s_m_dt[,transect_id]))),pos=4)

section_merged_sf_u$gr200 <- unlist(sf::st_intersects(section_merged_sf_u,gr.200))
section_merged_sf_u$gr100 <- unlist(sf::st_intersects(section_merged_sf_u,gr.100))
section_merged_sf_u$gr50 <- unlist(sf::st_intersects(section_merged_sf_u,gr.50))
section_merged_sf_u$gr25 <- unlist(sf::st_intersects(section_merged_sf_u,gr.25))
section_merged_sf_u$gr10 <- unlist(sf::st_intersects(section_merged_sf_u,gr.10))
section_merged_sf_u$gr5 <- unlist(sf::st_intersects(section_merged_sf_u,gr.5))

section_merged_grided <- section_merged_sf_u
sf::st_geometry(section_merged_grided) <- NULL
section_merged_grided_dt <- data.table::data.table(section_merged_grided)


## eplore data nbr of segement 200m per 5km
a <- section_merged_grided_dt[,.N,by=gr5]
a <- section_merged_grided_dt[,.N,by=gr10]

plot(gr.200[c(65:69)])
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.50,col=NA,border='grey50',lty=2,add=TRUE)
plot(gr.100,col=NA,border='orange',lty=2,add=TRUE)
plot(gr.200,col=NA,border='red',lwd=2,add=TRUE)
plot(section_merged_sf$geometry, pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)
plot(gr.5[a$gr5[a$N>=1]],col=NA,border='orange',add=TRUE)
plot(gr.5[a$gr5[a$N>=2]],col=NA,border='blue',add=TRUE)
plot(gr.5[a$gr5[a$N>=3]],col=NA,border='red',add=TRUE)

plot(gr.200[c(124:128)])
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.50,col=NA,border='grey50',lty=2,add=TRUE)
plot(gr.100,col=NA,border='orange',lty=2,add=TRUE)
plot(gr.200,col=NA,border='red',lwd=2,add=TRUE)
plot(section_merged_sf$geometry, pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)
plot(gr.5[a$gr5[a$N>=1]],col=NA,border='orange',add=TRUE)
plot(gr.5[a$gr5[a$N>=2]],col=NA,border='blue',add=TRUE)
plot(gr.5[a$gr5[a$N>=3]],col=NA,border='red',add=TRUE)

plot(gr.200[c(160:164)])
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.50,col=NA,border='grey50',lty=2,add=TRUE)
plot(gr.100,col=NA,border='orange',lty=2,add=TRUE)
plot(gr.200,col=NA,border='red',lwd=2,add=TRUE)
plot(section_merged_sf$geometry, pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)
plot(gr.5[a$gr5[a$N>=1]],col=NA,border='orange',add=TRUE)
plot(gr.5[a$gr5[a$N>=2]],col=NA,border='blue',add=TRUE)
plot(gr.5[a$gr5[a$N>=3]],col=NA,border='red',add=TRUE)


plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90')
plot(section_merged_sf$geometry, pch = 20, cex = 0.8, col = 'cadetblue', add = TRUE)
plot(gr.10[r[r[,2]>=1,1]], col = NA, border = 'orange', add = TRUE)
plot(gr.10[r[r[,2]>=2,1]], col = NA, border = 'red', add = TRUE)
plot(gr.10[r[r[,2]>=3,1]], col = NA, border = 'blue', add = TRUE)




section_within <- sf::st_intersects(gr.5,section_merged_sf_u)
nbr_merged_section[i] <- length(unlist(section_within))
}

gr.5$nbr_merged_sect <- nbr_merged_section


plot(section_merged_5km)
nbr_section_merged <- lapply(section_merged_5km,FUN=length)
nbr_section_merged[nbr_section_merged>0]
