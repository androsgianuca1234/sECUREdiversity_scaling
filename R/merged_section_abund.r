R --vanilla

dbconnect_param <- read.csv("data\\connect_file.csv", stringsAsFactors = FALSE)

library('RPostgreSQL')

dbcon <- dbConnect(dbDriver('PostgreSQL'),
                            dbname = 'ebms_v1_2',
                            host = 'localhost',
                            port = 5432,
                            user = dbconnect_param[1,1],
                            password = dbconnect_param[2,1])

sectioncountQ <- 'SELECT DISTINCT
                    EXTRACT(YEAR from v.visit_date) as year,
                    EXTRACT(MONTH from v.visit_date) as month,
                    EXTRACT(DAY from v.visit_date) as day,
                    s.site_id,
                    sp.species_acpt_sci_name as species,
                    c.butterfly_count as count
                  FROM
                    ebms.b_count as c
                    LEFT JOIN ebms.m_visit as v ON c.visit_id = v.visit_id
                    LEFT JOIN ebms.m_site as s ON c.site_id = s.site_id
                    LEFT JOIN ebms.b_species_id as sp ON c.species_id = sp.species_id
                  WHERE
                    EXTRACT(YEAR from v.visit_date) >= 2010 AND
                    EXTRACT(YEAR from v.visit_date) <= 2015 AND
                    s.monitoring_type = \'2\' AND
                    sp.aggregate = FALSE;'

section_count_dt <- data.table::data.table(dbGetQuery(dbcon,sectioncountQ))
site2merged_id <- readRDS('output/site2merged_id.rds')
data.table::setkey(section_count_dt, site_id)
data.table::setkey(site2merged_id, site_id)

data.table::fwrite(data.frame(id=seq_along(unique(section_count_dt[,species])),species=unique(section_count_dt[order(species),species])),'output/secure_ebms_specieslist.csv')

section_count_dt.2 <- merge(section_count_dt,site2merged_id[,.(site_id,merged_section_id)])

section_count_seasontrim <- section_count_dt.2[month>=4 & month <= 9,]

section_max_abund <- section_count_seasontrim[,sum(count,na.rm=TRUE),by=.(year,month,day,merged_section_id,species)][,max(V1),
                              by=.(year,month,merged_section_id,species)][,max(V1),by=.(year,month,merged_section_id,
                              species)][,sum(V1),by=.(year,species,merged_section_id)][,max(V1),by=.(merged_section_id,
                              species)][order(merged_section_id,species),]

merged_section_coord <- unique(site2merged_id[,.(merged_section_id,m_x,m_y)])
merged_section_coord_sf <- sf::st_as_sf(merged_section_coord,coords= c("m_x", "m_y"), crs = 3035, agr = "constant")

## grid
g.bbox <- raster::extent(2500000,5500000,1400000,5100000)
g.bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(g.bbox, 'SpatialPolygons')), 3035)

gr.200 <- sf::st_make_grid(g.bbox_sf, cellsize = 200000, what = 'polygons')
gr.100 <- sf::st_make_grid(g.bbox_sf, cellsize = 100000, what = 'polygons')
gr.50 <- sf::st_make_grid(g.bbox_sf, cellsize = 50000, what = 'polygons')
gr.25 <- sf::st_make_grid(g.bbox_sf, cellsize = 25000, what = 'polygons')
gr.20 <- sf::st_make_grid(g.bbox_sf, cellsize = 20000, what = 'polygons')
gr.10 <- sf::st_make_grid(g.bbox_sf, cellsize = 10000, what = 'polygons')
gr.5 <- sf::st_make_grid(g.bbox_sf, cellsize = 5000, what = 'polygons')
#gr.1 <- sf::st_make_grid(g.bbox_sf, cellsize = 1000, what = 'polygons')

# ### oli's special 1Km
# merged_section_coord_sf$gr1 <- unlist(sf::st_intersects(merged_section_coord_sf,gr.1))
# sf::st_geometry(merged_section_coord_sf) <- NULL
# merged_section_coord_dt <- data.table::data.table(merged_section_coord_sf)
# merged_section_coord_dt[, id := seq_len(.N), by = gr1]

merged_section_coord_sf$gr5 <- unlist(sf::st_intersects(merged_section_coord_sf,gr.5))
sf::st_geometry(merged_section_coord_sf) <- NULL
merged_section_coord_dt <- data.table::data.table(merged_section_coord_sf)
merged_section_coord_dt[, id := seq_len(.N), by = gr5]

cell_w2 <- unique(merged_section_coord_dt[id>=2,gr5])
keep_id_pergrid <- merged_section_coord_dt[gr5 %in% cell_w2,sample(id,2),by=gr5]
merged_section_2_gr5 <- merged_section_coord_dt[paste(gr5,id,sep='_')%in%paste(keep_id_pergrid$gr5,keep_id_pergrid$V1,sep='_'),][order(gr5),]

cell_w3 <- unique(merged_section_coord_dt[id>=3,gr5])
keep_id_pergrid <- merged_section_coord_dt[gr5 %in% cell_w3,sample(id,3),by=gr5]
merged_section_3_gr5 <- merged_section_coord_dt[paste(gr5,id,sep='_')%in%paste(keep_id_pergrid$gr5,keep_id_pergrid$V1,sep='_'),][order(gr5),]

section_max_abund_gr5_2sect <- section_max_abund[merged_section_id %in% merged_section_2_gr5$merged_section_id,]
data.table::setkey(section_max_abund_gr5_2sect,merged_section_id)
data.table::setkey(merged_section_coord_dt,merged_section_id)
section_max_abund_gr5_2sect <- merge(section_max_abund_gr5_2sect,merged_section_coord_dt,all.x=TRUE)

gr5_sp_assemblage_400m <- section_max_abund_gr5_2sect[,sum(V1),by=.(gr5,species)][order(gr5,species),]
gr5_sp_richness_400m <- gr5_sp_assemblage_400m[,.N,by=gr5]

unique(gr5_sp_assemblage_400m[,species])

data.table::fwrite(gr5_sp_assemblage_400m,'output/gr5_sp_assemblage_400m_effort.csv')

section_max_abund_gr5_3sect <- section_max_abund[merged_section_id %in% merged_section_3_gr5$merged_section_id,]
data.table::setkey(section_max_abund_gr5_3sect,merged_section_id)
data.table::setkey(merged_section_coord_dt,merged_section_id)
section_max_abund_gr5_3sect <- merge(section_max_abund_gr5_3sect,merged_section_coord_dt,all.x=TRUE)

gr5_sp_assemblage_600m <- section_max_abund_gr5_3sect[,sum(V1),by=.(gr5,species)][order(gr5,species),]
gr5_sp_richness_600m <- gr5_sp_assemblage_600m[,.N,by=gr5]

dev.new()
par(mfrow=c(2,1))
hist(gr5_sp_richness_400m[,N],breaks=15)
hist(gr5_sp_richness_600m[,N],breaks=15)

sp_rich_5kmgrid_400m <- sf::st_sf(gr.5[gr5_sp_richness_400m$gr5])
sp_rich_5kmgrid_400m$richness <- gr5_sp_richness_400m$N
sp_rich_5kmgrid_400m$grid5_id  <- gr5_sp_richness_400m$gr5
sp_rich_5kmgrid_600m <- sf::st_sf(gr.5[gr5_sp_richness_600m$gr5])
sp_rich_5kmgrid_600m$richness <- gr5_sp_richness_600m$N

sf::st_write(sp_rich_5kmgrid_400m,'output/secure_5kmgrid.shp', delete_dsn=TRUE)


sp_rich_5kmgrid_400m$gr_200 <- unlist(sf::st_intersects(sf::st_buffer(sp_rich_5kmgrid_400m,-1),gr.200))
sp_rich_5kmgrid_400m$gr_100 <- unlist(sf::st_intersects(sf::st_buffer(sp_rich_5kmgrid_400m,-1),gr.100))
sp_rich_5kmgrid_400m$gr_50 <- unlist(sf::st_intersects(sf::st_buffer(sp_rich_5kmgrid_400m,-1),gr.50))
sp_rich_5kmgrid_400m$gr_20 <- unlist(sf::st_intersects(sf::st_buffer(sp_rich_5kmgrid_400m,-1),gr.20))
sp_rich_5kmgrid_400m$gr_25 <- unlist(sf::st_intersects(sf::st_buffer(sp_rich_5kmgrid_400m,-1),gr.25))
sp_rich_5kmgrid_400m$gr_10 <- unlist(sf::st_intersects(sf::st_buffer(sp_rich_5kmgrid_400m,-1),gr.10))

sp_rich_5kmgrid_400m_df <- sp_rich_5kmgrid_400m
sf::st_geometry(sp_rich_5kmgrid_400m_df) <- NULL
data.table::fwrite(sp_rich_5kmgrid_400m_df,'output/grid5k_scaling.csv')

sp_rich_5kmgrid_400m_dt <- data.table::data.table(sp_rich_5kmgrid_400m_df)


dev.new()
plot(sf::st_buffer(sp_rich_5kmgrid_400m,5000),border=NA,main='400m')
dev.new()
plot(sf::st_buffer(sp_rich_5kmgrid_600m,5000),border=NA,main='600m')

data(Europe, package = 'tmap')
sf_e <- sf::st_as_sf(Europe)
sf_e <- sf::st_transform(sf_e, crs = 3035)
bbox <- raster::extent(1480000,6534652,1241889,5756279)
bbox_sf <- sf::st_set_crs(sf::st_as_sfc(as(bbox, 'SpatialPolygons')), 3035)
sf_e_clip <- sf::st_intersection(sf_e,bbox_sf)

dev.new()
plot(gr.200[c(223:225,238:240)])
plot(sf_e_clip$geometry, graticule = sf::st_crs(4326), axes = TRUE, col='grey90',add=TRUE)
plot(gr.200[c(223:225,238:240)],border='orange',add=TRUE)
plot(gr.50,col=NA,border='grey50',lty=2,add=TRUE)
plot(gr.100,col=NA,border='orange',lty=2,add=TRUE)
plot(gr.200,col=NA,border='red',lwd=2,add=TRUE)
plot(sf::st_buffer(sp_rich_5kmgrid_400m,5000),border=NA)

save.image('output/richness.rda')

load('output/richness.rda')
ls()


## check Finland
fin_5kg_grid <- unique(sp_rich_5kmgrid_400m_dt[gr_200%in%c(223:225,238:240),grid5_id])
fin_sp_assemblages <- gr5_sp_assemblage_400m[gr5%in%fin_5kg_grid,]
fin_sp_assemblages[,.N,by=gr5][order(N),]
fin_sp_assemblages[gr5==366571,][order(species),]
