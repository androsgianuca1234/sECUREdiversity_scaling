
#sessionInfo() 

#devtools::install_github("RetoSchmucki/rbms", force=TRUE)
library(rbms)
library(data.table)
library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(maps)
library(grid)
library(plyr)
library(gtable)
library(DBI)
library(magrittr)
library(units)
library(scales)
library(rlang)
library(lazyeval)
library(tools)
library(udunits2)
library(munsell)
library(compiler)
library(colorspace)
library(tibble)
#devtools::install_github("hadley/scales", force = T)
#devtools::install_github("tidyverse/ggplot2", force = T)
#library(ggplot2)




# ?geom_sf
# 
# ?geom_sf
# 
# packageVersion(ggplot)
# 
# install.packages('ggplot2-master.zip', lib = 'C:/filepath')
# install.packages('ggplot2-master.zip', lib = 'C:/filepath')
# install.packages('ggplot2', repo=NULL, lib = 'C:/filepath')
# 
# devtools::install_github("tidyverse/ggplot2", force=TRUE)
# 
# packageVersion('ggplot2') 
# devtools::install_github("tidyverse/ggplot2", force=TRUE)
# devtools::install_github("tidyverse/ggplot2")
# require(ggplot2)







#devtools::install_github("tidyverse/ggplot2")

## Compute count estimate for BMS data
## load regional gam functions
## setwd('/Users/androsgianuca/Dropbox/sECURE_collaboration_Reto/biodiversity_scaling')
## source('new_functionsRegionalGAM.r')

## load UK BMS data
## setwd("/Users/androsgianuca/Documents/eBMS UK Data for Reto")

## setwd("C:/Users/RETOSCHM/Dropbox/sECURE_collaboration_Reto/ebms_data_release_sECURE")
setwd('/Users/androsgianuca/Dropbox/sECURE_collaboration_Reto/biodiversity_scaling')

## load and truncate count data
ebms_count_data  <-  readRDS("/Users/androsgianuca/Dropbox/sECURE_collaboration_Reto/ebms_data_release_sECURE/ebms_count.rds")
ebms_count_data  <- ebms_count_data[, COUNT := sum(butterfly_count), by = .(visit_id, species_name)]
ebms_count_data_trunc <- unique(ebms_count_data[year >= 2011, c("visit_id", "transect_id", "species_name", "year", "month", "day", "COUNT")])

ebms_count <- data.table::data.table(SITE_ID = ebms_count_data_trunc$transect_id,
                                     SPECIES = ebms_count_data_trunc$species_name,
                                     DAY = ebms_count_data_trunc$day,
                                     MONTH = ebms_count_data_trunc$month, 
                                     YEAR = ebms_count_data_trunc$year,
                                     COUNT = ebms_count_data_trunc$COUNT)

ebms_count[, DATE := as.Date(paste(YEAR, MONTH, DAY, sep = "-"))]

## load and truncate visit data
ebms_visit_data <- readRDS("/Users/androsgianuca/Dropbox/sECURE_collaboration_Reto/ebms_data_release_sECURE/ebms_visit.rds")
ebms_visit_data_trunc <- ebms_visit_data[year >= 2011, ]
ebms_visit <- data.table::data.table(SITE_ID = ebms_visit_data_trunc$transect_id, 
                                     DATE=as.Date(paste(ebms_visit_data_trunc$year,
                                                        ebms_visit_data_trunc$month,
                                                        ebms_visit_data_trunc$day,
                                                        sep = "-")))

## load and transect coordinate data
ebms_sites_data <- readRDS("/Users/androsgianuca/Dropbox/sECURE_collaboration_Reto/ebms_data_release_sECURE/ebms_transect_geo_details.rds")
ebms_sites_data = as.data.table(ebms_sites_data)
data.table::setnames(ebms_sites_data,'transect_id','SITE_ID')
transect_point_ebms_sf <- sf::st_as_sf(ebms_sites_data[!is.na(longitude_3035)&!is.na(latitude_3035),], 
                                       coords = c("longitude_3035", "latitude_3035"), 
                                       crs = 3035, 
                                       agr = "constant")


?geom_sf
## visualize bms data
## get and build EUROPE MAP
if(!"world" %in% ls()) {
  world <- rnaturalearth::ne_download(scale = 50, type = 'countries', category = 'cultural')
}
world <- sf::st_as_sf(world)
europe <- sf::st_as_sf(dplyr::filter(world, REGION_UN=="Europe" & NAME_LONG!='Russian Federation'))

europe.bbox <- sf::st_polygon(list( matrix(c(-13, 32, 48, 32, 48, 74, -13, 74, -13, 32),
                                           byrow = TRUE,
                                           ncol = 2)))

europe.clipped <- suppressWarnings(sf::st_intersection(europe, sf::st_sfc(europe.bbox, crs=sf::st_crs(europe))))
europe.clipped <- sf::st_transform(europe.clipped,crs=3035)
eu <- ggplot2::geom_sf(data=europe.clipped, alpha=0 ,col='grey50')

transect_point_ebms.clipped <- sf::st_intersection(transect_point_ebms_sf,sf::st_transform(sf::st_sfc(europe.bbox, crs=sf::st_crs(europe)),crs=3035))

transect_point_ebms <- data.table::data.table(SITE_ID = transect_point_ebms.clipped$SITE_ID,
                                              transect_length_m = transect_point_ebms.clipped$transect_length_m,
                                              nbr_sections = transect_point_ebms.clipped$nbr_sections,
                                              bioclimatic_region = transect_point_ebms.clipped$bioclimatic_region,
                                              bioclimatic_code = transect_point_ebms.clipped$bioclimatic_code,
                                              highres_bioclimatic_code = transect_point_ebms.clipped$highres_bioclimatic_code,
                                              geometry = transect_point_ebms.clipped$geometry)

ggplot(transect_point_ebms.clipped) +
  geom_sf(aes(color = bioclimatic_region, fill=bioclimatic_region)) + eu 


## Compute abundance indices within one selected region - NOTE: I limited the j loop to 1 region and the sp loop to 5 species to speed up the example

sp_avg_abun_site_region <- data.frame()

(region_list <- unique(transect_point_ebms$bioclimatic_region))

for(j in region_list[c(1)]){
  
  cat(paste(j,'\n'))
  
  region_ebms_visit <- ebms_visit[SITE_ID %in% transect_point_ebms[bioclimatic_region == j, SITE_ID],]
  region_ebms_count <- ebms_count[SITE_ID %in% transect_point_ebms[bioclimatic_region == j, SITE_ID],]
  
  ts_date <- ts_dwmy_table(InitYear = 2011, LastYear = 2016, WeekDay1 = 'monday')
  
  ts_season <- ts_monit_season(ts_date, StartMonth = 4, EndMonth = 9, StartDay = 1, EndDay = NULL, 
                               CompltSeason = TRUE, Anchor = TRUE, AnchorLength = 7, AnchorLag = 7)
  
  ts_season_visit <- ts_monit_site(region_ebms_visit,ts_season)  
  
  (sp_list <- region_ebms_count[order(SPECIES),unique(SPECIES)])
  
  if(length(sp_list) < 1) {
    next()
  }
  
  sp_avg_abun_site_1 <- data.frame()
  
  for (sp in sp_list[c(1:20)]){
    
    cat(paste(sp,'\n'))
    
    ts_season_count <- ts_monit_count_site(ts_season_visit, region_ebms_count, sp = sp)
    
    ts_flight_curve <- flight_curve(ts_season_count, NbrSample = 200, MinVisit = 5, MinOccur = 3, 
                                    MinNbrSite = 3, MaxTrial = 3, FcMethod = 'regionalGAM', 
                                    GamFamily = 'quasipoisson', CompltSeason = TRUE, 
                                    KeepModel = TRUE)
    
    site_year_sp_count <-  impute_count(ts_season_count, ts_flight_curve, SpeedGlm = FALSE, 
                                        FamilyGlm = 'quasipoisson', KeepModel = T)
    
    sp_avg_abun_site <- site_year_sp_count[, sum(COUNT_IMPUTED), .(SITE_ID, M_YEAR)] [,mean(V1), .(SITE_ID)]
    sp_avg_abun_site <- sp_avg_abun_site[, species := sp]
    data.table::setnames(sp_avg_abun_site, "V1", "avg_abundance")
    
    sp_avg_abun_site_1 <- rbind(sp_avg_abun_site_1,sp_avg_abun_site)
    
  } ## loop sp
  
  sp_avg_abun_site_1$region <- j
  sp_avg_abun_site_region <- rbind(sp_avg_abun_site_region,sp_avg_abun_site_1)
  
} ##loop j


## map your average abundance for this region
##============================================
abun_site <- merge(transect_point_ebms.clipped,sp_avg_abun_site_region, by='SITE_ID')

de <- geom_sf(data=europe.clipped[europe.clipped$ISO_A3=='DEU',], alpha=0 ,col='grey50')

ggplot(abun_site[abun_site$species=="Aporia iris",]) +
  geom_sf(aes(color = avg_abundance)) +
  scale_colour_gradient2(low = "blue", mid = "yellow" , high = "red", midpoint=150) + 
  de


## map your flight curves, only for the last species
## to retrieve all flight curve, we should store them in a new object
##=========================
plot(ts_flight_curve[M_YEAR==2011, trimDAYNO], ts_flight_curve[M_YEAR==2011, NM], 
     type = 'l', 
     ylim = c(0, max(ts_flight_curve[, NM],na.rm = TRUE)),
     xlab = 'Monitoring Year Day',
     ylab = 'Relative Abundance',
     main = paste('fligth curve for', sp,'in region',j))
c <- 2
for(y in 2012:2016){
  points(ts_flight_curve[M_YEAR==y, trimDAYNO],
         ts_flight_curve[M_YEAR==y, NM],
         type = 'l',
         col = c)
  
  c <- c + 1
}
legend('topright', legend = c(2011:2016), col = c(seq_along(c(2011:2016))), lty=1, bty='n')