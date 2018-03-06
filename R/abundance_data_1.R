library(data.table)
library(sf)
library(RPostgreSQL)


## Compute count estimate for BMS data
## load regional gam functions
setwd('/Users/androsgianuca/Dropbox/sECURE_collaboration_Reto/biodiversity_scaling')
source('new_functionsRegionalGAM.r')

## load UK BMS data
setwd("/Users/androsgianuca/Documents")

## load and truncate count data
uk_count_data <- data.table::fread("eBMS UK Data for Reto/UK Counts Table.txt")
uk_count_data_trunc <- uk_count_data[YEAR>=2011,c("SITENO","SECTION","SPECIES","VISITDATE","YEAR","MONTH","DAY","COUNT")]
uk_count <- data.table::data.table(SITE_ID=uk_count_data_trunc$SITENO,
                                    SPECIES=uk_count_data_trunc$SPECIES,
                                    DAY=uk_count_data_trunc$DAY,
                                    MONTH=uk_count_data_trunc$MONTH, 
                                    YEAR=uk_count_data_trunc$YEAR,
                                    COUNT=uk_count_data_trunc$COUNT)

uk_count[,DATE:=as.Date(paste(YEAR,MONTH,DAY,sep = "-"))]    


## load and truncate visit data
uk_visit_data <- data.table::fread("eBMS UK Data for Reto/UK Visits Table2.csv")
uk_visit_data_trunc <- uk_visit_data[YEAR>=2011,]
uk_visit <- data.table::data.table(SITE_ID=uk_visit_data_trunc$SITENO, DATE=as.Date(paste(uk_visit_data_trunc$YEAR,uk_visit_data_trunc$MONTH,uk_visit_data_trunc$DAY,sep = "-")))
            
## load and transect coordinate data
uk_sites_data <- data.table::fread("eBMS UK Data for Reto/UK Sites Table2.csv")
transect_point <- sf::st_as_sf(uk_sites_data[!is.na(LONGITUDE)&!is.na(LATITUDE),], coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")
       plot(transect_point)             

### FILTER PER BIOGEOGRAPHIC REGION
dbcon <- dbConnect(dbDriver("PostgreSQL"), dbname = "spatial_db",
                 host = "localhost", port = 5432,
                 user = "postgres")

sf::st_write_db(dbcon, transect_point, table = "uk_transect_points")

transect_bioclim_point = sf::st_read_db(dbcon, query = "SELECT
                                        \"SITENO\",
                                           \"LENGTH\",
                                              b.genzv2,
b.genzname,
b.genzv2_seq,
b.genzv2,
a.wkb_geometry
FROM
public.UK_transect_points as a,
climate.bioclimatic_eu as b
WHERE
st_intersects(a.wkb_geometry, b.wkb_geometry)")


transect_bioclim_point

plot(transect_bioclim_point$wkb_geometry
     ,col=colorspace::rainbow_hcl(length(unique(as.factor(transect_bioclim_point$genzv2))))[as.factor(transect_bioclim_point$genzv2)])

####Computing abundance indices per region

sp_avg_abun_site_region <- data.frame()

region_list <- unique(transect_bioclim_point$genzname)

for(j in region_list[-1]){

cat(paste(j,'\n'))
    
db_transect_bioclim_point = data.table::data.table(transect_bioclim_point)

tr_list = db_transect_bioclim_point[genzname== j, SITENO]

region_uk_count = uk_count[SITE_ID %in% tr_list,]
region_uk_visit = uk_visit[SITE_ID %in% tr_list,]

ts_date <- ts_dwmy_table(InitYear=2011,LastYear=2016,WeekDay1='monday')

ts_season <- ts_monit_season(ts_date,StartMonth=4,EndMonth=9,StartDay=1,EndDay=NULL,CompltSeason=TRUE,Anchor=TRUE,AnchorLength=7,AnchorLag=7)

region_uk_visit <- df_visit_season(region_uk_visit,ts_season)

ts_season_visit <- ts_monit_site(region_uk_visit,ts_season)


sp_list <- region_uk_count[order(SPECIES),unique(SPECIES)]

if(length(sp_list)<1) {next}

sp_avg_abun_site_1 <-data.frame()

for (i in sp_list[1:2]){ #remove 1:2 to go across all species
  
ts_season_count <- ts_monit_count_site(ts_season_visit,region_uk_count,sp=i)

ts_flight_curve <- flight_curve(ts_season_count,NbrSample=100,MinVisit=3,MinOccur=2,MinNbrSite=1,MaxTrial=3,FcMethod='regionalGAM',GamFamily='poisson',CompltSeason=TRUE)

# plot(ts_flight_curve[M_YEAR==2015,trimDAYNO],ts_flight_curve[M_YEAR==2015,NM],type='l',xlab='Monitoring Year Day',ylab='Relative Abundance')
# c <- 2
# for(y in 2015:2016){
#   points(ts_flight_curve[M_YEAR==y,trimDAYNO],ts_flight_curve[M_YEAR==y,NM],type='l',col=c)
#   c <- c + 1
# }

site_year_sp_count <- impute_count(ts_season_count,ts_flight_curve)

sp_avg_abun_site = site_year_sp_count[,sum(COUNT_IMPUTED), .(SITE_ID, M_YEAR)] [,mean(V1),.(SITE_ID)]
sp_avg_abun_site = sp_avg_abun_site[,species:=i]
data.table::setnames(sp_avg_abun_site, "V1", "avg_abundance")


sp_avg_abun_site_1 <- rbind(sp_avg_abun_site_1,sp_avg_abun_site)

}

sp_avg_abun_site_1$region <- j
sp_avg_abun_site_region <- rbind(sp_avg_abun_site_region,sp_avg_abun_site_1)

}


