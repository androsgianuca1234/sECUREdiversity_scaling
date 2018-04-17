## Extract eBMS data for the sECURE project
## DATE: 16/04/2018
## AUTHOR: Reto Schmucki retoschm@ceh.ac.uk
##
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

bcountQ <- 'SELECT DISTINCT
                    EXTRACT(YEAR from v.visit_date) as year,
                    c.site_id,
                    c.species_id,
                    c.butterfly_count,
                    v.bms_id,
                    s.transect_length
                  FROM
                    ebms.b_count as c
                    LEFT JOIN ebms.m_visit as v ON c.visit_id = v.visit_id
                    LEFT JOIN ebms.m_site as s ON c.site_id = s.site_id
                    LEFT JOIN ebms.b_species_id as sp ON c.species_id = sp.species_id
                  WHERE
                    EXTRACT(YEAR from v.visit_date) >= 1976 AND
                    s.monitoring_type = \'2\' AND
                    sp.aggregate = FALSE
                  ORDER BY
                    year,
                    bms_id;'

bcount_dt <- data.table::data.table(dbGetQuery(dbcon,bcountQ))
bcount_dt
bcount_dt[,sum(butterfly_count)]
length(unique(bcount_dt[,species_id]))
bcount_dt[,sum(transect_length, na.rm = TRUE)]/1000


sectioncountQ <- 'SELECT DISTINCT
                    v.visit_date as date,
                    s.transect_id as transect_id,
                    s.section_id as section_id,
                    sp.species_acpt_sci_name as species,
                    c.butterfly_count as count
                  FROM
                    ebms.b_count as c
                    LEFT JOIN ebms.m_visit as v ON c.visit_id = v.visit_id
                    LEFT JOIN ebms.m_site as s ON c.site_id = s.site_id
                    LEFT JOIN ebms.b_species_id as sp ON c.species_id = sp.species_id
                  WHERE
                    EXTRACT(YEAR from v.visit_date) >= 2010 AND
                    s.monitoring_type = \'2\' AND
                    sp.aggregate = FALSE;'

section_count_dt <- data.table::data.table(dbGetQuery(dbcon,sectioncountQ))

section_count_dt[count!=0,][1:100]
