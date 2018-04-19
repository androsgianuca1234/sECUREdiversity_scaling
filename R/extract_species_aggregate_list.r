R --vanilla

set.seed(1234)
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
                    s.monitoring_type = \'2\' ;'

section_count_dt <- data.table::data.table(dbGetQuery(dbcon,sectioncountQ))

data.table::fwrite(data.frame(species_id = unique(section_count_dt[,species])),'output/species_aggregate_list.csv')
