library(dbplyr)
library(dplyr)
library(RPostgreSQL)
library(tidyverse)
library(PHABMetrics)
library(PHAB)

# setup connection
con <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  dbname = 'smc', 
  host = '192.168.1.17', 
  user = rstudioapi::askForPassword('Database user'), 
  password = rstudioapi::askForPassword("Database password")
)

# raw connections
datmetcon <- tbl(con, 'tmp_phabmetrics')
datrawcon <- tbl(con, 'tmp_phab')
datgiscon <- tbl(con, 'tblgismetrics')
datbugcon <- tbl(con, 'tbl_taxonomyresults')
CSCIcore <- tbl(con, 'csci_core')

# Pick only the chosen stations ------------------------------------
datbugcon <- tbl(con, 'tbl_taxonomyresults') %>% 
  filter(stationcode %in% c("SMC00476", "SGUR103", "SMC01424", "SMC01384", "801M16861", "SMC02984")) %>% 
  group_by(stationcode) %>%
  filter(sampledate == max(sampledate)) %>%
  # filter(fieldreplicate == max(fieldreplicate)) %>% 
  collect()


# metadata ----------------------------------------------------------------

load(system.file("metadata.rdata", package="BMIMetrics"))

### 
# 
# core <- as_tibble(CSCIcore)
# 
# pick <- core %>% 
#   filter(csci > 1.1, count > 500, 
#          pcnt_ambiguous_individuals < 0.1) %>%
#   slice(n()) %>%
#   select(stationcode)
# 


gis <- as_tibble(datgiscon)  %>%
  select("stationcode", "area_sqkm", "bdh_ave", 
         "elev_range", "kfct_ave", "p_mean", 
         "new_lat", "new_long", "ppt_00_09", 
         "site_elev", "sumave_p", "temp_00_09")

colnames(gis) <- c("StationCode", "AREA_SQKM", "BDH_AVE",
                           "ELEV_RANGE", "KFCT_AVE", "P_MEAN",
                           "New_lat", "New_Long", "PPT_00_09",
                           "SITE_ELEV", "SumAve_P", "TEMP_00_09")

# Bug data
bug <- as_tibble(datbugcon) %>% 
  select("stationcode", "sampledate", "fieldreplicate", 
         "fieldsampleid","finalid", "lifestagecode", 
         "baresult", "result", "unit", 
         "distinctcode") %>%
  mutate(
    baresult = as.numeric(baresult)
  )


colnames(bug) <- c("StationCode","SampleDate","SampleID", 
                        "FieldSampleID", "FinalID", "LifeStageCode", 
                        "BAResult", "Result", "Unit", 
                        "distinct")

#site <- as.character(pick 1)
site_list <- c("SMCR8_277","SMC00476", "SGUR103",  
                       "SMC01424", "SMC01384", "801M16861","SMC02984")


# -------------------------------------------------------------------------

# Pick bug site and gis based on sationcode
bug.site <- bug %>% 
  filter(StationCode == site[1]) %>%
  filter(SampleDate == max(SampleDate)) %>%
  filter(SampleID == max(SampleID))

gis.station <- gis %>% 
  filter(StationCode == site[1])

bug.site.new <- CSCI::cleanData(bug.site, purge = TRUE, msgs = FALSE)


library(CSCI)
site_core = CSCI(bug.site.new,gis.station)$core[['Pcnt_Ambiguous_Individuals']]

# picking stations
common <-  semi_join(bug, gis)
site_list <- common %>% select(StationCode) %>% unique()
pot <- read_excel("bugsample.xlsx") %>%
  select(-SampleID)
another_list <- semi_join(common, pot)