library(dbplyr)
library(dplyr)
library(RPostgreSQL)
library(tidyverse)
library(PHABMetrics)
library(PHAB)

library(foreach)
library(doParallel)
library(CSCI)


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

station_list <- read_csv("data/station_list.csv")

# pick <- station_list %>% 
#   slice(sample(1:275, 1)) %>%
#   select(StationCode)
# 
# site <- as.character(pick)

# Site data
gis <- as_tibble(datgiscon)  %>%
  select("stationcode", "area_sqkm", "bdh_ave", 
         "elev_range", "kfct_ave", "p_mean", 
         "new_lat", "new_long", "ppt_00_09", 
         "site_elev", "sumave_p", "temp_00_09")
# Bug data
bug <- as_tibble(datbugcon) %>% 
  select("stationcode", "sampledate", "fieldreplicate", 
         "fieldsampleid","finalid", "lifestagecode", 
         "baresult", "result", "unit", 
         "distinctcode")%>%
  mutate(
    baresult = as.numeric(baresult)
  )

# Fix names
colnames(bug) <- c("StationCode","SampleDate","SampleID", 
                   "FieldSampleID", "FinalID", "LifeStageCode", 
                   "BAResult", "Result", "Unit", 
                   "distinct")

colnames(gis) <- c("StationCode", "AREA_SQKM", "BDH_AVE",
                   "ELEV_RANGE", "KFCT_AVE", "P_MEAN",
                   "New_lat", "New_Long", "PPT_00_09",
                   "SITE_ELEV", "SumAve_P", "TEMP_00_09")

# bug and gis datas are provided
# pick a site in the list of site provided or enter a string with quotation marks "sitecode"
# it = number of iterations in resampling
# inc = number of increament (every 20, 30, 40, ect.)


fun <- function(bug, gis, site, it, inc){

  
  bug.site <- bug  %>%
    filter(StationCode == site) %>%
    filter(SampleDate == max(SampleDate)) %>%
    filter(SampleID == max(SampleID))
  
  gis.station <- gis %>% 
    filter(StationCode == site) 
  
  if(nrow(bug.site) == 0 | nrow(gis.station) == 0) 
  {return("NOPE! Not this site")}
  
  bug.site.clean <- CSCI::cleanData(bug.site, purge = TRUE, msgs = FALSE)
  
  # Expand of bug data
  bug.expand <- bug.site.clean %>% 
    uncount(as.numeric(BAResult)) %>% 
    mutate(BAResult, BAResult= 1)
  
  # Site record
  site_original <- CSCI(bug.site.clean,gis.station)
  
  # Expand of bug data
  bug.expand <- bug.site.clean %>% 
    uncount(as.numeric(BAResult)) %>% 
    mutate(BAResult, BAResult= 1)
  
  nc <- detectCores()
  cl<-makeCluster(nc-1)
  registerDoParallel(cl)
  
  nb <- seq(50, 500, by = inc)
  ind_500 <- sample(1:nrow(bug.expand), 500, replace = F)
  bug_500_pick <- bug.expand[ind_500,]
  
  
  strt <- Sys.time()
  res <- foreach(i = 1:length(nb), .packages = c("tidyverse", "CSCI") ) %dopar% {
    core_res <- c()
    mmi_res <- c()
    grp_res <-c()
    omni_res <- c()
    for (j in 1:it){
      if(nb[i] == 500){j = it}
      ind <- sample(1:nrow(bug_500_pick), nb[i], replace = F)
      bugs <- bug_500_pick[ind,] %>% 
        count(BAResult, FinalID, LifeStageCode,
              SampleDate, SampleID, Result,
              Unit, distinct, FieldSampleID, StationCode) %>%
        mutate(BAResult = n)
      gis <- gis.station
      omni <- CSCI(bugs,gis)
      omni_res[[j]] <- omni
      core_res[[j]] <- omni$core
      mmi_res[[j]] <- omni$Suppl1_mmi
      grp_res[[j]] <- omni$Suppl1_grps
    }
    list(core_res,
         mmi_res,
         grp_res,
         omni_res)
  }
  
  t = print(Sys.time()-strt)
  
  save(site_original, res, file = paste0("data/", site, ".RData"))
  return(t)
}

# Parameters for the sample function

it = 100
inc = 10


site <- c("SMCR8_277","SMC00476", "SGUR103",  "SMC01424", "SMC01384", 
          "801M16861","SMC02984", "SMC16832", "801M12669","412CE0732")

time <- c()

for (i in 2:length(site)){
  time[i] <- fun(bug, gis, site[i], it, inc)
}



