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

station_list <- read_csv("data/station_list.csv")

pick <- station_list %>% 
  slice(sample(1:275, 1)) %>%
  select(StationCode)

site <- as.character(pick)

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

library(CSCI)

station_list %>% filter(StationCode == site)

fun <- function(bug, gis, site, it, inc){
  
  library(foreach)
  library(doParallel)
  library(CSCI)
  
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
  site_original <- CSCI(bug.site.clean,gis.station)$core
  
  
  # Expand of bug data
  bug.expand <- bug.site.clean %>% 
    uncount(as.numeric(BAResult)) %>% 
    mutate(BAResult, BAResult= 1)
  
  nc <- detectCores()
  cl<-makeCluster(nc)
  registerDoParallel(cl)
  
  nb <- seq(50, 500, by = inc)
  ind_500 <- sample(1:nrow(bug.expand), 500, replace = F)
  bug_500_pick <- bug.expand[ind_500,]
  
  res1 <- foreach(i = 1:length(nb), .packages = c("tidyverse", "CSCI") ) %dopar% {
    #omni_res <- c()
    core_res <- c()
    mmi_res <- c()
    grp_res <- c()
    for (j in 1:it){
      if (nb[i] == 500){j = it}
      ind <- sample(1:nrow(bug_500_pick), nb[i], replace = F)
      bugs <- bug_500_pick[ind,] %>% 
        count(BAResult, FinalID, LifeStageCode,
              SampleDate, SampleID, Result,
              Unit, distinct, FieldSampleID, StationCode) %>%
        mutate(BAResult = n)
      gis <- gis.station
      omni_res <- CSCI(bugs,gis)
      #omni_res[[j]] <- list(omni$core, omni$Suppl1_mmi, omni$Suppl1_grps)
      core_res[[j]] <- omni_res$core
      mmi_res[[j]] <- omni_res$Suppl1_mmi
      grp_res[[j]] <- omni_res$Suppl1_grps
    }
    list(core_res,
         mmi_res,
         grp_res)
    #omni_res
  }
  
  save(site_original, res, file = "test3.RData")
  save(site_original, res, file = paste0(site, ".RData"))
  return(list(ori = site_original, result = res))
}

# Parameters for the sample function

it = 3
inc = 150

site = "SMC01384" #0.8519
site1 <- fun(bug, gis, site, it, inc)

site = "SMC02984" #0.556
site2 <- fun(bug, gis, site, it, inc)

site = "SMC01424" #.988
site3 <- fun(bug, gis, site, it, inc)

site = "SGUR103" #1.1619
site5 <- fun(bug, gis, site, it, inc)

site = "801M12669" #0.164
site6 <- fun(bug, gis, site, it, inc)

site = "SMC16832"  #0.31
site7 <- fun(bug, gis, site, it, inc)



site2 <- fun(bug, gis, site[2], it, inc)
site3 <- fun(bug, gis, site[3], it, inc)
site4 <- fun(bug, gis, site[4], it, inc)

# NOPE
goo <- c()

for (i in length(site)){
  goo[[i]] <- fun(bug, gis, site[i], it, inc)
}



res_df <- res %>% 
  enframe() %>% 
  unnest() %>% 
  group_by_at(vars(name)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=name, value=value) %>%    # spread
  select(-row_id)  # drop the index

mean.core <- apply(res_df, 2, mean)
CI.core <- apply(res_df, 2, quantile, c(0.025, 0.975))
sd.core <- apply(res_df, 2, sd)

result <- tibble(n = nb, 
                 mean = mean.core, 
                 lower = CI.core[1,], 
                 upper = CI.core[2,],
                 sd. = sd.core)


res_plot <- result %>% 
  ggplot() +
  geom_line(aes(nb,mean), color = "red",
            size = 0.7, linetype = 1) +
  geom_line(aes(nb,lower), color = "blue",
            size = 0.4, linetype = 4) +
  geom_line(aes(nb,upper), color = "blue",
            size = 0.4, linetype = 4) +
  labs(x = "number of bug", y = "CSCI score") +
  ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                    ", core = ", site_core, 
                                                    ", iters = ", it))  res_df <- res %>% 
    enframe() %>% 
    unnest() %>% 
    group_by_at(vars(name)) %>%  # group by everything other than the value column. 
    mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
    spread(key=name, value=value) %>%    # spread
    select(-row_id)  # drop the index


a <- fun(bug, gis, "1368546", 2, 1000)


# Test
goo <- c()

for (i in length(site)){
  goo[[i]] <- fun(bug, gis, site[i])
}

