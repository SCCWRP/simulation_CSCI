library(foreach)
library(doParallel)


# set up for parallel processing ------------------------------------------

nc <- detectCores()
cl<-makeCluster(nc-1)
registerDoParallel(cl)


# select columns in metadata ----------------------------------------------

metadata_select <- metadata %>% 
  select(FinalID, Order, LifeStageCode) %>% 
  mutate(LifeStageCode = as.character(LifeStageCode))


## Subsample only 500 bugs----------------------------------------------------
sub_sampling_chosen_site <- function(bug_origin, site, num_bug, metadata_select){
  set.seed(200)
  bug.site <- bug_origin %>%
    filter(StationCode == site) %>%
    filter(SampleDate == max(SampleDate)) %>%
    filter(SampleID == max(SampleID)) %>% 
    left_join(metadata_select, by = c('FinalID', 'LifeStageCode'))
  bug.expand <- bug.site %>%
    uncount(as.numeric(BAResult)) %>%
    mutate(BAResult = 1)
  ind <- sample(1:nrow(bug.expand), num_bug, replace = F)
  bug.pick <- bug.expand[ind,]
  return(bug.pick)
}



# simultate bugs --------------------------------------------------------------

simulating_ambig_bugs_and_scores <- function(bug_origin, site, gis, it, inc, num_bug, perc_start, metadata_select){
  gis.station <- gis %>%
    filter(StationCode == site)
  pb <- seq(perc_start, .9, by = inc)
  a <- sub_sampling_chosen_site(bug_origin, site, num_bug, metadata_select)
  res <- foreach(i = 1:length(pb), .packages = c("tidyverse", "CSCI") ) %dopar% {
    core_res <- c()
    mmi_res <- c()
    grp_res <-c()
    omni_res <- c()
    for (j in 1:it){
      bug.pick <- a
      ind <- sample(1:nrow(bug.pick), pb[i]*nrow(bug.pick), replace = F)
      bug.pick[ind,]$FinalID = bug.pick[ind,]$Order
      bugs <- bug.pick %>% 
        count(BAResult, FinalID, LifeStageCode,
              SampleDate, SampleID, Result,
              Unit, distinct, FieldSampleID, StationCode) %>%
        mutate(BAResult = n)
      gis <- gis.station
      bugs <- cleanData(bugs, T, F)
      omni <- CSCI(bugs,gis)
      omni$core
      omni_res[[j]] <- omni
      core_res[[j]] <- omni$core %>% 
        mutate(Pcnt_Replaced = pb[i])
      mmi_res[[j]] <- omni$Suppl1_mmi
      grp_res[[j]] <- omni$Suppl1_grps
    }
    list(core_res,
         mmi_res,
         grp_res,
         omni_res)
  }
  save(res, file = paste0("stations/", site, "-ambig.RData"))
  return()
}


for(i in seq_along(site_list)){
  site = site_list[i]
  simulating_ambig_bugs_and_scores(bug_origin = bug, site, gis, 100, 0.1, 500, .1, metadata_select)
}
