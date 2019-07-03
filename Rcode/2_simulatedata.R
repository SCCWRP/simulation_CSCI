

simulating_bugs_and_scores <- function(bug_origin, site, gis, it, inc, num_bug, num_bug_start){
  sub_sampling_chosen_site(bug_origin, site, num_bug)
  gis.station <- gis %>%
    filter(StationCode == site)
  nb <- seq(num_bug_start, num_bug, by = inc)
  bug.pick <- sub_sampling_chosen_site(bug_origin, site, num_bug)
  
  res <- foreach(i = 1:length(nb), .packages = c("tidyverse", "CSCI") ) %dopar% {
    core_res <- c()
    mmi_res <- c()
    grp_res <-c()
    omni_res <- c()
    for (j in 1:it){
      if(nb[i] == 500){j = it}
      ind <- sample(1:nrow(bug.pick), nb[i], replace = F)
      bugs <- bug.pick[ind,] %>% 
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
    
    save(res, file = paste0("stations/", site, ".RData"))
    return()
  }
}

