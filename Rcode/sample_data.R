
library(foreach)
library(doParallel)


# Expand of bug data
bug.expand <- bug.site.new %>% 
  uncount(as.numeric(BAResult)) %>% 
  mutate(BAResult, BAResult= 1)

nc <- detectCores()
cl<-makeCluster(nc-1)
registerDoParallel(cl)



# n.bug = nb
# function for sampling method (w/ or wt/ replacement)
# med <- function(nb, bug.expand){
#   if (nb < nrow(bug.expand)) {return(F)}
#   else {return(T)}
# }


nb <- seq(50,500,by = 300)
it = 3

strt <- Sys.time()

res <- foreach(i = 1:length(nb), .packages = c("tidyverse", "CSCI") ) %dopar% {
  core <- c()
  for (j in 1:it){
    ind <- sample(1:nrow(bug.expand), nb[i], replace = F)
    bugs <- bug.expand[ind,] %>% 
      count(BAResult, FinalID, LifeStageCode,
            SampleDate, SampleID, Result,
            Unit, distinct, FieldSampleID, StationCode) %>%
      mutate(BAResult = n)
    gis <- gis.station
    core[[j]] <- CSCI(bugs,gis)$core
  }
  core
}



Sys.time() - strt


res_df <- res %>% 
  enframe() %>% 
  unnest() %>% 
  group_by_at(vars(name)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=name, value=value) %>%    # spread
  select(-row_id)  # drop the index


res %>% enframe() %>% unnest() %>% group_by(name)

mean.core <- apply(res_df, 2, mean)
CI.core <- apply(res_df, 2, quantile, c(0.025, 0.975))
sd.core <- apply(res_df, 2, sd)

result <- tibble(n = nb, 
                 mean = mean.core, 
                 lower = CI.core[1,], 
                 upper = CI.core[2,],
                 sd. = sd.core)


write.csv(res_df, file = "data/bad1_iter.csv")


CSCI_iter <- c()

for (i in 1:2){
  CSCI_iter[[i]] <- res[[i]] %>% 
    enframe() %>% 
    unnest()
}







