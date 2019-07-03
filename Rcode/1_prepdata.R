## Cleaning up the input data
prep_bug_data <- function(bug_origin, site){
  bug.site <- bug_origin %>%
    filter(StationCode == site) %>%
    filter(SampleDate == max(SampleDate)) %>%
    filter(SampleID == max(SampleID))
  bug.site.clean <- CSCI::cleanData(bug.site, purge = T, message = F)
  return(bug.site.clean)
}




sub_sampling_chosen_site <- function(bug_origin, site, num_bug){
  clean_site <- prep_bug_data(bug_origin, site)
  bug.expand <- clean_site %>%
    uncount(as.numeric(BAResult)) %>%
    mutate_at("BAResult", 1)
  ind <- sample(1:nrow(bug.expand), num_bug, replace = F)
  bug.pick <- bug.expand[ind,]
  return(bug.pick)
}



