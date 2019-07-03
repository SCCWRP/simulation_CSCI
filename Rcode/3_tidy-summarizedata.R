

tidying_simulated_data <- function(site){
  load_dir <- paste0("stations/", site, ".RData")
  load(load_dir)
  res1 <- res %>% enframe()
  result <- c()
  
  for (i in 1:nrow(res1)){
    result[[i]]<- res[[i]] %>%
      enframe() %>%
      filter(name != 4) %>%
      deframe()
  }
  
  omni_data <- result %>% 
    modify_depth(.depth = 2, .f = ~as_tibble(bind_rows(.))) %>%
    modify_depth(.depth = 1, .f = ~as_tibble(bind_cols(.))) %>%
    bind_rows() %>%
    select(-c(StationCode1, StationCode2, SampleID1))
  
  return(omni_data)
}




summarising_tidied_data <- function(site){
  omni_data <- tidying_simulated_data(site)
  
  summary_data <- omni_data %>%
    group_by(Count) %>%
    select(Count, MMI, CSCI, OoverE) %>%
    summarise_all(.,
                  list(~mean(.), 
                       ~sd(.), 
                       ~quantile(., probs = 0.025),
                       ~quantile(., probs = .975))
    )
  return(summary_data)
}

