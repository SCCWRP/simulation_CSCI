# tidy, summarize ambig data ----------------------------------------------

tidying_ambig_simulated_data <- function(site){
  load_dir <- paste0("stations/", site, "-ambig.RData")
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


summarising_ambig_tidied_data <- function(site){
  omni_data <- tidying_ambig_simulated_data(site)
  
  summary_data <- omni_data %>%
    group_by(Pcnt_Replaced) %>%
    select(Pcnt_Replaced, Count, CSCI, Pcnt_Ambiguous_Individuals, Pcnt_Ambiguous_Taxa) %>%
    summarise_all(.,
                  list(~mean(.), 
                       ~sd(.), 
                       ~quantile(., probs = 0.025),
                       ~quantile(., probs = 0.975))
    ) %>% 
    mutate(StationCode = site)
  return(summary_data)
}

site_list <- c("SMCR8_277","SMC00476", "SGUR103",  
               "SMC01424", "SMC01384", "801M16861","SMC02984")
data_ready <- c()
for(i in seq_along(site_list)){
  data_ready[[i]] <- summarising_ambig_tidied_data(site_list[i])
}

