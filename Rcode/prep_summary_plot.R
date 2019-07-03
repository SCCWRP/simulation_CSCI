site_list <- c("SMCR8_277","SMC00476", "SGUR103",  
               "SMC01424", "SMC01384", "801M16861","SMC02984")


simulated_data <- c()

for(i in seq_along(site_list)){
  simulated_data[[i]] <- tidying_simulated_data(site = site_list[i])
}


simulated_data_bind <- bind_rows(simulated_data)


summary_within_station <- simulated_data_bind %>% 
  group_by(StationCode, Count) %>%
  select(StationCode, Count, CSCI, MMI, OoverE) %>%
  summarise_all(.,
                list(~mean(.), 
                     ~sd(.), 
                     ~quantile(., probs = 0.025),
                     ~quantile(., probs = .975))
  )

summary_within_station %>% last()
