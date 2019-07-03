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
                     ~sd(.)
  ))

test <- summary_within_station %>% 
  group_by(StationCode) %>%
  arrange(StationCode, desc(Count)) %>%
  mutate(
    change_CSCI = abs(CSCI_mean-first(CSCI_mean))/first(CSCI_mean),
    change_MMI = abs(MMI_mean-first(MMI_mean))/first(MMI_mean),
    change_OE = abs(OoverE_mean-first(OoverE_mean))/first(OoverE_mean),
    reduction_bug = (500 - Count)
  ) %>%
  gather(.,change_type, rel.diff, c(change_CSCI, change_MMI, change_OE)) %>%
  gather(., mean_group, mean, c(contains("_mean"))) %>%
  gather(., sd_group, stdv, c(contains("_sd")))

View(test)


test %>% 
  filter(StationCode == "SMC00476") %>%
  ggplot() +
  geom_line(aes(x = reduction_bug, y = rel.diff)) +
  geom_line(aes(x = reduction_bug, y = stdv)) +
  facet_grid(change_type ~ sd_group)

