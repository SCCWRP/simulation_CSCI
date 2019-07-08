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
  gather('var', 'val', -StationCode, -Count) %>% 
  separate(var, c('index', 'measure'), sep = '_') %>% 
  group_by(StationCode, index, measure) %>%
  mutate(
    chng = val / max(val, na.rm = T)
  ) %>%
  ungroup %>% 
  mutate(Count = 500 - Count)
View(test)


test %>% 
  ggplot(aes(color = StationCode, group = StationCode)) +
  geom_line(aes(x = Count, y = chng)) +
  facet_grid(index ~ measure)


#relative difference

test2 <- summary_within_station %>% 
  gather('var', 'val', -StationCode, -Count) %>% 
  separate(var, c('index', 'measure'), sep = '_') %>% 
  group_by(StationCode, index, measure) %>%
  mutate(
    val = replace_na(val,min(val, na.rm = T)),
    chng = abs(val - last(val)) /max(val,last(val))
  ) %>%
  ungroup %>% 
  mutate(Count = 500 - Count)

test2 %>% 
  ggplot(aes(color = StationCode, group = StationCode)) +
  geom_line(aes(x = Count, y = chng)) +
  facet_grid(index ~ measure)


readr::write_rds(summary)

