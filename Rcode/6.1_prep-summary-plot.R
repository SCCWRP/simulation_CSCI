site_list <- c("SMCR8_277","SMC00476", "SGUR103",  
               "SMC01424", "SMC01384", "801M16861","SMC02984")

station_list <- read_csv("data/station_list.csv")

model_data_mean <- c()

for(i in seq_along(site_list)){
  model_data_mean[[i]] <- summaried_data_with_models_to_csci_mean(site = site_list[i], station_list)
}

mean_bind <- model_data_mean %>% 
  modify_depth(.depth = 1, "summ") %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(
    CSCI_mean = Expo
  ) %>%
  select(-c(Expo, Sphere))


summary_within_station <- mean_bind %>% 
  group_by(StationCode, Count) %>%
  mutate(
    OoverE_sd = replace_na(OoverE_sd, 0),
    CSCI_sd = replace_na(CSCI_sd, 0),
    MMI_sd = replace_na(MMI_sd, 0),

    OoverE_cv = OoverE_sd/OoverE_mean,
    CSCI_cv = CSCI_sd/CSCI_mean,
    MMI_cv = MMI_sd/MMI_mean
  ) %>% 
  select(-contains("_sd")) %>% 
  select(-contains("quantile"))

test <- summary_within_station %>% 
  gather('var', 'val', -StationCode, -Count) %>% 
  separate(var, c('index', 'measure'), sep = '_') %>% 
  group_by(StationCode, index, measure) %>%
  mutate(
    chng = val / max(val, na.rm = T)
  ) %>%
  ungroup %>% 
  mutate(Count = 500 - Count)
#View(test)


test %>% 
  ggplot(aes(color = StationCode, group = StationCode)) +
  geom_smooth(aes(x = Count, y = chng), method = "loess", se = F) +
  facet_grid(index ~ measure)


#relative difference

test2 <- summary_within_station %>% 
  gather('var', 'val', -StationCode, -Count) %>% 
  separate(var, c('index', 'measure'), sep = '_') %>% 
  group_by(StationCode, index, measure) %>%
  mutate(
    val = replace_na(val,min(val, na.rm = T)),
    chng = abs(val - last(val)) /max(last(val),val)
  ) %>%
  ungroup %>% 
  mutate(Count = 500 - Count)

test2 %>% 
  ggplot(aes(x = Count, y = chng, color = StationCode, group = StationCode)) +
  #geom_point(size = 0.25) +
  geom_smooth(method = "loess", se = F) +
  labs( x = "Reduction of bug",
        y = "Relative difference"
        #title = "Relative Difference of Mean and Standard Deviation for each Scores",
        #subtitle = "As we decrease the number of bug, mean and standard deviation of scores 
        #(CSCI, MMI, O over E) differ significantly"
        ) +
  facet_grid(measure~index)+
  theme_bw()


