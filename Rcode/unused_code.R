
res_df <- res %>% 
  enframe() %>% 
  unnest() %>% 
  group_by_at(vars(name)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=name, value=value) %>%    # spread
  select(-row_id)  # drop the index

mean.core <- apply(res_df, 2, mean)
CI.core <- apply(res_df, 2, quantile, c(0.025, 0.975))
sd.core <- apply(res_df, 2, sd)

result <- tibble(n = nb, 
                 mean = mean.core, 
                 lower = CI.core[1,], 
                 upper = CI.core[2,],
                 sd. = sd.core)


res_plot <- result %>% 
  ggplot() +
  geom_line(aes(nb,mean), color = "red",
            size = 0.7, linetype = 1) +
  geom_line(aes(nb,lower), color = "blue",
            size = 0.4, linetype = 4) +
  geom_line(aes(nb,upper), color = "blue",
            size = 0.4, linetype = 4) +
  labs(x = "number of bug", y = "CSCI score") +
  ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                    ", core = ", site_core, 
                                                    ", iters = ", it))


h <- CSCI_iter %>%
  map(function(df){
    df %>%
      filter(name != 4) %>%
      group_by(name)
  }) %>% 
  modify_depth(.depth = 1, .f = ~as_tibble(bind_rows(.)))