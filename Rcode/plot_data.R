
site_bug <- site[4]
result %>% 
  ggplot() +
  geom_line(aes(nb,mean), color = "red",
            size = 0.7, linetype = 1) +
  geom_line(aes(nb,lower), color = "blue",
            size = 0.4, linetype = 4) +
  geom_line(aes(nb,upper), color = "blue",
            size = 0.4, linetype = 4) +
  labs(x = "number of bug", y = "CSCI score") +
  ggtitle(paste("Station ", site), 
          subtitle = paste("bugs = ", site_bug, ", core = ", site_core, ", iters = ", it))


result %>% 
  ggplot() +
  geom_point(aes(nb,mean), color = "red",
             size = 0.7) +
  geom_point(aes(nb,lower), color = "blue",
             size = 0.4) +
  geom_point(aes(nb,upper), color = "blue",
             size = 0.4) +
  labs(x = "number of bug", y = "CSCI score") +
  ggtitle(paste("Station ", site), 
          subtitle = paste("bugs = ", site_bug, ", core = ", site_core, ", iters = ", it))


res_df %>% 
  gather(.) %>% 
  arrange(as.numeric(key), desc(key)) %>%
  ggplot() +
  aes(x = key, y = value) +
  geom_boxplot()