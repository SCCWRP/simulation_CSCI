

# load in CSCI simulation data

mean_test <- summary_data %>%
  select(-contains("Percentile")) %>%
  select(Count, CSCI_mean)


x <- mean_test$Count
y <- mean_test$CSCI_mean

mean_test %>% 
  ggplot() +
  geom_line(aes(Count, CSCI_mean), color = "blue",
            size = 0.7, linetype = 4) +
  labs(x = "number of bug", y = "CSCI score")



m = nls(y ~ c0s + cs*(1.5*abs(x)/as - .5*(abs(x)/as)^3), 
        start = list(c0s = 0.4, cs = 0.9, as = 200), trace = T)

exp_model = nls(y~c0e + ce*(1-exp(-abs(x)/ae)), 
                start = list(c0e = 0.3, ce = 0.9, ae = 400), trace = T)

summary(exp_model)


temp <- summary(m)$coef
c0s = temp[1,1]
cs = temp[2,1]
as = temp[3,1]

plot(x,y, frame.plot = F)
x1 = x
lines(x1, (c0s + cs*(1.5*abs(x1)/as - .5*(abs(x1)/as)^3)) * 
        (x1 <= as) + (c0s+cs)*(x1 > as), lty = 2, col = "red")

temp1 <- summary(exp_model)$coef
c0e = temp1[1,1]
ce = temp1[2,1]
ae = temp1[3,1]

lines(x1, (c0e + ce*(1-exp(-abs(x1)/ae))), lty = 2, col = "blue")



sd_graph <- summary_data %>%
  select(-contains("Percentile")) %>%
  select(Count, CSCI_sd) %>%
  ggplot() +
  geom_line(aes(Count, CSCI_sd), color = "blue",
            size = 0.7, linetype = 4) +
  labs(x = "number of bug", y = "sd CSCI score") +
  ggtitle(paste("Station ", site))




OE_test <- summary_data %>%
  select(-contains("Percentile")) %>%
  select(Count, OoverE_mean)


x <- OE_test$Count
y <- OE_test$OoverE_mean

OE_test %>% 
  ggplot() +
  geom_line(aes(Count, OoverE_mean), color = "blue",
            size = 0.7, linetype = 4) +
  labs(x = "number of bug", y = "O/E")



m = nls(y ~ c0 + cs*(1.5*abs(x)/as - .5*(abs(x)/as)^3), 
        start = list(c0 = 0.4, cs = 0.9, as = 200), trace = T)


temp <- summary(m)$coef
c0 = temp[1,1]
cs = temp[2,1]
as = temp[3,1]

plot(x,y, frame.plot = F)
x1 = x
lines(x1, (c0 + cs*(1.5*abs(x1)/as - .5*(abs(x1)/as)^3)) * 
        (x1 <= as) + (c0+cs)*(x1 > as), lty = 2, col = "green")