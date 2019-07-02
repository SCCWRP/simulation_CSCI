library(dbplyr)
library(dplyr)
library(tidyverse)

load(".RData")

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
  

summary_data <- omni_data %>%
  group_by(Count) %>%
  select(Count, MMI, CSCI, OoverE) %>%
  summarise_all(.,
               list(~mean(.), 
                    ~sd(.), 
                    ~quantile(., probs = 0.025),
                    ~quantile(., probs = .975))
               )


mean_data <- summary_data %>%
  select(Count, contains("_mean"))

sd_data <- summary_data %>%
  select(Count, contains("_sd"))

low_data <- summary_data %>%
  select(Count, contains("quantile..3"))

up_data <- summary_data %>%
  select(Count, contains("quantile..4"))

site_core <- site_original$core
site <- site_core$StationCode
site_bug <- site_core$Count
site_csci <- site_core$CSCI
site_mmi <- site_core$MMI
site_oovere <- site_core$OoverE

summary_data %>%
  select(Count, contains("CSCI")) %>%
  ggplot()+
  geom_line(aes(Count, CSCI_mean), color = "red",
            size = 0.7, linetype = 1) +
  geom_line(aes(Count,CSCI_quantile..3), color = "blue",
            size = 0.4, linetype = 4) +
  geom_line(aes(Count,CSCI_quantile..4), color = "blue",
            size = 0.4, linetype = 4) +
  labs(x = "number of bug", y = "CSCI score")+
  ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                    ", core = ", site_csci))


summary_data %>%
  select(Count, contains("OoverE")) %>%
  ggplot()+
  geom_line(aes(Count, OoverE_mean), color = "red",
            size = 0.7, linetype = 1) +
  geom_line(aes(Count, OoverE_quantile..3), color = "blue",
            size = 0.4, linetype = 4) +
  geom_line(aes(Count, OoverE_quantile..4), color = "blue",
            size = 0.4, linetype = 4) +
  labs(x = "number of bug", y = "Observed/Expected")+
  ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                    ", O/E = ", site_oovere))


summary_data %>%
  select(Count, contains("MMI")) %>%
  ggplot()+
  geom_line(aes(Count, MMI_mean), color = "red",
            size = 0.7, linetype = 1) +
  geom_line(aes(Count, MMI_quantile..3), color = "blue",
            size = 0.4, linetype = 4) +
  geom_line(aes(Count, MMI_quantile..4), color = "blue",
            size = 0.4, linetype = 4) +
  labs(x = "number of bug", y = "MMI")+
  ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                    ", MMI = ", site_mmi))




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

