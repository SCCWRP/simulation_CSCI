library(dbplyr)
library(dplyr)
library(tidyverse)


load("P:/PartTimers/ThiHo/Projects/CSCISample/data/SMC00480.RData")

# CSCI_iter <- c()
# for (i in 1:length(nb)){
#   CSCI_iter[[i]] <- res1[[i]] %>% 
#     enframe()
# }


omni_data <- bind_rows(CSCI_iter) %>% group_by(Count) %>%
  summarise_at(vars(-(1:4)), 
               list(~mean(.), 
                    ~sd(.), 
                    ~quantile(., probs = 0.025),
                    ~quantile(., probs = .975)))

mean_data <- omni_data %>%
  select(Count, contains("_mean"))

sd_data <- omni_data %>%
  select(Count, contains("_sd"))

low_data <- omni_data %>%
  select(Count, contains("quantile..3"))

up_data <- omni_data %>%
  select(Count, contains("quantile..4"))


site <- site_original$StationCode
site_bug <- site_original$Count
site_core <- site_original$CSCI
site_mmi <- site_original$MMI
site_oovere <- site_original$OoverE

omni_data %>%
  select(-contains("Percentile")) %>%
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
                                                    ", core = ", site_core))


omni_data %>%
  select(-contains("Percentile")) %>%
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


omni_data %>%
  select(-contains("Percentile")) %>%
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

mean_test <- omni_data %>%
  select(-contains("Percentile")) %>%
  select(Count, CSCI_mean)


x <- mean_test$Count
y <- mean_test$CSCI_mean


m = nls(y ~ c0s + cs*(1.5*abs(x)/as - .5*(abs(x)/as)^3), 
        start = list(c0s = 0.4, cs = 0.9, as = 200), trace = T)

exp_model = nls(y~c0e + ce*(1-exp(-abs(x)/ae)), 
                start = list(c0e = 0.3, ce = 0.9, ae = 400), trace = T)

summary(exp_model)


temp <- summary(m)$coef
c0s = temp[1,1]
cs = temp[2,1]
as = temp[3,1]


temp1 <- summary(exp_model)$coef
c0e = temp1[1,1]
ce = temp1[2,1]
ae = temp1[3,1]

x1 <- x
spher <- (c0s + cs*(1.5*abs(x1)/as - .5*(abs(x1)/as)^3)) * 
        (x1 <= as) + (c0s+cs)*(x1 > as)

expo <- (c0e + ce*(1-exp(-abs(x1)/ae)))


  omni_data %>% 
  ggplot() +
  geom_point(aes(Count, CSCI_mean)) +
  geom_line(aes(Count,CSCI_quantile..3, color = "lower bound"),
            size = 0.4, linetype = 4) +
  geom_line(aes(Count, CSCI_quantile..4, color = "upper bound"),
            size = 0.4, linetype = 4) +
  geom_line(aes(x=x1, y = spher, color = "sphere"), size = 0.7) +
  geom_line(aes(x=x1, y = expo, color = "exponent"), size = 0.7) + 
  geom_hline(yintercept = site_core, linetype = 3) +
  geom_hline(yintercept = (site_core -.1), linetype = 3) +
  geom_hline(yintercept = (site_core +.1), linetype = 3) +
  geom_vline(xintercept = ae, color = "red")+
  geom_vline(xintercept = as, color = "skyblue")+
  labs(x = "number of bug", y = "CSCI score",
       fill = "model") +
  ggtitle(paste("Station ", site), 
          subtitle = paste("CSCI score = ", site_csci,
                           "\nexp_sill = ", round(ae,2),
                           "\nsph_sill = ", round(as,2)))







sd_graph <- omni_data %>%
  select(-contains("Percentile")) %>%
  select(Count, CSCI_sd) %>%
  ggplot() +
  geom_line(aes(Count, CSCI_sd), color = "blue",
            size = 0.7, linetype = 4) +
  labs(x = "number of bug", y = "sd CSCI score") +
  ggtitle(paste("Station ", site))




OE_test <- omni_data %>%
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

