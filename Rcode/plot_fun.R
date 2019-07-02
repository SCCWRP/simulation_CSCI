library(dbplyr)
library(tidyverse)


site_list <- c("SMCR8_277","SMC00476", "SGUR103",  "SMC01424", "SMC01384", 
          "801M16861","SMC02984", "SMC16832", "801M12669","412CE0732")


site = "SMC01424"
plot_fun <- function(site){
  load(paste0("stations/", site,".RData"))
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
  
  site_core <- site_original$core
  site <- site_core$StationCode
  site_bug <- site_core$Count
  site_csci <- site_core$CSCI
  site_mmi <- site_core$MMI
  site_oovere <- site_core$OoverE
  
  mean_test <- summary_data %>%
    select(Count, CSCI_mean)
  
  x <- mean_test$Count
  y <- mean_test$CSCI_mean
  sph_model = nls(y ~ c0s + cs*(1.5*abs(x)/as - .5*(abs(x)/as)^3), 
          start = list(c0s = 0.4, cs = 0.9, as = 400), trace = F)
  temp <- summary(sph_model)$coef
  c0s = temp[1,1]
  cs = temp[2,1]
  as = temp[3,1]
  
  exp_model = nls(y~c0e + ce*(1-exp(-abs(x)/ae)), 
                  start = list(c0e = 0.3, ce = 0.9, ae = 200), trace = F)
  temp1 <- summary(exp_model)$coef
  c0e = temp1[1,1]
  ce = temp1[2,1]
  ae = temp1[3,1]
  x1 <- x
  spher <- (c0s + cs*(1.5*abs(x1)/as - .5*(abs(x1)/as)^3)) * 
    (x1 <= as) + (c0s+cs)*(x1 > as)
  expo <- (c0e + ce*(1-exp(-abs(x1)/ae)))
  summary_data <- add_column(summary_data, Sphere = spher, Expo = expo)
  
  CSCI_plot <- summary_data %>%
    ggplot() +
    geom_point(aes(Count, CSCI_mean)) +
    geom_line(aes(Count,CSCI_quantile..3), color = "blue",
              size = 0.4, linetype = 4) +
    geom_line(aes(Count, CSCI_quantile..4), color = "blue",
              size = 0.4, linetype = 4) +
    geom_line(aes(Count, Sphere, color = "Spherical"), size = 1) +
    geom_line(aes(Count, Expo, color = "Exponential"), size = 1) + 
    geom_hline(yintercept = site_csci, linetype = 3) +
    geom_hline(yintercept = (site_csci -.1), linetype = 3) +
    geom_hline(yintercept = (site_csci +.1), linetype = 3) +
    geom_vline(xintercept = ae, color = "darkgreen", linetype = 4) +
    geom_vline(xintercept = as, color = "red", linetype = 4) +
    labs(x = "number of bug", y = "CSCI score",
         fill = "model") +
    scale_color_manual("", 
                       breaks = c("Spherical", "Exponential"), 
                       values = c("darkgreen", "red"))+
    ggtitle(paste("Station", site), 
            subtitle = paste("CSCI score = ", round(site_csci,3), ", number of bugs = ", site_bug,
                             "\nrange: exponential: ", round(ae,2),
                             "\n           spherical:     ", round(as,2)))
  
  OoverE_plot <- summary_data %>%
    ggplot()+
    geom_line(aes(Count, OoverE_mean), color = "red",
              size = 0.7, linetype = 1) +
    geom_line(aes(Count, OoverE_quantile..3), color = "blue",
              size = 0.4, linetype = 4) +
    geom_line(aes(Count, OoverE_quantile..4), color = "blue",
              size = 0.4, linetype = 4) +
    labs(x = "number of bug", y = "Observed/Expected")+
    ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                      ", O/E = ", round(site_oovere,3)))
  
  MMI_plot <- summary_data %>%
    ggplot()+
    geom_line(aes(Count, MMI_mean), color = "red",
              size = 0.7, linetype = 1) +
    geom_line(aes(Count, MMI_quantile..3), color = "blue",
              size = 0.4, linetype = 4) +
    geom_line(aes(Count, MMI_quantile..4), color = "blue",
              size = 0.4, linetype = 4) +
    labs(x = "number of bug", y = "MMI")+
    ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                      ", MMI = ", round(site_mmi,3)))
  est_data <- tibble(count = site_bug,
                     CSCI = site_csci,
                     MMI = site_mmi,
                     OoverE = site_oovere,
                     nugget_s = c0s, nugget_e = c0e,
                     sill_s = cs, sill_e = ce, 
                     range_s = as, range_e = ae, 
                     delta = as-ae)
  
  plot_list <- list(CSCI_plot, OoverE_plot, MMI_plot)
  tibble_list <- list(summary_data, est_data)
  
  return(list(csci = CSCI_plot, 
              oovere = OoverE_plot, 
              mmi = MMI_plot, 
              est = est_data, 
              summ = summary_data))

}


site = site_list[2]
aa <-plot_fun(site)

site = site_list[2]
aa <-plot_fun(site)

site3 <- c()

for (i in 2:7){
  assign(paste0("site", i), plot_fun(site_list[i]))
}

save(site2, file = "Rmarkdown/site2.RData")
save(site3, file = "Rmarkdown/site3.RData")
save(site4, file = "Rmarkdown/site4.RData")
save(site5, file = "Rmarkdown/site5.RData")
save(site6, file = "Rmarkdown/site6.RData")
save(site7, file = "Rmarkdown/site7.RData")