
scores_plot <- function(site, station_list){
  scores <- subsampling.site.info(site = site, station_list= station_list)
  site_csci <- scores$csci
  site_mmi <- scores$mmi
  site_oovere <- scores$oovere
  site_bug <- scores$cnt
  
  data <-  summaried_data_with_models_to_csci_mean(site, station_list)
  summary_data <- data$summ
  model_data <- data$est
  
  ae = model_data$range_e
  as = model_data$range_s
  
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
    labs(x = "Sample size", y = "CSCI score",
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
    labs(x = "Sample size", y = "Observed/Expected")+
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
    labs(x = "Sample size", y = "MMI") +
    ggtitle(paste("Station ", site), subtitle = paste("bugs = ", site_bug,
                                                      ", MMI = ", round(site_mmi,3)))
    
  return(list(csci = CSCI_plot, 
              oovere = OoverE_plot, 
              mmi = MMI_plot,
              est = model_data))
}


site_list <- c("SMCR8_277","SMC00476", "SGUR103",  
               "SMC01424", "SMC01384", "801M16861","SMC02984")

for (i in seq_along(site_list)){
  assign(paste0("site", i), scores_plot(site_list[i], station_list))
}

save(site1, file = "Rmarkdown/site1.RData")
save(site2, file = "Rmarkdown/site2.RData")
save(site3, file = "Rmarkdown/site3.RData")
save(site4, file = "Rmarkdown/site4.RData")
save(site5, file = "Rmarkdown/site5.RData")
save(site6, file = "Rmarkdown/site6.RData")
save(site7, file = "Rmarkdown/site7.RData")


