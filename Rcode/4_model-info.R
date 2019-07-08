

subsampling.site.info <- function(site, station_list){
  site_pick_core <- tidying_simulated_data(site) %>%
    filter(Count == 500)
  
  site <- site_pick_core$StationCode
  site_csci <- site_pick_core$CSCI
  site_mmi <- site_pick_core$MMI
  site_oovere <- site_pick_core$OoverE
  
  site_info <- station_list %>%
    filter(StationCode == site) %>%
    filter(SampleDate == max(SampleDate))
  site_bug <- site_info$Count
  
  return(list(site = site, 
              csci = site_csci, 
              mmi = site_mmi, 
              oovere = site_oovere,
              cnt = site_bug))
}





summaried_data_with_models_to_csci_mean <- function(site, station_list){
  
  scores <- subsampling.site.info(site, station_list)
  site_csci <- scores$csci
  site_mmi <- scores$mmi
  site_oovere <- scores$oovere
  site_bug <- scores$cnt
  
  summary_data <- summarising_tidied_data(site)
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
  summary_model_data <- add_column(summary_data, Sphere = spher, Expo = expo)
  
  est_data <- tibble(count = site_bug,
                     CSCI = site_csci,
                     MMI = site_mmi,
                     OoverE = site_oovere,
                     nugget_s = c0s, nugget_e = c0e,
                     sill_s = cs, sill_e = ce, 
                     range_s = as, range_e = ae, 
                     delta = as-ae)
  
  return(list(summ = summary_model_data,
              est = est_data)
         )
}

