
models_fit <- function(site, name_string){
  summary_data <- summarising_tidied_data(site)
  test <- summary_data %>%
    select(Count, name_string)
  
  x <- pull(test[,1])
  y <- pull(test[,2])
  sph_model = nls(y ~ c0s + cs*(1.5*abs(x)/as - .5*(abs(x)/as)^3), 
                  start = list(c0s = 0.4, cs = 0.9, as = 300), trace = F)
  temp <- summary(sph_model)$coef
  c0s = temp[1,1]
  cs = temp[2,1]
  as = temp[3,1]
  
  exp_model = nls(y~c0e + ce*(1-exp(-abs(x)/ae)), 
                  start = list(c0e = 0.3, ce = 0.9, ae = 300), trace = F)
  temp1 <- summary(exp_model)$coef
  c0e = temp1[1,1]
  ce = temp1[2,1]
  ae = temp1[3,1]
  x1 <- x
  spher <- (c0s + cs*(1.5*abs(x1)/as - .5*(abs(x1)/as)^3)) * 
    (x1 <= as) + (c0s+cs)*(x1 > as)
  expo <- (c0e + ce*(1-exp(-abs(x1)/ae)))
  summary_model_data <- add_column(test, Sphere = spher, Expo = expo)
  
  est_data <- tibble(type = glue::glue(name_string),
                     nugget_s = c0s, nugget_e = c0e,
                     sill_s = cs, sill_e = ce, 
                     range_s = as, range_e = ae, 
                     delta = as-ae)
  
  return(list(summary_model_data, est_data))
}



summaried_data_with_models_to_csci <- function(site, 
                                               station_list, 
                                               name_string){
  
  scores <- subsampling.site.info(site, station_list)
  site_csci <- scores$csci
  site_mmi <- scores$mmi
  site_oovere <- scores$oovere
  site_bug <- scores$cnt
  
  a <- models_fit(site, name_string[1])
  b <- models_fit(site, name_string[2])
  c <- models_fit(site, name_string[3])
  
  model_data <- bind_rows(a[[1]], b[[1]], c[[1]])
  est_data <- bind_rows(a[[2]], b[[2]], c[[2]])
  
  return(list(summ = model_data,
              est = est_data)
  )
}


