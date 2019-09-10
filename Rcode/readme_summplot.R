
library(here)
library(glue)
library(tidyverse)
library(colorspace)
library(drlib)
library(dbplyr)
library(RPostgreSQL)
library(CSCI)
library(patchwork)

# effort summary plots ----------------------------------------------------

# import results
for (i in 2:7){
  load(here('Rmarkdown', glue::glue("site{i}.RData")))
}

# extract summary results for each, get average, sd, cv
sumdat <- tibble(
    labs = 2:7
  ) %>% 
  rowwise %>% 
  mutate(
    Site = eval(parse(text = glue('site{labs}$csci$labels$title'))), 
    Site = gsub('^Station\\s', '', Site),
    dat = purrr::map(labs, function(labs){
      
      dat <- eval(parse(text = glue('site{labs}$summ'))) %>% 
        select(Count, CSCI_mean, CSCI_sd) %>% 
        rename(
          sd = CSCI_sd,
          av = CSCI_mean
        ) %>% 
        mutate(
          cv = sd / av
        )
      
      return(dat)
      
      })
  ) %>% 
  unnest %>% 
  select(-labs)

# fit exponential models to the results
moddateff <- sumdat %>% 
  group_by(Site) %>% 
  mutate(
    ests = predict(nls(av~c0e + ce*(1-exp(-abs(Count)/ae)), 
                      start = list(c0e = 0.3, ce = 0.9, ae = 200), trace = F)), 
    estsrc = ests / max(ests), 
    avrc = av / max(ests)
  ) %>% 
  nest %>% 
  mutate(
    data = purrr::map(data, function(x){
      
      mod <- try(nls(cv ~ SSasymp(Count, yf, y0, log_alpha), data = x))
      if(inherits(mod, 'try-error')) 
        cvests <- NA
      else 
        cvests <- c(predict(mod), NA)
         
      x <- x %>% 
        mutate(
          cvests = cvests
        )
      
      return(x)
      
    }) 
  ) %>% 
  unnest

# plots

newlbs <- reorder(moddateff$Site, moddateff$av, max) %>% 
  attr('scores') %>% 
  round(2) %>% 
  sort(decreasing = T)

toplo <- moddateff %>% 
  mutate(
    Site = factor(Site, levels = names(newlbs), labels = newlbs)
  )

colpal <- 'agGrnYl'

p1 <- ggplot(toplo, aes(x = Count, y = av, colour = Site)) + 
  geom_vline(aes(xintercept = 250), size =1) +
  geom_point(alpha = 0.6) +
  geom_hline(data = filter(group_by(toplo, Site), ests == max(ests)), aes(yintercept = ests, colour = reorder(Site, av, max)), linetype = 'dashed') + 
  geom_line(aes(y = ests), size = 1) + 
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Sample count', 
    y = 'CSCI score', 
    title = '(a) Average score for 100 subsamples\nat each sample count'
  ) +
  scale_color_discrete_sequential(palette = colpal)
  
p2 <- ggplot(toplo, aes(x = Count, y = avrc, colour = Site)) + 
  geom_vline(aes(xintercept = 250), size =1) + 
  geom_point(alpha = 0.6) +
  # geom_hline(yintercept = 0.9, linetype = 'dashed') +
  geom_line(aes(y = estsrc), size = 1) + 
  theme_bw(base_family = 'serif') + 
  theme(
    legend.position = c(0.8, 0.27)
    ) +
  labs(
    x = 'Sample count', 
    y = 'Relative CSCI', 
    title = '(b) Relative scores scaled by actual CSCI'
  ) +
  guides(colour = guide_legend(title = 'Actual CSCI')) + 
  scale_color_discrete_sequential(palette = colpal)  

p3 <- ggplot(toplo, aes(x = Count, y = cv, colour = Site)) + 
  geom_vline(aes(xintercept = 250), size =1) + 
  geom_point(alpha = 0.6) +
  # geom_hline(yintercept = 0.1, linetype = 'dashed') +
  geom_line(aes(y = cvests), size = 1) +
  geom_smooth(data = filter(toplo, is.na(cvests)), se = F, span = 1.5) +
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') +
  labs(
    x = 'Sample count', 
    y = 'Coefficient of variation', 
    title = '(c) Variation of CSCI scores for each\nsample count'
  ) +
  scale_color_discrete_sequential(palette = colpal)

png(here('summary_results_effort.png'), width = 11, height = 4.5, res = 300, units = 'in')
p1 + p2 + p3 + plot_layout(ncol = 3)
dev.off()

# p1 by itself
p1 <- ggplot(toplo, aes(x = Count, y = av, colour = Site)) + 
  geom_vline(aes(xintercept = 250), size =1) +
  geom_point(alpha = 0.6) +
  geom_hline(data = filter(group_by(toplo, Site), ests == max(ests)), aes(yintercept = ests, colour = reorder(Site, av, max)), linetype = 'dashed') + 
  geom_line(aes(y = ests), size = 1) + 
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Sample count', 
    y = 'CSCI score'
  ) +
  scale_color_discrete_sequential(palette = colpal)

png(here('summary_results_effort_ind.png'), width = 4, height = 4, res = 300, units = 'in')
p1
dev.off()

# # richness and abundance plots --------------------------------------------
# 
# # setup connection
# con <- DBI::dbConnect(
#   RPostgreSQL::PostgreSQL(),
#   dbname = 'smc', 
#   host = '192.168.1.17', 
#   user = rstudioapi::askForPassword('Database user'), 
#   password = rstudioapi::askForPassword("Database password")
# )
# 
# # raw connections
# datbugcon <- tbl(con, 'tbl_taxonomyresults') %>% 
#   filter(stationcode %in% c("SMC00476", "SGUR103", "SMC01424", "SMC01384", "801M16861", "SMC02984")) %>% 
#   group_by(stationcode) %>%
#   filter(sampledate == max(sampledate)) %>%
#   filter(fieldreplicate == max(fieldreplicate)) %>% 
#   collect()
# 
# bug_origin <- as_tibble(datbugcon) %>% 
#   select("stationcode", "sampledate", "fieldreplicate", 
#          "fieldsampleid","finalid", "lifestagecode", 
#          "baresult", "result", "unit", 
#          "distinctcode")%>%
#   mutate(
#     baresult = as.numeric(baresult)
#   )
# 
# # Fix names
# colnames(bug_origin) <- c("StationCode","SampleDate","SampleID", 
#                           "FieldSampleID", "FinalID", "LifeStageCode", 
#                           "BAResult", "Result", "Unit", 
#                           "distinct")
# 
# # safit 1 bug names
# refnames <- CSCI::loadRefBugData() %>% 
#   select(FinalID, SAFIT1) %>% 
#   unique
#   
# 
# bug.site.clean <- CSCI::cleanData(bug_origin, purge = T) %>% 
#   select(StationCode, FinalID, BAResult) %>% 
#   left_join(refnames, by = 'FinalID') %>% 
#   group_by(StationCode, SAFIT1) %>% 
#   summarise(BAResult = sum(BAResult, na.rm = T)) %>% 
#   ungroup %>% 
#   filter(!is.na(SAFIT1))
# 
# # summarize diversity, richness, evenness
# div <- bug.site.clean %>% 
#   group_by(StationCode) %>% 
#   nest %>% 
#   mutate(
#     sums = purrr::map(data, function(x){
#       
#       x <- x %>% 
#         select(SAFIT1, BAResult) %>% 
#         spread(SAFIT1, BAResult)
#       
#       div <- diversity(x, index = 'shannon')
#       ric <- ncol(x)
#       evn <- div / log(ric)
#       
#       out <- data.frame(div = div, ric = ric, evn = evn)
#       
#       return(out)
#       
#     })
#   ) %>% 
#   unnest(sums) %>% 
#   select(-data) %>% 
#   gather('var', 'val', -StationCode)
# 
# # abundance/richness plot
# p <- ggplot(bug.site.clean, aes(x = reorder_within(SAFIT1, BAResult, StationCode), y = BAResult, fill = BAResult)) + 
#   geom_bar(stat = 'identity', color = 'grey') + 
#   facet_wrap(StationCode~., scales = 'free_y') + 
#   theme_bw() +
#   theme(
#     axis.title = element_blank(), 
#     axis.text.y = element_text(size = 6), 
#     legend.position = 'none', 
#     strip.background = element_blank()
#   ) + 
#   scale_fill_continuous_sequential('Peach') +
#   scale_x_reordered() +
#   coord_flip() 
# 
# png(here('siteabu.png'), height = 9, width = 12, units = 'in', res = 300, family = 'serif')
# p 
# dev.off()

# ambiguous summary plots -------------------------------------------------

# import results
load(file = 'stations/sum-ambig.RData')

# extract summary results for each, get average, sd, cv
sumdat <- sum_dat %>% 
  filter(!StationCode %in% 'SMCR8_277') %>% 
  select(StationCode, Pcnt_Replaced, CSCI_mean, CSCI_sd) %>% 
  rename(
    Site = StationCode,
    av = CSCI_mean,
    sd = CSCI_sd
  ) %>% 
  mutate(
    cv = sd / av, 
    Pcnt_Complete = 1 - Pcnt_Replaced
  )

# fit exponential models to the results
moddat <- sumdat %>% 
  group_by(Site) %>% 
  nest %>% 
  mutate(
    data = purrr::map(data, function(x){
      
      # ave values
      modav <- try(nls(av ~ SSasymp(Pcnt_Complete, yf, y0, log_alpha), data = x))
      if(inherits(modav, 'try-error')) 
        avests <- NA
      else 
        avests <- predict(modav)
      
      # cv values
      modcv <- try(nls(cv ~ SSasymp(Pcnt_Complete, yf, y0, log_alpha), data = x))
      if(inherits(modcv, 'try-error')) 
        cvests <- NA
      else 
        cvests <- predict(modcv)
      
      x <- x %>% 
        mutate(
          avests = avests,
          cvests = cvests
        )
      
      return(x)
      
    }) 
  ) %>% 
  unnest %>% 
  group_by(Site) %>% 
  mutate(
    avrc = av / max(av)
  ) %>% 
  ungroup

# plots

newlbs <- reorder(moddateff$Site, moddat$av, max) %>% 
  attr('scores') %>% 
  round(2) %>% 
  sort(decreasing = T)

toplo <- moddat %>% 
  mutate(
    Site = factor(Site, levels = names(newlbs), labels = newlbs)
  )

colpal <- 'agGrnYl'

p1 <- ggplot(toplo, aes(x = Pcnt_Complete, y = av, colour = Site)) + 
  geom_vline(aes(xintercept = 0.5), size =1) +
  geom_point(alpha = 0.6) +
  geom_hline(data = filter(group_by(toplo, Site), avests == max(avests)), aes(yintercept = avests, colour = reorder(Site, av, max)), linetype = 'dashed') +
  # geom_line(aes(y = avests), size = 1) +
  geom_smooth(method = 'nls', 
              formula = y ~ SSasymp(x, yf, y0, log_alpha), 
              se = F
  ) +
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') + 
  labs(
    x = '% taxa identified', 
    y = 'CSCI score', 
    title = '(a) Average score for 100 subsamples\nat each percentage of taxa identified'
  ) +
  scale_color_discrete_sequential(palette = colpal)

p2 <- ggplot(toplo, aes(x = Pcnt_Complete, y = avrc, colour = Site)) + 
  geom_vline(aes(xintercept = 0.5), size =1) +
  geom_point(alpha = 0.6) +
  # geom_hline(yintercept = 0.9, linetype = 'dashed') +
  geom_smooth(method = 'nls', 
              formula = y ~ SSasymp(x, yf, y0, log_alpha), 
              se = F
  ) +
  theme_bw(base_family = 'serif') + 
  theme(
    legend.position = c(0.8, 0.27)
  ) +
  labs(
    x = '% taxa identified', 
    y = 'Relative CSCI', 
    title = '(b) Relative scores scaled by actual CSCI'
  ) +
  guides(colour = guide_legend(title = 'Actual CSCI')) + 
  scale_color_discrete_sequential(palette = colpal)  

p3 <- ggplot(toplo, aes(x = Pcnt_Complete, y = cv, colour = Site)) +
  geom_vline(aes(xintercept = 0.5), size =1) +
  geom_point(alpha = 0.6) +
  # geom_hline(yintercept = 0.1, linetype = 'dashed') +
  # geom_line(aes(y = cvests), size = 1) +
  # geom_smooth(method = 'nls', 
  #             formula = y ~ SSasymp(x, yf, y0, log_alpha), 
  #             se = F
  # ) +
  geom_smooth(se = F, span = 1.5) +
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') +
  labs(
    x = '% taxa identified', 
    y = 'Coefficient of variation', 
    title = '(c) Variation of CSCI scores at each\npercentage of taxa identified'
  ) +
  scale_color_discrete_sequential(palette = colpal)

png(here('summary_results_ambig.png'), width = 11, height = 4.5, res = 300, units = 'in')
p1 + p2 + p3 + plot_layout(ncol = 3)
dev.off()

# p1 by itself
p1 <- ggplot(toplo, aes(x = Pcnt_Complete, y = av, colour = Site)) + 
  geom_vline(aes(xintercept = 0.5), size =1) +
  geom_point(alpha = 0.6) +
  geom_hline(data = filter(group_by(toplo, Site), avests == max(avests)), aes(yintercept = avests, colour = reorder(Site, av, max)), linetype = 'dashed') +
  # geom_line(aes(y = avests), size = 1) +
  geom_smooth(method = 'nls', 
              formula = y ~ SSasymp(x, yf, y0, log_alpha), 
              se = F
  ) +
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') + 
  labs(
    x = '% taxa identified', 
    y = 'CSCI score'
  ) +
  scale_color_discrete_sequential(palette = colpal)

png(here('summary_results_ambig_ind.png'), width = 4, height = 4, res = 300, units = 'in')
p1
dev.off()

