
library(here)
library(glue)
library(tidyverse)
library(colorspace)

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
moddat <- sumdat %>% 
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

colpal <- 'Dark2'

p1 <- ggplot(moddat, aes(x = Count, y = av, group = Site, colour = Site)) + 
  geom_point(alpha = 0.6) +
  geom_hline(data = filter(group_by(moddat, Site), ests == max(ests)), aes(yintercept = ests, colour = Site), linetype = 'dashed') + 
  geom_line(aes(y = ests), size = 1) + 
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Sample count', 
    y = 'CSCI score', 
    title = '(a) Average score for 100 subsamples\nat each sample count'
  ) +
  scale_color_discrete_qualitative(palette = colpal)
  
p2 <- ggplot(moddat, aes(x = Count, y = avrc, group = Site, colour = Site)) + 
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0.9, linetype = 'dashed') +
  geom_line(aes(y = estsrc), size = 1) + 
  theme_bw(base_family = 'serif') + 
  theme(
    legend.position = c(0.8, 0.27), 
    legend.title = element_blank()
    ) +
  labs(
    x = 'Sample count', 
    y = 'Relative CSCI', 
    title = '(b) Relative scores scaled by actual CSCI'
  ) +
  scale_color_discrete_qualitative(palette = colpal)

p3 <- ggplot(moddat, aes(x = Count, y = cv, group = Site, colour = Site)) + 
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0.1, linetype = 'dashed') +
  geom_line(aes(y = cvests), size = 1) +
  theme_bw(base_family = 'serif') + 
  theme(legend.position = 'none') +
  labs(
    x = 'Sample count', 
    y = 'Coefficient of variation', 
    title = '(c) Variation of CSCI scores for each\nsample count'
  ) +
  scale_color_discrete_qualitative(palette = colpal)

png(here('summary_results.png'), width = 11, height = 4.5, res = 300, units = 'in')
p1 + p2 + p3 + plot_layout(ncol = 3)
dev.off()
