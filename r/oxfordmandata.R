library(tidyverse)
#----------------------------------
# About assets: https://realized.oxford-man.ox.ac.uk/data/assets
# About realized measures: https://realized.oxford-man.ox.ac.uk/documentation/estimators
# Rdata file
# data: matrix (transposed)
# date: from 2010-01-04 to 2019-11-15
# assets: see the list in the above link
#----------------------------------
load("data/raw/rk2010.Rdata", rv <- new.env())
ls.str(rv)
rk2010 <- 
  rv$data %>% 
  t() %>% 
  as_tibble() %>% 
  add_column(Date = rv$date, .before = 1) %>% 
  filter(between(
    Date,
    lubridate::as_date("2014-01-06"),
    lubridate::as_date("2019-11-15")
  ))
# save-----------------------------
rk2010 %>% 
  write_csv(file = "data/processed/oxfordman.csv")
