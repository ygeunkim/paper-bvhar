library(tidyverse)
# Download data same as in bvhar package------
fred_id <- fredr::fredr_series_search_text(search_text = "CBOE ETF")
etf_vix_long <- map_dfr(
  fred_id$id, 
  fredr::fredr,
  observation_start = as.Date("2012-01-09"), 
  observation_end = as.Date("2015-06-27") 
) %>% 
  select(date, series_id, value)
# Preprocess----------------------------------
etf_vix_raw <- 
  etf_vix_long %>% 
  pivot_wider(names_from = "series_id", values_from = "value")
# only variables and impute missing-----------
etf_vix_raw[, -1] <- 
  etf_vix_raw %>% 
  select(-date) %>% 
  apply(2, imputeTS::na_interpolation) %>% 
  as_tibble()
# save rds------------------------------------
etf_list <- list(
  data_wide = etf_vix_raw,
  data_long = 
    etf_vix_raw %>% 
    pivot_longer(-date, names_to = "series_id", values_to = "value")
)
saveRDS(etf_list, "data/processed/cboe_etf.rds")
rm(list = ls())
ls()
