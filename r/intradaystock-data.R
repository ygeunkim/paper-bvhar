library(tidyverse)
library(highfrequency)
#--------------------------------
# Data source: https://www.kaggle.com/borismarjanovic/daily-and-intraday-stock-price-data
#--------------------------------
# lists of ETF files-------------
# 1341 files
# txt files are CSV format
# No NA values in each file
#--------------------------------
etf_list <- list.files(
  "data/raw/intradaystock/5 Min/ETFs",
  full.names = TRUE
)
# etf_price <- parallel::mclapply(etf_list, read_csv, mc.cores = 8)
etf_price <- 
  parallel::mclapply(
    etf_list,
    function(FILE) {
      # extract the asset name from the file name (between ETFs/ and .txt)
      asset <- str_extract(FILE, pattern = "(?<=ETFs/)(.*)(?=\\.txt$)")
      # import the file using readr::read_csv----------------------------
      read_csv(FILE) %>% 
        unite(col = "Datetime", c(Date, Time), sep = " ") %>% # make Date-Time to dttm class
        mutate(
          Datetime = lubridate::as_datetime(Datetime)
        ) %>% 
        select(Datetime, Close) %>% # only need close price in this analysis
        setNames(c("Datetime", asset))
    },
    mc.cores = 12
  ) %>% 
  reduce(full_join, by = "Datetime") %>% # wide format (Datetime, names of each asset)
  arrange(Datetime)
# save----------------------------
# write_csv(etf_price, "data/processed/etf_5min.csv")
# Deal with NA--------------------
# NA: non-overlapped period
# might be okay to remove head and tail
#---------------------------------
etf_price %>% 
  filter(between(
    Datetime,
    lubridate::as_datetime("2017-11-17"),
    lubridate::as_datetime("2017-12-06")
  ))









