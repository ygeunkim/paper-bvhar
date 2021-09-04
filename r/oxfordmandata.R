library(tidyverse)
#----------------------------------
# About assets: https://realized.oxford-man.ox.ac.uk/data/assets
# About realized measures: https://realized.oxford-man.ox.ac.uk/documentation/estimators
# Use oxfordman RV5 as starting value of simulation
# VAR(p): only need p
# VHAR: 22
# From March 2015
#----------------------------------
oxfordman <- read_csv("data/raw/oxfordmanrealizedvolatilityindices.csv")
oxfordman <- 
  oxfordman %>% 
  select(Date, Symbol, rv5) %>% # rv5
  mutate(Symbol = str_remove(Symbol, pattern = "^.")) %>% 
  filter(Date > "2015-03-01", Date < "2021-09-01") %>% # from March 2015
  pivot_wider(id_cols = Date, names_from = "Symbol", values_from = "rv5") %>%  # each column is asset
  arrange(Date)
# save-----------------------------
oxfordman %>% 
  write_csv(file = "data/processed/oxfordman.csv")
