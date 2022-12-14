library(tidyverse)
library(dplyr)

sl <- read_csv("species_lists.csv")
aus_sl <- read_csv("duck_creek_floras_data.csv")

distinct_sl = sl %>% distinct(species)
distinct_aus = aus_sl %>% distinct(taxon_name)

distinct_aus <- distinct_aus %>% 
  rename(
    species = taxon_name
  )

x <- setdiff(distinct_sl, distinct_aus) 
