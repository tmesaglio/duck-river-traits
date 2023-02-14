#find species from surveys that are not in Austraits

library(tidyverse)
library(dplyr)

sl <- read_csv("species_lists.csv")
aus_sl <- read_csv("duck_creek_floras_data.csv")

distinct_sl = sl %>% distinct(taxon_name)
distinct_aus = aus_sl %>% distinct(taxon_name)

x <- setdiff(distinct_sl, distinct_aus) 


