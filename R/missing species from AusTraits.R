library(tidyverse)
library(dplyr)

sl <- read_csv("species_lists.csv")
aus_sl <- read_csv("duck_creek_floras_data.csv")

distinct_sl = sl %>% distinct(taxon_name)
distinct_aus = aus_sl %>% distinct(taxon_name)

distinct_aus <- distinct_aus %>% 
  rename(
    taxon_name = species
  )

x <- setdiff(distinct_sl, distinct_aus) 


#install austraits
remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits) 
austraits <- load_austraits(version = "4.1.0", path = "austraits")

#get traits

spvector <- sl$taxon_name
splist<-dplyr::select(sl, taxon_name) 

#all field adult records
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "plant_height") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen") & #preserved specimen is flora data
           value_type=="maximum") -> trait1


# mean and max height by species 
trait1 %>%         
  group_by(taxon_name) %>%
  summarize(max_max_height=max(as.numeric(value),na.rm=T),mean_max_height=mean(as.numeric(value),na.rm=T)) -> trait1a


check<-data.frame(trait1$taxon_name)

check<-check %>% 
  rename(
    species = trait1.taxon_name
  )

y <- setdiff(distinct_aus, check) 

write_csv(y,"data/austraits_missing_species.csv")
write_csv(x,"data/austraits_missing_species2.csv")
