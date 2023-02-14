#append traits to species from surveys

library(tidyverse)
library(dplyr)

sl <- read_csv("species_lists.csv")


#install austraits
remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits) 
austraits <- load_austraits(version = "4.1.0", path = "austraits")

#get traits

spvector <- sl$taxon_name
splist<-dplyr::select(sl, taxon_name) 


#first is maximum plant height
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

#see which species are in Austraits, but have no data for this trait (height)
check<-data.frame(trait1$taxon_name)

check<-check %>% 
  rename(
    taxon_name = trait1.taxon_name
  )

y <- setdiff(distinct_aus, check) 

#append this trait data to original file, with missing data shown as n/a

sl_h<-dplyr::left_join(sl, trait1a, by = "taxon_name")

#now we repeat this process for the other traits

#dispersal syndrome
#all field adult records
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> trait2


#dispersal syndrome by species 
trait2 %>%         
  group_by(taxon_name) %>%
  summarize(dispersal=value) ->trait2a

#see which species are in Austraits, but have no data for this trait (height)
check2<-data.frame(trait2$taxon_name)

check2<-check2 %>% 
  rename(
    taxon_name = trait2.taxon_name
  )

y2 <- setdiff(distinct_aus, check2) 

#append this trait data to original file, with missing data shown as n/a

sl_d<-dplyr::left_join(sl_h, trait2a, by = "taxon_name")
