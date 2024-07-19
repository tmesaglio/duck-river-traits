#note: I use all the same dataframe/object names as the 'append traits to extinct natives' R script, so don't run this in the same session as that one

library(tidyverse)
library(dplyr)

list <- read_csv("species_list.csv")
sl<-dplyr::filter(list, mesaglio_2024 == "yes")


#install austraits
remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask", force = TRUE)

library(austraits) 
austraits::load_austraits(version = "6.0.0")

#for now, line 17 isn't working for me for some reason (it's working for other users), so here's a workaround
#download the RDS file from https://zenodo.org/records/10156222, rename it to 'austraits', then manually click on the .rds file in file explorer to open it in Rstudio

#get traits

spvector <- sl$taxon_name

#first is maximum plant height
#all field adult records
(`austraits-6.0.0` %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "plant_height") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature field","preserved_specimen") & #preserved specimen is flora data
           value_type=="maximum") -> trait1


# mean and max height by species 
trait1 %>%         
  group_by(taxon_name) %>%
  summarize(max_max_height=max(as.numeric(value),na.rm=T),mean_max_height=mean(as.numeric(value),na.rm=T)) -> trait1a


#see which species are in Austraits, but have no data for this trait 
check<-data.frame(trait1$taxon_name)

check<-check %>% 
  rename(
    taxon_name = trait1.taxon_name
  )

APC_sp <-dplyr::select(sl, taxon_name)

y <- setdiff(APC_sp, check)


#for almost all of the species missing height data here, I took measurements at Duck River. Added those externally, plus one from looking up online
height_append<- read_csv("data/height_append_persist2.csv")

pre_master <-dplyr::select(trait1a, taxon_name, max_max_height)

master1 <-dplyr::bind_rows(pre_master, height_append)

#correct error for Lobelia purpurascens
master1[130, 2] = 0.28

#change mistletoe heights to NA
master1a <- master1 %>% 
  mutate(max_max_height=ifelse(taxon_name=="Muellerina eucalyptoides",NA,max_max_height),
         max_max_height=ifelse(taxon_name=="Amyema miquelii",NA,max_max_height))


#now let's do dispersal syndrome
#all field adult records
(`austraits-6.0.0` %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature field","preserved_specimen"))-> trait2


#remove Draper study (too many species scored from broad family/genus values)
trait2x<-dplyr::filter(trait2, dataset_id != "Draper_2023")

#dispersal syndrome by species 
trait2x %>%         
  group_by(taxon_name) %>%
  summarize(dispersal=value) ->trait2a

#see which species are in Austraits, but have no data for this trait
check2<-data.frame(trait2$taxon_name)

check2<-check2 %>% 
  rename(
    taxon_name = trait2.taxon_name
  )

y2 <- setdiff(APC_sp, check2) 

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait2a,"data/dispersal_syndrome_persist.csv")

#for missing species, extract all values for the trait, and then can search for the relevant genera in the dataframe
disp1 <- extract_trait(`austraits-6.0.0`, "dispersal_syndrome")
disp2<-(disp1$traits)



#now we read in that edited file and append

dispersalv2 <- read_csv("data/dispersal_syndrome_persist_updated.csv")

master2 <-dplyr::left_join(master1a, dispersalv2, by = "taxon_name")


#next is growth form/habit

(`austraits-6.0.0` %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "plant_growth_form") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature field","preserved_specimen"))-> trait3

#this trait is a fully curated one (Wenk_2022), so we'll filter to that
trait3x<-dplyr::filter(trait3, dataset_id=="Wenk_2022")

#habit by species 
trait3x %>%         
  group_by(taxon_name) %>%
  summarize(growth_habit=value) ->trait3a


#see which species are in Austraits, but have no data for this trait 
check3<-data.frame(trait3x$taxon_name)

check3<-check3 %>% 
  rename(
    taxon_name = trait3x.taxon_name
  )

y3 <- setdiff(APC_sp, check3) #just one missing (was only accepted by APC recently, hence not in Wenk_2022)

#first though, write up the csv for this trait so I can manually collapse multiple values for same species
write_csv(trait3a,"data/growth_form_persist.csv")

#read back in
formv2 <- read_csv("data/growth_form_persist_updated.csv")

master3 <-dplyr::left_join(master2, formv2, by = "taxon_name")


#now life_history
(`austraits-6.0.0` %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "life_history") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature field","preserved_specimen"))-> trait4

#this trait is a fully curated one (Wenk_2023), so we'll filter to that
trait4x<-dplyr::filter(trait4, dataset_id=="Wenk_2023")

#life history by species 
trait4x %>%         
  group_by(taxon_name) %>%
  summarize(life_history=value) ->trait4a

#see which species are in Austraits, but have no data for this trait 
check4<-data.frame(trait4x$taxon_name)

check4<-check4 %>% 
  rename(
    taxon_name = trait4x.taxon_name
  )

y4 <- setdiff(APC_sp, check4)

#first though, write up the csv for this trait
write_csv(trait4a,"data/life_history_persist.csv")

#read back in
historyv2 <- read_csv("data/life_history_persist_updated.csv")

master4 <-dplyr::left_join(master3, historyv2, by = "taxon_name")


#next is photosynthetic pathway
(`austraits-6.0.0` %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "photosynthetic_pathway") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature field","preserved_specimen"))-> trait5

#photosynthesis by species 
trait5 %>%         
  group_by(taxon_name) %>%
  summarize(photosynthetic_pathway=value) ->trait5a

#see which species are in Austraits, but have no data for this trait 
check5<-data.frame(trait5$taxon_name)

check5<-check5 %>% 
  rename(
    taxon_name = trait5.taxon_name
  )

y5 <- setdiff(APC_sp, check5)

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait5a,"data/photosynthesis_persist.csv")

#also add missing ones here

#read back in
photov2 <- read_csv("data/photosynthesis_persist_updated.csv")

master5 <-dplyr::left_join(master4, photov2, by = "taxon_name")


#fire response
(`austraits-6.0.0` %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "resprouting_capacity") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature field","preserved_specimen"))-> trait6

#fire by species 
trait6 %>%         
  group_by(taxon_name) %>%
  summarize(resprouting_capacity=value) ->trait6a

#see which species are in Austraits, but have no data for this trait 
check6<-data.frame(trait6$taxon_name)

check6<-check6 %>% 
  rename(
    taxon_name = trait6.taxon_name
  )

y6 <- setdiff(APC_sp, check6)

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait6a,"data/fire_persist.csv")

#add missing and read back in


firev2 <- read_csv("data/fire_persist_updated.csv")

master6 <-dplyr::left_join(master5, firev2, by = "taxon_name")


#the final trait will be a 'water associated' one that I'm manually scoring based on online floras, so will do that in excel then load it in to append

water <- read_csv("data/water_persist.csv")

master7 <-dplyr::left_join(master6, water, by = "taxon_name")


#reappend columns from original list

master8<-dplyr::left_join(master7, sl, by = "taxon_name")

master9<-master8[,c(1,9,10,11,2,3,4,5,6,7,8)]

write_csv(master9,"data/persist_natives_master_traits.csv")

#I alphabetise this file in excel after
