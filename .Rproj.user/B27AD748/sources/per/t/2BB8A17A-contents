#read in data file of 'extinct' native species from Mesaglio 2023

library(tidyverse)
library(dplyr)

sl <- read_csv("new_traits_extinct.csv")



#append some traits from austraits


#install austraits
remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask", force = TRUE)

library(austraits) 
austraits::load_austraits(version = "5.0.0")

#for now, line 17 isn't working for me for some reason (it's working for other users), so here's a workaround
#download the RDS file from https://zenodo.org/records/10156222, rename it to 'austraits', then manually click on it in file explorer to open it in Rstudio

#get traits

spvector <- sl$taxon_name


#first is maximum plant height
#all field adult records
(austraits %>% join_all)$traits %>%
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

#get height data for those from online sources and append
height_append<- read_csv("height_append.csv")

pre_master <-dplyr::select(trait1a, taxon_name, max_max_height)

master1 <-dplyr::bind_rows(pre_master, height_append)

#correct error for Arthropodium minus
master1[5, 2] = 0.5


#now let's do dispersal syndrome
#all field adult records
(austraits %>% join_all)$traits %>%
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
check2<-data.frame(trait2x$taxon_name)

check2<-check2 %>% 
  rename(
    taxon_name = trait2x.taxon_name
  )

y2 <- setdiff(APC_sp, check2) 

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait2a,"data/dispersal_syndrome.csv")

#whilst I'm directly editing that file, also add in the values for those 8 missing species (file y2)

#for some of these, there is no online info. So my method is to retrieve the dispersal syndrome for all other NSW species in the genus to compare with

#to do this, we extract all values for the trait, and then can search for the relevant genera in the dataframe
disp1 <- extract_trait(austraits, "dispersal_syndrome")
disp2<-(disp1$traits)


#now we read in that edited file and append

dispersalv2 <- read_csv("dispersal_syndrome_updated.csv")

master2 <-dplyr::left_join(master1, dispersalv2, by = "taxon_name")

#next is growth form/habit

(austraits %>% join_all)$traits %>%
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

y3 <- setdiff(APC_sp, check3)

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait3a,"data/growth_form.csv")

#also tack on habit for two missing species here, then read back in and append
formv2 <- read_csv("growth_form_updated.csv")

master3 <-dplyr::left_join(master2, formv2, by = "taxon_name")


#now life_history
(austraits %>% join_all)$traits %>%
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

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait4a,"data/life_history.csv")


#also tack on value for two missing species here, then read back in and append
historyv2 <- read_csv("life_history_updated.csv")

master4 <-dplyr::left_join(master3, historyv2, by = "taxon_name")

#next is photosynthetic pathway
(austraits %>% join_all)$traits %>%
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
write_csv(trait5a,"data/photosynthesis.csv")

#for missing species with info not found in literature, repeat step as above for dispersal (check other genera)
phot1 <- extract_trait(austraits, "photosynthetic_pathway")
phot2<-(phot1$traits)

#now load in updated file

photov2 <- read_csv("photosynthesis_updated.csv")

master5 <-dplyr::left_join(master4, photov2, by = "taxon_name")

#fire response
(austraits %>% join_all)$traits %>%
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
write_csv(trait6a,"data/fire.csv")

#call to check genera for some missing species
fir1 <- extract_trait(austraits, "resprouting_capacity")
fir2<-(fir1$traits)

#load edited file
firev2 <- read_csv("fire_updated.csv")

master6 <-dplyr::left_join(master5, firev2, by = "taxon_name")


#the final trait will be a 'water associated' one that I'm manually scoring based on online floras, so will do that in excel then load it in to append

water <- read_csv("water.csv")

master7 <-dplyr::left_join(master6, water, by = "taxon_name")

#now finally reappend the APC columns (family, genus, authors)

master8<-dplyr::left_join(master7, sl, by = "taxon_name")

master9<-master8[,c(1,9,10,11,2,3,4,5,6,7,8)]

write_csv(master9,"data/extinct_natives_master_traits.csv")

#I alphabetise this file in excel after 
