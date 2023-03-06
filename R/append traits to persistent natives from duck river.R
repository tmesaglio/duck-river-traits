#note: I use all the same dataframe/object names as the extract extinct native R script, so don't run this in the same session as that one

library(tidyverse)
library(dplyr)

sl <- read_csv("species_lists.csv")

#filter to natives only
native <- dplyr::filter(sl, establishment_means=="native")

#separate into two files, Mesaglio and non-Mesaglio

mes <- dplyr::filter(native, source=="Mesaglio2022")
nonmes<- dplyr::filter(native, source!="Mesaglio2022")

#filter to species only, and remove duplicates
mes2 <-dplyr::select(mes, taxon_name)
nonmes2 <-dplyr::select(nonmes, taxon_name)

nonmes3<-dplyr::distinct(nonmes2)

#find common species, ie persisters
persist <- dplyr::inner_join(mes2, nonmes3, by = "taxon_name")

#I ran this list through the APC to get author info + family

APC<- read_csv("APC_metadata_persisters.csv")

#append some traits from austraits


#install austraits
remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits) 
austraits <- load_austraits(version = "4.1.0", path = "austraits")

#get traits

spvector <- APC$taxon_name

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


#see which species are in Austraits, but have no data for this trait 
check<-data.frame(trait1$taxon_name)

check<-check %>% 
  rename(
    taxon_name = trait1.taxon_name
  )

APC_sp <-dplyr::select(APC, taxon_name)

y <- setdiff(APC_sp, check)


#for almost all of the species missing height data here, I took measurements at Duck River. Added those externally, plus one from looking up online
height_append<- read_csv("height_append_persist.csv")

pre_master <-dplyr::select(trait1a, taxon_name, max_max_height)

master1 <-dplyr::bind_rows(pre_master, height_append)

#change mistletoe heights to NA
master1a <- master1 %>% 
  mutate(max_max_height=ifelse(taxon_name=="Muellerina eucalyptoides",NA,max_max_height),
         max_max_height=ifelse(taxon_name=="Amyema miquelii",NA,max_max_height))


#now let's do dispersal syndrome
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

#see which species are in Austraits, but have no data for this trait
check2<-data.frame(trait2$taxon_name)

check2<-check2 %>% 
  rename(
    taxon_name = trait2.taxon_name
  )

y2 <- setdiff(APC_sp, check2) 

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait2a,"data/dispersal_syndrome_persist.csv")

#whilst I'm directly editing that file, also add in the values for those 11 missing species (file y2)

#for some of these, there is no online info. So my method is to retrieve the dispersal syndrome for all other NSW species in the genus to compare with

#callistemon
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Callistemon brachyandrus","Callistemon subulatus","Callistemon pityoides","Callistemon sieberi","Callistemon linearis","Callistemon pallidus","Callistemon shiressii","Callistemon Callistemon pachyphyllus","Callistemon flavovirens","Callistemon viminalis","Callistemon rigidus","Callistemon acuminatus","Callistemon comboynensis","Callistemon montanus","Callistemon citrinus","Callistemon pungens")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> callistemon

#lagenophora
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Lagenophora brachyglossa","Lagenophora gracilis","Lagenophora montana","Lagenophora stipitata")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> lagenophora

#melaleuca
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Melaleuca acuminata","Melaleuca alternifolia","Melaleuca armillaris","Melaleuca biconvexa","Melaleuca bracteata","Melaleuca capitata","Melaleuca deanei","Melaleuca densispicata","Melaleuca diosmatifolia","Melaleuca ericifolia","Melaleuca glomerata","Melaleuca groveana","Melaleuca howeana","Melaleuca hypericifolia","Melaleuca interioris","Melaleuca irbyana","Melaleuca lanceolata","Melaleuca parvistaminea","Melaleuca quinquenervia","Melaleuca sieberi","Melaleuca squamea","Melaleuca squarrosa","Melaleuca thymifolia","Melaleuca tortifolia","Melaleuca trichostachya","Melaleuca uncinata")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> melaleuca

#pittosporum
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Pittosporum angustifolium","Pittosporum bicolor","Pittosporum erioloma","Pittosporum lancifolium","Pittosporum oreillyanum","Pittosporum spinescens","Pittosporum viscidum")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> pittosporum

#now we read in that edited file and append

dispersalv2 <- read_csv("dispersal_syndrome_persist_v2.csv")

master2 <-dplyr::left_join(master1a, dispersalv2, by = "taxon_name")


#next is growth form/habit

(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "plant_growth_form") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> trait3


#habit by species 
trait3 %>%         
  group_by(taxon_name) %>%
  summarize(growth_habit=value) ->trait3a

#see which species are in Austraits, but have no data for this trait 
check3<-data.frame(trait3$taxon_name)

check3<-check3 %>% 
  rename(
    taxon_name = trait3.taxon_name
  )

y3 <- setdiff(APC_sp, check3) #no missing species!

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait3a,"data/growth_form_persist.csv")

formv2 <- read_csv("growth_form_persist_v2.csv")

master3 <-dplyr::left_join(master2, formv2, by = "taxon_name")


#now life_history
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "life_history") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> trait4

#life history by species 
trait4 %>%         
  group_by(taxon_name) %>%
  summarize(life_history=value) ->trait4a

#see which species are in Austraits, but have no data for this trait 
check4<-data.frame(trait4$taxon_name)

check4<-check4 %>% 
  rename(
    taxon_name = trait4.taxon_name
  )

y4 <- setdiff(APC_sp, check4)

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait4a,"data/life_history_persist.csv")

#read back in
historyv2 <- read_csv("life_history_persist_v2.csv")

master4 <-dplyr::left_join(master3, historyv2, by = "taxon_name")


#next is photosynthetic pathway
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "photosynthetic_pathway") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> trait5

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
photov2 <- read_csv("photosynthesis_persist_v2.csv")

master5 <-dplyr::left_join(master4, photov2, by = "taxon_name")


#fire response
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% spvector) %>%
  filter(trait_name == "fire_response") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> trait6

#fire by species 
trait6 %>%         
  group_by(taxon_name) %>%
  summarize(fire_response=value) ->trait6a

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

#callistemon fire
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Callistemon brachyandrus","Callistemon subulatus","Callistemon pityoides","Callistemon sieberi","Callistemon linearis","Callistemon pallidus","Callistemon shiressii","Callistemon Callistemon pachyphyllus","Callistemon flavovirens","Callistemon viminalis","Callistemon rigidus","Callistemon acuminatus","Callistemon comboynensis","Callistemon montanus","Callistemon citrinus","Callistemon pungens")) %>%
  filter(trait_name == "fire_response") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> callistemonfire

#cheilanthes fire
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Cheilanthes austrotenuifolia","Cheilanthes distans","Cheilanthes lasiophylla")) %>%
  filter(trait_name == "fire_response") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> cheilanthesfire

#lagenophora fire
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Lagenophora brachyglossa","Lagenophora gracilis","Lagenophora montana","Lagenophora stipitata")) %>%
  filter(trait_name == "fire_response") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> lagenophorafire


firev2 <- read_csv("fire_persist_v2.csv")

master6 <-dplyr::left_join(master5, firev2, by = "taxon_name")


#the final trait will be a 'water associated' one that I'm manually scoring based on online floras, so will do that in excel then load it in to append

water <- read_csv("water_persist.csv")

master7 <-dplyr::left_join(master6, water, by = "taxon_name")


#now finally reappend the APC columns (family, genus, authors)

master8<-dplyr::left_join(master7, APC, by = "taxon_name")

master9<-master8[,c(1,9,10,11,2,3,4,5,6,7,8)]

write_csv(master9,"data/persist_natives.csv")
