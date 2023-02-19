
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

#find the differences, ie native extinctions
extinct <-dplyr::setdiff(nonmes3, mes2)

#I ran this list through the APC to get author info + family

APC<- read_csv("APC_metadata.csv")


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
write_csv(trait2a,"data/dispersal_syndrome.csv")

#whilst I'm directly editing that file, also add in the values for those 8 missing species (file y2)

#for some of these, there is no online info. So my method is to retrieve the dispersal syndrome for all other NSW species in the genus to compare with

#dysphania
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Dysphania ambrosioides","Dysphania carinata","Dysphania cristata","Dysphania glomulifera","Dysphania kalpari","Dysphania melanocarpa","Dysphania multifida","Dysphania plantaginella","Dysphania platycarpa","Dysphania pumilio","Dysphania rhadinostachya","Dysphania simulans","Dysphania truncata")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> dysphania

#cyperus

cyperus_names <- read_csv("cyperus_names.csv")
cypvector <- cyperus_names$taxon_name

(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% cypvector) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> cyperus

#isotoma
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Isotoma anethifolia","Isotoma axillaris","Isotoma petraea","Isotoma tridens")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> isotoma

#juncus
juncus_names <- read_csv("juncus_names.csv")
junvector <- juncus_names$taxon_name

(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% junvector) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> juncus

#lomandra
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Lomandra bracteata","Lomandra brevis","Lomandra collina","Lomandra confertifolia","Lomandra cylindrica","Lomandra effusa","Lomandra elongata","Lomandra filiformis","Lomandra glauca","Lomandra gracilis","Lomandra hystrix","Lomandra laxa","Lomandra leucocephala","Lomandra longifolia","Lomandra micrantha","Lomandra montana","Lomandra multiflora","Lomandra obliqua","Lomandra patens","Lomandra spicata")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> lomandra

#tricoryne
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Tricoryne elatior","Tricoryne anceps")) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> tricoryne

#there are also 4 species listed as zoochory, but since we have 3 other syndromes that are nested in that (epi, endo, myrmechory), I will get more specific for them

#need to do a check for brachyscome
brachyscome_names <- read_csv("brachyscome_names.csv")
bravector <- brachyscome_names$taxon_name

(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% bravector) %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> brachyscome

#now we read in that edited file and append

dispersalv2 <- read_csv("dispersal_syndrome_v2.csv")

master2 <-dplyr::left_join(master1, dispersalv2, by = "taxon_name")

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

y3 <- setdiff(APC_sp, check3)

#first though, write up the csv for this trait so I can manually collapse rows (multiple values for same species)
write_csv(trait3a,"data/growth_form.csv")

#also tack on habit for single missing species here (Microtis), then read back in and append
formv2 <- read_csv("growth_form_v2.csv")

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
write_csv(trait4a,"data/life_history.csv")

#also tack on value for single missing species here (Microtis), then read back in and append
historyv2 <- read_csv("life_history_v2.csv")

master4 <-dplyr::left_join(master3, historyv2, by = "taxon_name")
