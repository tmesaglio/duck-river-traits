library(tidyverse)
library(dplyr)

#read in two files: extinct and persist

extinct <- read_csv("extinct_natives.csv")
persist <- read_csv("persist_natives.csv")

#add a new column for each (extinct or persist)
extinct['status']='extinct'
persist['status']='persister'

#rename a column for consistency
extinct <- dplyr::rename(extinct, growth_habit = growth_form)

#combine df
combined <- dplyr::bind_rows(extinct, persist)

#tidy up some other column headings etc
combined <- dplyr::rename(combined, "maximum_height_metres" = max_max_height)
combined <- dplyr::rename(combined, "dispersal_syndrome" = dispersal)
combined <- combined %>% arrange(taxon_name)
combined[203, 3] = "Microtis" 
combined[203, 4] = "Orchidaceae" 

#AMEND ONE VALUE AFTER RECTIFIED HERBARIUM ID: DROSERA PELTATA IS NOW A PERSISTER, NOT EXTINCT
combined[94, 12] = "persister"

#write
write_csv(combined,"data/final_master_list.csv")


#SOME MORE ADJUSTMENTS (ADDED 28 JUNE) TO REFLECT SOME CHANGES I MADE TO SPECIES LISTS
#read edited file in with some deletions and changes
#change 1 is deletion of Hypericum gramineum (done )
combined_pre_new <- read_csv("final_master_list_updated.csv")

#change 2 is adding traits for Euchiton involucratus instead of E. japonicus
library(austraits) 
austraits <- load_austraits(version = "4.1.0", path = "austraits")

(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Euchiton involucratus") %>%
  filter(trait_name == "plant_height") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen") & #preserved specimen is flora data
           value_type=="maximum") -> heightEI
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Euchiton involucratus") %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> dispersalEI
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Euchiton involucratus") %>%
  filter(trait_name == "life_history") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> lifeEI
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Euchiton involucratus") %>%
  filter(trait_name == "photosynthetic_pathway") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> photoEI
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Euchiton involucratus") %>%
  filter(trait_name == "plant_growth_form") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> growthEI
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Euchiton involucratus") %>%
  filter(trait_name == "fire_response") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> fireEI

#add these to df
combined_new0 <- combined_pre_new %>% add_row(taxon_name = "Euchiton involucratus",APC_name = "Euchiton involucratus (G.Forst.) Holub",genus = "Euchiton",family = "Asteraceae",maximum_height_metres = 0.5,dispersal_syndrome = "anemochory",growth_habit = "herb",life_history = "perennial",photosynthetic_pathway = "c3",fire_response = "fire_killed",water_association = "no",status = "persister")

#change 3 is add Melaleuca thymifolia traits
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Melaleuca thymifolia") %>%
  filter(trait_name == "plant_height") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen") & #preserved specimen is flora data
           value_type=="maximum") -> heightMT
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Melaleuca thymifolia") %>%
  filter(trait_name == "dispersal_syndrome") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> dispersalMT
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Melaleuca thymifolia") %>%
  filter(trait_name == "life_history") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> lifeMT
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Melaleuca thymifolia") %>%
  filter(trait_name == "photosynthetic_pathway") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> photoMT
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Melaleuca thymifolia") %>%
  filter(trait_name == "plant_growth_form") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> growthMT
(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% "Melaleuca thymifolia") %>%
  filter(trait_name == "fire_response") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> fireMT

#dispersal was missing, add manually

combined_new <- combined_new0 %>% add_row(taxon_name = "Melaleuca thymifolia",APC_name = "Melaleuca thymifolia Sm.",genus = "Melaleuca",family = "Myrtaceae",maximum_height_metres = 2,dispersal_syndrome = "anemochory",growth_habit = "shrub",life_history = "perennial",photosynthetic_pathway = "c3",fire_response = "resprouting_possible",water_association = "no",status = "persister")

write_csv(combined_new,"data/final_master_list_updated2.csv")
#this latest csv now becomes Supplementary Table 4 for the paper

#t-test height
#first check data distribution
hist(combined_new$maximum_height_metres)

#heavily right skewed, so log transform first
combined_new <- dplyr::mutate(combined_new, log_height = log(maximum_height_metres))

t.test(log_height ~ status, data = combined_new, var.equal = TRUE)



#chi square tests for traits
cont_table1 <- table(combined_new$dispersal_syndrome, combined_new$status)
chisq_result1 <- chisq.test(cont_table1)

cont_table2 <- table(combined_new$growth_habit, combined_new$status)
chisq_result2 <- chisq.test(cont_table2)

cont_table3 <- table(combined_new$life_history, combined_new$status)
chisq_result3 <- chisq.test(cont_table3)

cont_table4 <- table(combined_new$photosynthetic_pathway, combined_new$status)
chisq_result4 <- chisq.test(cont_table4)

cont_table5 <- table(combined_new$fire_response, combined_new$status)
chisq_result5 <- chisq.test(cont_table5)

cont_table6 <- table(combined_new$water_association, combined_new$status)
chisq_result6 <- chisq.test(cont_table6)

#plot these
library(ggstatsplot)
library(ggplot2)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


p1 <- ggbarstats(
  data = combined_new,
  x = dispersal_syndrome,
  y = status
) +
  labs(caption = NULL) + theme_classic() + scale_fill_manual(values=cbPalette)

p1

extract_stats(p1)

p2<-ggbarstats(
  data = combined_new,
  x = growth_habit,
  y = status
) +
  labs(caption = NULL)+ theme_classic() + scale_fill_manual(values=cbPalette)

p2

extract_stats(p2)

p3<-ggbarstats(
  data = combined_new,
  x = life_history,
  y = status
) +
  labs(caption = NULL)+ theme_classic() + scale_fill_manual(values=cbPalette)

p3

extract_stats(p3)

p4<-ggbarstats(
  data = combined_new,
  x = photosynthetic_pathway,
  y = status
) +
  labs(caption = NULL)+ theme_classic() + scale_fill_manual(values=cbPalette)

p4

extract_stats(p4)

p5<-ggbarstats(
  data = combined_new,
  x = fire_response,
  y = status
) +
  labs(caption = NULL)+ theme_classic() + scale_fill_manual(values=cbPalette)

p5

extract_stats(p5)

p6<-ggbarstats(
  data = combined_new,
  x = water_association,
  y = status
) +
  labs(caption = NULL)+ theme_classic() + scale_fill_manual(values=cbPalette)

p6

extract_stats(p6)


#out of interest, check photosynthetic pathway of new invasive grasses
library(tidyverse)
library(dplyr)

sl <- read_csv("species_lists.csv")

#filter to non-natives only
nonnative <- dplyr::filter(sl, establishment_means=="non-native")

#separate into two files, Mesaglio and non-Mesaglio

mes <- dplyr::filter(nonnative, source=="Mesaglio2022")
nonmes<- dplyr::filter(nonnative, source!="Mesaglio2022")

#filter to species only, and remove duplicates
mes2 <-dplyr::select(mes, taxon_name)
nonmes2 <-dplyr::select(nonmes, taxon_name)

nonmes3<-dplyr::distinct(nonmes2)

#find new invaders
invade <- dplyr::setdiff(mes2, nonmes3)

#pull out just grasses
grasses<-dplyr::slice(invade,27,31,32,33,60,66,67,76,87,110)

#append some traits from austraits

library(austraits) 
austraits <- load_austraits(version = "4.1.0", path = "austraits")

(austraits %>% join_all)$traits %>%
  filter(taxon_name %in% c("Digitaria didactyla","Ehrharta erecta","Eragrostis pilosa","Eragrostis tenuifolia","Lolium rigidum","Megathyrsus maximus","Melinis repens","Paspalum urvillei","Polypogon viridis","Vulpia muralis","Eragrostis curvula","Cenchrus clandestinus","Cynodon dactylon","Avena barbata","Briza subaristata","Bromus catharticus","Digitaria sanguinalis","Ehrharta longiflora","Lolium multiflorum","Lolium perenne","Setaria parviflora","Setaria pumila","Sorghum halepense")) %>%
  filter(trait_name == "photosynthetic_pathway") %>%
  filter(life_stage=="adult" & 
           basis_of_record %in% c("field","literature","literature, field","preserved_specimen"))-> grassphoto

#note that I did the invaders plus other abundant species
