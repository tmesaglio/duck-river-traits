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

#write
write_csv(combined,"data/final_master_list.csv")


#t-test height
#first check data distribution
hist(combined$maximum_height_metres)

#heavily right skewed, so log transform first
combined <- dplyr::mutate(combined, log_height = log(maximum_height_metres))

t.test(log_height ~ status, data = combined, var.equal = TRUE)



#chi square tests for traits
cont_table1 <- table(combined$dispersal_syndrome, combined$status)
chisq_result1 <- chisq.test(cont_table1)

cont_table2 <- table(combined$growth_habit, combined$status)
chisq_result2 <- chisq.test(cont_table2)

cont_table3 <- table(combined$life_history, combined$status)
chisq_result3 <- chisq.test(cont_table3)

cont_table4 <- table(combined$photosynthetic_pathway, combined$status)
chisq_result4 <- chisq.test(cont_table4)

cont_table5 <- table(combined$fire_response, combined$status)
chisq_result5 <- chisq.test(cont_table5)

cont_table6 <- table(combined$water_association, combined$status)
chisq_result6 <- chisq.test(cont_table6)

#plot these
library(ggstatsplot)
library(ggplot2)


p1 <- ggbarstats(
  data = combined,
  x = dispersal_syndrome,
  y = status
) +
  labs(caption = NULL)

p1

extract_stats(p1)

p2<-ggbarstats(
  data = combined,
  x = growth_habit,
  y = status
) +
  labs(caption = NULL)

p2

extract_stats(p2)

p3<-ggbarstats(
  data = combined,
  x = life_history,
  y = status
) +
  labs(caption = NULL)

p3

extract_stats(p3)

p4<-ggbarstats(
  data = combined,
  x = photosynthetic_pathway,
  y = status
) +
  labs(caption = NULL)

p4

extract_stats(p4)

p5<-ggbarstats(
  data = combined,
  x = fire_response,
  y = status
) +
  labs(caption = NULL)

p5

extract_stats(p5)

p6<-ggbarstats(
  data = combined,
  x = water_association,
  y = status
) +
  labs(caption = NULL)

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
