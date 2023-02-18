
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
