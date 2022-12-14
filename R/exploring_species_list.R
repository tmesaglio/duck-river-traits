library(tidyverse)

sl<-read_csv("species_lists.csv")

#trying ausflora package
remotes::install_github("traitecoevo/ausflora")
library(ausflora)
tmp <- dataset_access_function("0.0.0.9000")
aligned_data <- align_taxa(sl$species)

sl<-read_csv("species_lists.csv")
sl$species_f<-iconv(sl$species, "UTF-8", "UTF-8",sub='x') 
aligned_data <- align_taxa(sl$species_f)



#a bit of plotting
unique(sl$source)
sl$source_lumped<-case_when(sl$source=="Mesaglio2022" ~ "recent",
          TRUE ~ "1979-1992")

sl %>%
  group_by(source_lumped,establishment_means) %>%
  summarize(num_species=n_distinct(species))->sr_summary

ggplot(sr_summary,aes(x=establishment_means,fill=source_lumped,y=num_species))+
  geom_col(position="dodge")+theme_bw()

library(taxonlookup)
?plant_lookup
lt<-lookup_table(unique(sl$species),missing_action="NA",by_species = TRUE)

lt2<-  rownames_to_column(lt,var="species")

sl2<-left_join(sl,lt2)

sl2 %>%
  group_by(source_lumped,establishment_means,group) %>%
  summarize(num_species=n_distinct(species))->sr_summary2

a<-filter(sl2,is.na(group))

ggplot(sr_summary2,aes(x=establishment_means,fill=source_lumped,y=num_species))+
  geom_col(position="dodge")+theme_bw()+facet_grid(group~.,scales = "free_y")
