library(tidyverse)

sl <- read_csv("species_lists.csv")

#trying ausflora package
#remotes::install_github("traitecoevo/ausflora")
library(ausflora)
sl <- read_csv("species_lists.csv")
sl$species_f <- iconv(sl$species, "UTF-8", "UTF-8", sub = 'x')
aligned_data <- align_taxa(sl$species_f)



#a bit of manipulation
unique(sl$source)
sl$source_lumped <- case_when(sl$source == "Mesaglio2022" ~ "recent",
                              TRUE ~ "1979-1992")

#getting basic summary of species richness
sl %>%
  group_by(source_lumped, establishment_means) %>%
  summarize(num_species = n_distinct(species)) -> sr_summary

sr_summary %>%
  ggplot(aes(x = establishment_means, fill = source_lumped, y = num_species)) +
  geom_col(position = "dodge") + theme_bw()
ggsave("figures/species_richness_summary.pdf")

#adding column for status
sl %>%
  group_by(species) %>%
  summarize(how_many_surveys = n_distinct(source_lumped)) -> persistence_summary

sl %>%
  left_join(persistence_summary) %>%
  mutate(
    status = case_when(
      how_many_surveys == "2" ~ "persistent",
      how_many_surveys == "1" &
        source_lumped == "1979-1992" ~ "extirpated",
      how_many_surveys == "1" & source_lumped == "recent" ~ "coloniser"
    )
  ) -> sl2

sl2 %>%
  filter(!grepl("sp\\.", species)) %>% # hack for now; change this later
  group_by(status, establishment_means) %>%
  summarise(n = n_distinct(species)) -> gs

gs %>%
  ggplot(aes(x = establishment_means, fill = status, y = n)) +
  geom_col(position = "dodge") + theme_bw()
ggsave("figures/dynamics_summary.pdf")
