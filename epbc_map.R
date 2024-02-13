# Threatened species map for 2023 report

# library
pacman::p_load(janitor, sf, terra, tidyterra, tidyverse)

# TS public grid shapefiles
ts <- vect("TS_grids/SNES_Public.shp") %>%
  st_as_sf() %>%
  clean_names() %>%
  mutate(species = gsub(r"{\s*\([^\)]+\)}","", as.character(scientific))) %>%
  mutate(species = gsub("subsp. ", "", scientific))

ggplot()+
  geom_spatvector(data= ts[1:10,], aes(fill=scientific))


temp <- data.frame(unique(ts$species))

# processed listing data
list <- read.csv("epbc_processed_2023.csv")

# compare lists
diff <- data.frame(differ = setdiff(unique(ts$species), list$species))

ammend <- data.frame(old = c("Plectranthus habrophyllus"),
                     new = c("Coleus habrophyllus"))