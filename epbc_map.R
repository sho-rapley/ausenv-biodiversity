# Threatened species map for 2023 report

# library
pacman::p_load(janitor, sf, terra, tidyterra, tidyverse)

# processed listing data
list <- read.csv("epbc_processed_2023.csv") %>%
  mutate(status = gsub("Transfer from ..* to ", "", status))

# taxonomic changes
taxon <- read.csv("epbc_taxonomy.csv")

# TS public grid shapefiles - not in directory because too large.
# Available at: https://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B337B05B6-254E-47AD-A701-C55D9A0435EA%7D
ts <- vect("TS_grids/SNES_Public.shp")

# align taxonomy and nomenclature with processed list
ts2 <- ts %>%
  st_as_sf() %>%
  clean_names() %>%
  subset(!is.na(threatened)) %>%
  mutate(species = ifelse(scientific %in% taxon$Listed, taxon$Accepted, scientific),
         species = gsub(r"{\s*\([^\)]+\)}","", species),
         species = str_squish(species),
         species = gsub("subsp. ", "", species),
         species = gsub(" MS", "", species),
         species = gsub(" ms", "", species),
         species = ifelse(species %in% taxon$Listed, taxon$Accepted, species), #run again to reinstate genuine parentheses
         species = ifelse(listed_tax==68751, "Carcharias taurus (east coast population)", species),
         species = ifelse(listed_tax==68752, "Carcharias taurus (west coast population)", species),
         species = ifelse(listed_tax==75184, "Dasyurus maculatus maculatus (SE mainland population)", species),
         species = ifelse(listed_tax==75183, "Dasyurus maculatus maculatus (Tasmanian population)", species))

# 2023 updates subset
ts_23 <- subset(ts2, year == 2023)

ggplot()+
  geom_sf(data = ts_23, aes(fill = species))
