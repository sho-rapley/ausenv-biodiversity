# Threatened species map for 2023 report

# library
pacman::p_load(janitor, sf, terra, tidyterra, tidyverse)

# processed listing data
list <- read.csv("epbc_processed_2023.csv")

# taxonomic changes
taxon <- read.csv("epbc_taxonomy.csv")

# TS public grid shapefiles
ts <- vect("TS_grids/SNES_Public.shp")

# align taxonomy with processed list
ts2 <- ts %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  clean_names() %>%
  subset(!is.na(threatened)) %>%
  mutate(species = ifelse(scientific %in% taxon$Listed, taxon$Accepted, scientific),
         species = gsub(r"{\s*\([^\)]+\)}","", species),
         species = str_squish(species),
         species = gsub("subsp. ", "", species),
         species = gsub(" MS", "", species),
         species = gsub(" ms", "", species),
         species = ifelse(species %in% taxon$Listed, taxon$Accepted, species), #run again to reinstate needed ()
         species = ifelse(listed_tax==68751, "Carcharias taurus (east coast population)", species),
         species = ifelse(listed_tax==68752, "Carcharias taurus (west coast population)", species),
         species = ifelse(listed_tax==75184, "Dasyurus maculatus maculatus (SE mainland population)", species),
         species = ifelse(listed_tax==75183, "Dasyurus maculatus maculatus (Tasmanian population)", species),
         sci2 = scientific)

# compare lists
diff <- data.frame(differ = setdiff(ts2$species, list$species)) %>%
  arrange(differ)



ggplot()+
  geom_spatvector(data= ts[1:10,], aes(fill=scientific))
