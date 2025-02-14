# Threatened species map for 2023 report

# library
pacman::p_load(janitor, ozmaps, paletteer, sf, terra, tidyterra, tidyverse)



# TS public grid shapefiles - not in directory because too large.
# Available at: https://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B337B05B6-254E-47AD-A701-C55D9A0435EA%7D
ts <- vect("TS_grids/SNES_Public.shp") %>%
  clean_names() %>%
  filter(threatened %in% c("Critically Endangered", "Conservation Dependent",
                           "Vulnerable", "Endangered")) %>%
  project(crs(ibra))

# count terrestrial threatened species by ibra subregion
sp_terra <- ts %>%
  filter(is.na(marine) | marine == "Listed - overfly marine area")

ibra_ts <- terra::intersect(sp_terra, ibra)

ibra_ts2 <- ibra_ts %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  select(c("scientific", "reg_name")) %>%
  distinct() %>%
  group_by(reg_name) %>%
  summarise(ts_count = length(scientific))

# count marine species
sp_marine <- ts %>%
  filter(marine == "Listed") %>%
  select(scientific) %>%
  distinct()

# create polygon for marine species
marine <- vect(ext(c(110, 155, -45, -9)), crs(ibra)) %>%
  erase(ibra) %>%
  st_as_sf() %>%
  mutate(reg_name = "Marine",
         ts_count = nrow(sp_marine)) %>%
  vect()
  
# combine terrestrial and marine polygons
ibra_ts3 <- left_join(ibra, ibra_ts2) %>%
  rbind(marine)

temp <- st_as_sf(ibra_ts3)

# save polygons
writeVector(ibra_ts3, "ibra_ts/ibra_ts.shp", filetype = "ESRI Shapefile")

# map (playing with colour options)
ggplot()+
  geom_spatvector(data = ibra_ts3, aes(fill = ts_count))+
  #paletteer::scale_fill_paletteer_c("grDevices::BrBG", direction = 1)+
  #scale_fill_gradient2(low = "black", mid = "white", high = "#68001DFF", midpoint = 50)+
  scale_fill_viridis_c(option = "inferno", direction = -1)+
  theme_void()

# count species by islands and states and territories
ts_df <- ts %>%
  st_as_sf() %>%
  st_drop_geometry()

juris <- c("ACI", "CKI", "CI", "CSI", "NFI", "HMI", "AAT", 
             "TAS", "NSW", "JBT", "QLD", "VIC", "WA", "SA", "NT", "ACT")
ts_juris <- data.frame()

for(i in 1:length(juris)){
   sp <- ts_df %>%
    filter(grepl(juris[i], regions)) %>%
    distinct(scientific)
   
   temp <- data.frame(jurisdiction = juris[i],
                          ts = nrow(sp))
   
   ts_juris <- rbind(ts_juris, temp)
}



#' Next part is to make the interactive map data. 
#' come back to the section below this later, 
#' probably instead exporting each species as a file or stacked netcdf. 
#' Currently it's for loops to extract species by year. 
#' 
# processed listing data
list <- read.csv("epbc_processed_2023.csv")

# taxonomic changes
taxon <- read.csv("epbc_taxonomy.csv")

# align taxonomy and nomenclature with processed list
ts2 <- ts %>%
  st_as_sf() %>%
  clean_names() %>%
  subset(!is.na(threatened)) %>%
  filter(!threatened == "Extinct in the wild") %>%
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

temp <- data.frame(original = ts2$scientific, 
                   new = ts2$species,
                   common = ts2$vernacular)

#' note to self: there's currently a problem with the taxonomy mutate, it's misaligned


# base raster- from previous rasterized biodiversity data
fish <- rast("Fishes.nc")
base <- rast(extent = ext(fish), resolution = res(fish))

# list of categories to rasterise
cats <- c("Critically Endangered", "Endangered", "Vulnerable")

# rasterise threatened species by year
years <- 2000:2023

out <- base

for(i in 1:24){
    sub <- subset(list, year == years[7] & status %in% cats)
    if(nrow(sub)<1){
      next
    }
    sub2 <- sub$species
    temp <- subset(ts2, species %in% sub2) %>%
      rasterize(base, fun = "count") 
    names(temp) <- as.character(years[i])
    print(names(temp))
    out <- c(out, temp)
}

# rasterise threatened species by status and year
years <- 2000:2023
cats <- c("Critically Endangered", "Endangered", "Vulnerable")
out <- base

for(i in 1:24){
  for(j in 1:3){
    sub <- subset(list, year == years[i] & status == cats[j])
    if(nrow(sub)<1){
      next
    }
    sub2 <- sub$species
    temp <- subset(ts2, species %in% sub2) %>%
      rasterize(base, fun = "count", background = 0) 
    names(temp) <- as.character(paste(cats[j], years[i], sep = "_"))
    print(names(temp))
    out <- c(out, temp)
  }
}

# replace NAs with zeroes
out2 <- subst(out, from = NA, to = 0)

# plot
ggplot()+
  geom_spatraster(data= out2)+
  facet_wrap(~lyr)
  scale_fill_viridis_c()+
  theme_classic()+
  geom_spatvector(data=test, aes(fill="yellow"))

# test plotting range restricted species
test <- filter(ts2, species == "Galaxias johnstoni")

ggplot()+
  #geom_spatraster(data= out2$'2000')+
  scale_fill_viridis_c()+
  theme_classic()+
  geom_spatvector(data=test, colour = "red")+
  coord_sf(xlim = c(146.1, 146.6), ylim = c(-42.4, -41.9))
