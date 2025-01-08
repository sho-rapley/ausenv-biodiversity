# cane toad spread map 2023 report
# library
pacman::p_load(galah, janitor, ozmaps, sf, smoothr, terra, tidyterra, tidyverse)

# background map
oz <- ozmap("states") %>%
  vect() %>%
  project(crs("EPSG:4326"))

# galah config
galah_config(email = "shoshana.rapley@anu.edu.au")

# call data
toads_ala <- galah_call() %>%
  galah_identify("Rhinella marina") %>%
  galah_select(group = "basic",
               basisOfRecord,
               coordinateUncertaintyInMeters,
               occurrenceStatus) %>%
  atlas_occurrences() %>%
  clean_names()

# save ala data so I dont need to repeat the download call
write.csv(toads_ala, "Rhinella_marina_ala.csv", row.names = FALSE)

# apply additional filtering 
toads <- read.csv("Rhinella_marina_ala.csv") %>%
  filter(occurrence_status == "PRESENT", # remove absences
         basis_of_record %in% c("OCCURRENCE", "OBSERVATION", #obs only
                                "HUMAN_OBSERVATION","MACHINE_OBSERVATION"),
         !is.na(event_date), #remove no date
         !is.na(decimal_longitude),
         !is.na(decimal_latitude),
         !str_ends(1, pattern = "000"), # remove dubious coords
         !str_ends(2, pattern = "000"),
         !coordinate_uncertainty_in_meters>2000) %>% # remove >2km uncertainty
  mutate(year = year(event_date))

# rasterise by time bin, convert to concave hull and smooth range map
base_raster <- rast(ncol=62.5, nrow=52.3, xmin=105.5, xmax=168, ymin=-43, ymax=-9.3)
ch_ratio <- c(.25, .22, .15, .11)
yearA <- c(1970, 2000, 2020, 2024)
yearB <- c(1900, 1970, 2000, 2020)
toad_maps <- data.frame()

for(i in 1:4){
  temp <- subset(toads, year<yearA[i] & year>=yearB[i]) %>%
    select(c(5,4)) %>%
    data.matrix() %>%
    rasterize(base_raster, fun = "max") %>%
    as.polygons() %>%
    st_as_sf() %>%
    drop_crumbs(threshold = units::set_units(10000, km^2)) %>% # drop single cell polygons
    st_concave_hull(ratio = ch_ratio[i]) %>% # variable concave hull ratio by time period
    smooth(method = "ksmooth") %>%
    vect() %>%
    crop(oz) %>%
    st_as_sf()  %>%
    mutate(bin = paste("pre", yearA[i], sep = "_"),
           factor = i ) %>%
    select(!max)
  
  toad_maps <- rbind(toad_maps, temp)
}

toad_maps <- arrange(toad_maps, -factor)

# plot
ggplot()+
  geom_sf(data = toad_maps, aes(fill = bin), colour = NA)+
  geom_sf(data = oz, fill = NA)+
  theme_void()+
  theme(legend.position = c(.85, .5))+
  #geom_point(data = toads, aes(decimal_longitude, decimal_latitude))
  scale_fill_manual(values = c("wheat", "tan2", "darkorange3","brown"),
                     labels = c("Pre 1970", "1970-1999",
                                "2000-2019", "2020-2023"))+
  labs(fill = "Time Period")
  

