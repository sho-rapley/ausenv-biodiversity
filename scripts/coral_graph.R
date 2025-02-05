# Manta tow data hard coral cover AIMS
# reference: Australian Institute of Marine Science (AIMS). 2015, AIMS Long-term Monitoring Program: Crown-of-thorns starfish (Acanthaster planci) and benthos Manta Tow Data (Great Barrier Reef), https://doi.org/10.25845/5c09b0abf315a
# doi: https://doi.org/10.25845/5c09b0abf315a

# library
pacman::p_load(janitor, tidyverse)

# coral reef zones (north, central, southern) by sector- not using but i'll keep anyway
zone <- data.frame(
  sector = c("CG", "PC", "CL",
             "CA", "IN", "TO", "CU", "WH",
             "PO", "SW", "CB"),
  zone = c(rep("north",3),
           rep("central",5),
           rep("south",3))
  )

# 1993-2022 data
# downloaded from https://apps.aims.gov.au/metadata/view/5bb9a340-4ade-11dc-8f56-00008a07204e
coral_past <- read.csv("data/manta-tow-1993-2022.csv") %>%
  clean_names() %>%
  group_by(report_year) %>%
  summarise(coral_cover = mean(mean_live_coral),
            sd = sd(mean_live_coral),
            reef_count = length(report_year),
            ci = as.numeric(1.96*(sd/sqrt(reef_count))))

# 2023 data - manually imported from the reef data viewer by sector 
# https://apps.aims.gov.au/reef-monitoring/sector/list 
coral_2023 <- read.csv("data/manta-tow-2023.csv") %>%
  clean_names() %>%
  group_by(report_year) %>%
  summarise(coral_cover = mean(live_hard_coral_cover),
            sd = sd(live_hard_coral_cover),
            reef_count = length(report_year),
            ci = as.numeric(1.96*(sd/sqrt(reef_count))))

# add 2023 to previous data
coral <- rbind(coral_past, coral_2023)

# write out 
write.csv(coral, "coral_processed_2023.csv", row.names = FALSE)

# plot
ggplot(data=coral)+
  geom_ribbon(aes(ymin = coral_cover - ci, 
                  ymax = coral_cover + ci, 
                  x = report_year), fill = "skyblue", alpha = .4)+
  geom_point(data = subset(coral, report_year==2023), aes(report_year, coral_cover))+
  geom_line(aes(report_year, coral_cover))+
  theme_classic()+
  ylim(0, 50)
