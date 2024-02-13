# Manta tow data hard coral cover AIMS
# reference: Australian Institute of Marine Science (AIMS). 2015, AIMS Long-term Monitoring Program: Crown-of-thorns starfish (Acanthaster planci) and benthos Manta Tow Data (Great Barrier Reef), https://doi.org/10.25845/5c09b0abf315a
# doi: https://doi.org/10.25845/5c09b0abf315a

# library
pacman::p_load(janitor, tidyverse)

# coral reef zones (north, central, southern) by sector
zone <- data.frame(
  sector = c("CG", "PC", "CL",
             "CA", "IN", "TO", "CU", "WH",
             "PO", "SW", "CB"),
  zone = c(rep("north",3),
           rep("central",5),
           rep("south",3))
  )

# 1993-2022 data
coral_past <- read.csv("manta-tow-by-reef.csv") %>%
  clean_names() %>%
  left_join(zone, by = "sector") %>%
  group_by(report_year, zone) %>%
  summarise(coral_cover = mean(mean_live_coral),
            sd = sd(mean_live_coral),
            reef_count = length(report_year),
            ci = as.numeric(1.96*(sd/sqrt(reef_count))))

# 2023 data
coral_2023 <- read.csv("manta-tow-2023.csv") %>%
  clean_names() %>%
  left_join(zone, by = "sector") %>%
  group_by(report_year, zone) %>%
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
  geom_line(aes(report_year, coral_cover))+
  geom_point(aes(report_year, coral_cover))+
  theme_classic()+
  ylim(0, 50)+
  facet_wrap(~zone)
            