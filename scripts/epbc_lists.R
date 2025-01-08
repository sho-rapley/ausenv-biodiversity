# EPBC list wrangling for Aus Env Biodiversity report
# last updated 30/01/2024 Shoshana Rapley

# set up
pacman::p_load(janitor, tidyverse, readxl)

# notes on data sources:
#' Current list (29/1/24): manually formatted in long format; I used the currently accepted 
#' sci name when two were listed and removed naming authority.
#' flora: https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl?wanted=flora 
#' fauna: https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl?wanted=fauna
#' 
#' Changes to the list (29/1/24): 'text to columns' function in excel used to split common and binomial names
#' https://www.environment.gov.au/cgi-tmp/publiclistchanges.309265b1b3529d5bf13e.html 
#' 
#' Removals from the lis (1/2/22): "true" removals i.e. not taxonomic or info reasons were
#' manually sorted by reading the listings
#' https://www.environment.gov.au/cgi-tmp/publiclistchanges.407465b1b51cc727df3e.html

# Make table for initial listings of later removed species
initial <- read_excel("epbc_listings_to2023.xlsx", sheet = "Removals") %>%
  clean_names() %>%
  subset(grepl("Increase", reason) | grepl("Relisted", reason)) %>%
  select(c("group","species","name","previous_status","year_first_listed")) %>%
  rename(status = previous_status) %>%
  rename(effective = year_first_listed) %>%
  # add an initial listing date
  mutate(effective = gsub("2000",as_date("16/07/2000", format = "%d/%m/%Y"), effective))

# Make table for initial listings of later transferred species
transfers <- read_excel("epbc_listings_to2023.xlsx", sheet = "Changes") %>%
  clean_names() %>%
  filter(str_detect(status, "Transfer")) %>%
  mutate(status = word(status, start=3L)) %>%
  mutate(status = ifelse(str_detect(status, "Critically"),
                         "Critically Endangered", status)) %>%
  mutate(status = ifelse(str_detect(status, "Conservation"),
                         "Conservation Dependent", status)) %>%
  mutate(effective = as_date("16/07/2000", format = "%d/%m/%Y"))

# Join all data
epbc <- read_excel("epbc_listings_to2023.xlsx", sheet = "Fauna") %>%
  rbind(read_excel("epbc_listings_to2023.xlsx", sheet = "Flora")) %>%
  rbind(read_excel("epbc_listings_to2023.xlsx", sheet = "Changes")) %>%
  rbind(read_excel("epbc_listings_to2023.xlsx", sheet = "Delist")) %>%
  clean_names() %>%
  rbind(transfers) %>%
  rbind(initial) %>%
  arrange(species, status) %>%
  mutate(effective = as_date(effective)) %>%
  # standardise nomenclature and syntax, remove whitespace
  mutate(species = str_trim(species,side = c("both")))

# Lost some group data due to non standing naming so retrieving it here
group <- select(epbc, c(group, species)) %>% arrange(group) %>%
  filter(!duplicated(species)) %>% arrange(species)
  
epbc <- right_join(group, select(epbc, -(group)), by= "species") %>%
# Remove duplicates where transferred
  mutate(order = ifelse(str_detect(status, "Transfer"),1,2)) %>%
  arrange(order) %>%
  filter(!duplicated(cbind(species, effective))) %>%
  arrange(species) %>%
# Remove false initial listings
  arrange(desc(effective)) %>%
  filter(!duplicated(cbind(species, status))) %>%
  arrange(species) %>%
# Create column for year listed
  mutate(year = year(effective)) %>%
# Create column for listing type 
  mutate(listing = ifelse(grepl("to Extinct",status),"Transfer extinct",
                   ifelse(grepl("Transfer from Extinct",status),"Rediscovered",
                   ifelse(grepl("Extinct",status),"Extinct",
                   ifelse(grepl("Transfer",status),"Transfer",
                   ifelse(grepl("Delisted",status),"Delisted", "Listed")))))) %>%
  arrange(status) %>%
  subset(select = -c(order))

# Export 
write.csv(epbc, "epbc_processed_2023.csv", row.names = FALSE)

# Manual tasks in exported file:
#' 1) Check correct listings of the 2 species that were delisted and relisted
#' which seems to mess with the system:
#' a) Macroderma gigas	Ghost Bat (true de and re listing)
#' Mammal	Macroderma gigas	Vulnerable	Ghost Bat	16/07/2000	2000	Listed
#' b) Amytornis textilis myall	Western Grasswren (true de and re listing)
#' Bird	Amytornis textilis myall	Vulnerable	Western Grasswren 	16/07/2000	2000	Listed
#' 
#' 2) Check listing of CI Pipistrelle as there seems to be a timeline issue in the official data;
#' should go endangered 2001 -> uplist CI 2006 -> uplist extinct 2021
#' 
#' 3) Manually check for duplicated common names (using conditional formatting) to find taxonomic synonyms to combine;
#' usually occurs for species that were transferred but originally listed after 2000. Usually the initial listing
#' has the outdated name (and therefore the group hasn't been brought across for the new name either).
#' 
#' 4) Check duplicated listings in species where naming isn't consistent on the listings:
#' a) Genoplesium vernale AKA Corunastylis vernalis
#' b) Galaxias truttaceus (Western Australian population)
#' c) Phascolarctos cinereus (combined populations of Qld, NSW and the ACT)
#' d) Vappodes phalaenopsis AKA Dendrobium bigibbum (Cooktown orchid)
#' 
#' 5) missing species: Macrozamia platyrhachis, listed endangered 2000
#' 
#' In 2024 should be able to append the list changes to the 2023 processed list. 

## Populate list for completeness by calendar year

# read in data
epbc <- read.csv("epbc_processed_2023.csv") %>%
  mutate(status2 = gsub("Transfer from ..* to ", "", status),
         year = as.numeric(year)) %>%
  subset(!year==2024)

# add column for number of times listed
epbc_count <- epbc %>%
  group_by(species) %>% 
  summarise(n = n()) 
epbc <- left_join(epbc, epbc_count)

# add years to 2023 for single listed species
sp <- unique(epbc$species)
epbc1 <- subset(epbc, n==1)
epbc1_full <- data.frame()

for(i in 1:length(sp)){
  temp <- filter(epbc1, species == sp[i])
  if(length(temp$year)==0) {
    next
  }
  yrs <- data.frame(species = sp[i],
                    year2 = temp$year:2023)
  out <- left_join(temp, yrs)
  epbc1_full <- rbind(epbc1_full, out)
}

# add years to listing change and to 2023 for species that changed status
epbc2 <- subset(epbc, n==2)
epbc2_dup <- distinct(epbc2, species, .keep_all = TRUE)
epbc2_full <- data.frame()

for(i in 1:length(sp)){
  temp <- filter(epbc2, species == sp[i])
  if(nrow(temp)==0) {
    next
  }
  status <- unique(temp$status2)
  listed <- unique(temp$year)
  change1 <- data.frame(species = sp[i],
                        status2 = status[1],
                       year2 = listed[1]:(listed[2]-1))
  change2 <- data.frame(species = sp[i],
                        status2 = status[2],
                        year2 = listed[2]:2023)
  changes <- rbind(change1, change2)
  line <- filter(epbc2_dup, species == sp[i]) %>%
    select(!status2)
  out <- left_join(line, changes, by = join_by(species))
  epbc2_full <- rbind(epbc2_full, out)
}

# add years to listing change and to 2023 for species that changed status twice
epbc3 <- subset(epbc, n==3)
epbc3_dup <- distinct(epbc3, species, .keep_all = TRUE)
epbc3_full <- data.frame()

for(i in 1:length(sp)){
  temp <- filter(epbc3, species == sp[i])
  if(nrow(temp)==0) {
    next
  }
  status <- temp$status2
  listed <- unique(temp$year)
  change1 <- data.frame(species = sp[i],
                        status2 = status[1],
                        year2 = listed[1]:(listed[2]-1))
  change2 <- data.frame(species = sp[i],
                        status2 = status[2],
                        year2 = listed[2]:(listed[3]-1))
  change3 <- data.frame(species = sp[i],
                        status2 = status[3],
                        year2 = listed[3]:2023)
  changes <- rbind(change1, change2, change3)
  line <- filter(epbc3_dup, species == sp[i]) %>%
    select(!status2)
  out <- left_join(line, changes, by = join_by(species))
  epbc3_full <- rbind(epbc3_full, out)
}

# rediscoveries
redisc <- unique(filter(epbc, listing=="Rediscovered")$species)

# combine all
epbc_all <- rbind(epbc1_full, epbc2_full, epbc3_full) %>%
  # remove years where delisted
  filter(!status2=="Delist") %>%
  select(c(group, species, name, status2, year2)) %>%
  rename(status = status2,
         year = year2) %>%
  # remove extinction status of rediscovered species
  mutate(temp = ifelse(species %in% redisc & status == "Extinct", 1,0)) %>%
  filter(temp==0) %>%
  select(!temp)

# write out
write.csv(epbc_all, "epbc_processed_2023.csv", row.names = FALSE)
