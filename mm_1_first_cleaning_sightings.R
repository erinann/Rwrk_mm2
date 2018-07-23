# 1: Importing mm sightings data. Removing non-marine mammal sightings. 
# Pooling Kogia species and pilot whale species.


library(tidyverse)
#tidyverse is:#ggplot2
              #tibble
              #tidyr
              #readr
              #purrr
              #dplyr
library(lubridate)


#dir()

rm(list = ls())

mm <- read_csv("mm_st_all_20171222.csv")
mm300 <- read_csv("mm_4_300km.txt")

mm$date <- mdy(mm$date)
mm$team <- factor(mm$team)
mm$sighttype <- factor(mm$sighttype)
mm$corspcs <- factor(mm$corspcs)
mm$Behavior <- factor(mm$Behavior)
mm$keep <- factor(mm$keep)


  # sf dataframe. quick check to see if ship positions look okay
# ship_loc_mmst <- st_as_sf(mm, coords = c("ShipLong", "ShipLat"), crs = 4326)
#   #EPSG 4326 = WGS84, or crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# plot(ship_loc_mmst["corspcs"])  # quick look to make sure we have straight lines from the ship's track.
# 
# sum(is.na(mm$GroupLong)); sum(is.na(mm$GroupLat))# checking to see if any lon/lat is empty
# 
#   ## sf dataframe. quick check to see if sightings look okay
# mm_st <- st_as_sf(mm, coords = c("GroupLong", "GroupLat"), crs = 4326)
# plot(mm_st["corspcs"])  # quick look to make sure we do not have straight lines from the ship's track.


glimpse(mm)
mm_orginal_names <- mm$corspcs

summary(mm$corspcs) 
#fix names: Rissos dolphin -> Risso's dolphin
#           Cuviers beaked whale -> Cuvier's beaked whale
#           Sowerbys beaked whale -> Sowerby's beaked whale
#           Long-finned pilot whale -> Pilot whales           #not enough sightings to seperate
#           Short-finned pilot whale -> Pilot whales          #not enough sightings to seperate
#           Pygmy sperm whale -> Kogi sp.                     #after plotting (GIS) no clear spatial seperation, pooling sightings.
#           Dwarf sperm whale -> Kogi sp.                     #after plotting (GIS) no clear spatial seperation, pooling sightings.
#           Pygmy/Dwarf sperm whale -> Kogi sp.               #after plotting (GIS) no clear spatial seperation, pooling sightings.
#Remove:    Mola mola (sunfish)
#           Tuna sp.
#           Turtle, leatherback
#           unid. dolphin
#           unid. medium whale
#           unid. shark
#           unid. small whale
#           Unknown
#           Basking shark
#Keep:      Unid. large whale
#           Unid. Mesoplondant
#           Unid. Ziphiid

corspcs <- mm$corspcs 

cor <- recode(corspcs, 'Rissos dolphin' = "Risso's dolphin", 'Cuviers beaked whale' = "Cuvier's beaked whale",
                       'Sowerbys beaked whale' = "Sowerby's beaked whale", 'Gervais beaked whale'= "Gervais' beaked whale",
                       'Long-finned pilot whale' = "Pilot whales", 'Short-finned pilot whale' = "Pilot whales", 
                       'Pygmy sperm whale' = "Kogia sp.", 'Dwarf sperm whale' = "Kogia sp.", 'Pygmy/Dwarf sperm whale' = "Kogia sp.")
mm$corspcs <- cor  
summary(mm$corspcs)

mm_list <- c("Atlantic spotted dolphin", "Bottlenose dolphin", "Clymene dolphin", "Common dolphin", "Cuvier's beaked whale", "Kogia sp.", 
             "Fin whale", "Fin/Sei whale", "Gervais' beaked whale", "Harbor porpoise", "Humpback whale", "Killer whale", "Pilot whales", 
             "Minke whale", "Pantropical spotted dolphin", "Risso's dolphin", "Rough-toothed dolphin", "Sei whale", "Sowerby's beaked whale", 
             "Sperm whale", "Stenella sp.", "Striped dolphin", "unid. large whale", "Unid. Mesoplondant", "Unid. Ziphiid", "White-sided dolphin")

mm_mm <- mm %>%
  filter(corspcs %in% mm_list)

mm_mm$corspcs <- factor(mm_mm$corspcs) #dropping factor levels
summary(mm_mm$corspcs)

# checking data type
head(mm_mm$date); head(mm_mm$time)

#checking 
mm_mm %>% 
  select(date, time) %>%
  mutate(DT_local = ymd_hms(paste(date, time), tz = "US/Eastern"))

#adding rowid, adding old_UID2 (int) and change UID2 (dbl), adding DT_local and putting in time-order, adding DT_UTC, adding leg numbers 
#legs: (1) June 2 - June 22. (2) June 27 - July 15, (3) July 20 - Aug 1

mm_mm <- mm_mm %>% 
  rownames_to_column(var = "rowid") %>%
  rename(old_UID2 = UID2) %>%
  mutate(UID2 = as.numeric(old_UID2), 
         DT_local = ymd_hms(paste(date, time), tz = "US/Eastern")) %>%
  arrange(DT_local) %>% # to put in time-order
  mutate(DT_UTC = with_tz(DT_local, tzone = "UTC"), 
         leg = case_when(date <= ymd("2011-06-22") ~ 1,
                         date >= ymd("2011-06-27") & date <= ymd("2011-07-15") ~ 2,
                         date >= ymd("2011-07-20") ~ 3))

glimpse(mm_mm)

#looking for sightings with more than 1 species listed
st_multiple_sp <- mm_mm %>% group_by(old_UID2, st_num) %>% distinct(corspcs) %>% 
  filter(n() > 1)

rowids_need_new_UIDs <- mm_mm %>% filter(old_UID2 %in% st_multiple_sp$old_UID2)

#need to give the 2-species sightings different UID2s so they don't get deleted
table(rowids_need_new_UIDs$rowid, rowids_need_new_UIDs$UID2) 

mm_mm2 <- mm_mm %>%
  select(old_UID2, UID2, rowid, team, sighttype) %>%
  mutate(UID2 = case_when(
    rowid == 513 ~ 287.5,
    rowid == 1680 ~ 870.5,
    rowid == 1854 ~ 1009.5, 
    rowid == 1149 ~ 2314.5,
    TRUE ~ as.numeric(UID2)))

#check that UID2 values are unique
filter(mm_mm2, old_UID2 %in% st_multiple_sp$old_UID2)
#good - no overlaps with teams and all multispecies sightings are initial sightings.

#for realz now
mm_mm <- mm_mm %>%
  mutate(UID2 = case_when(
    rowid == 513 ~ 287.5,
    rowid == 1680 ~ 870.5,
    rowid == 1854 ~ 1009.5,
    rowid == 1149 ~ 2314.5,
    TRUE ~ as.numeric(UID2)))

#cleaing up the environment
rm(list = setdiff(ls(), "mm_mm"))

survey_days_stng <- mm_mm %>%
  group_by(date) %>%
  summarize(n_distinct(date))

write_rds(mm_mm, "mm_mm.rds")


# NEXT: Go to makeing_mm_shapefile_2.R


