# 3: Working in ArcGIS (it's a visual and measuring thing) and R. Removing "Orginal/Follow" sighting to retain 1 entry per sighting.

library(tidyverse) #tidyverse is: ggplot2, tibble, tidyr, readr, purrr, dplyr
library(lubridate)
library(geosphere)

#rm(list = setdiff(ls(), "mm_mm"))
rm(list = ls())
mm_mm <- read_rds("mm_mm.rds")

#check columns
names(mm_mm)
glimpse(mm_mm)

mm_up <- mm_mm %>%
  filter(team == "up")

#survey days with sightings
survey_days_stng <- mm_mm %>%
  group_by(date) %>%
  summarize(n_distinct(date))

mm_up %>%
  group_by(date, sighttype) %>%
  summarize(n_distinct(st_num))

# Making sure follows make sense. Calculating m/s (travel speed for critter) that would be needed between sightings
# and follow-ons.
mm_up_dup_speed <- mm_up %>%
  group_by(UID2) %>%
  filter(n() > 1) %>% #filtering out the single UIDs created above (can't do dist and time difference with itself)
  select(-glare_mag, -swell_angl, -swell_ht) %>%
  arrange(DT_UTC) %>%
  mutate(dist = c(NA,
                  distHaversine(cbind(GroupLong[-n()], GroupLat[-n()]),
                                cbind(GroupLong[  -1], GroupLat[  -1]))),
         t_diff = c(NA, difftime(DT_UTC[ -1], DT_UTC[-n()], units = "secs")),
         m_per_sec = dist/t_diff) %>%
  ungroup()

# save data as .rds and go to general_make_shapefile.R to make shapefile.
write_rds(mm_up_dup_speed, "mm_up_dup_speed.rds")


mm_up_single <- mm_up %>%
  group_by(UID2) %>%
  filter(n() == 1) %>% #filtering out the single UIDs created above (can't do dist and time difference with itself)
  select(-glare_mag, -swell_angl, -swell_ht) %>%
  mutate(dist = -9999,
         t_diff = -9999,
         m_per_sec = -9999) %>%
  ungroup()

mm_up2 <- bind_rows(mm_up_dup_speed, mm_up_single)

# save data as .rds and go to general_make_shapefile.R to make shapefile.
write_rds(mm_up2, "mm_up2.rds")

rm(list = setdiff(ls(), c("mm_mm", "mm_up2")))

# Based on Roha et al. 2002, max swim speeds for Tt, Dd, Pc is no more than 10 m/sec.
# Went through mm_up2 shapefile by hand (checking speeds) to see if the m/sec attribute made
# sense and if groupings made sense. Some don't. 

UID_to_split <- c(328, 73, 75, 105, 211, 328, 435, 886, 951, 1021, 1442, 1546)

prob_UIDs <- mm_up2 %>%
  filter(UID2 %in% UID_to_split)

prob_UIDs %>% 
  group_by(UID2) %>%
  summarize(n())
# A tibble: 11 x 2
#UID2 `n()`
#<dbl> <int>
#1    73     4
#2    75     2
#3   105     2
#4   211     3
#5   328     3
#6   435     4
#7   886     3
#8   951     2
#9   1021     4
#10  1442     4
#11  1546     3

mm_up2 %>%
  summarize(minU = min(UID2, na.rm = TRUE),
            maxU = max(UID2, na.rm = TRUE))
# min = 1, max = 3206

#mm_test <- mm_mm %>%
#  select(rowid, old_UID2, st_num, corspcs, UID2) %>%
#  filter(UID2 %in% UID_to_split)


mm_up3 <- mm_up2 %>%
  mutate(UID3 = UID2) %>%
  mutate(UID3 = case_when(rowid == 575 ~ 3300,
                          rowid == 577 | rowid == 579 ~ 3301,
                          rowid == 117 | rowid == 123 ~ 3302,
                          rowid == 118 ~ 3303,
                          rowid == 125 ~ 3304,
                          rowid == 127 ~ 3305, 
                          rowid == 128 ~ 3306,
                          rowid == 190 ~ 3307,
                          rowid == 192 ~ 3308,
                          rowid == 391 | rowid == 394 ~ 3309,
                          rowid == 396 ~ 3310,
                          rowid == 575 ~ 3311,
                          rowid == 577 | rowid == 579 ~ 3312,
                          rowid == 789 | rowid == 791 | rowid == 792 ~ 3313,
                          rowid == 795 ~ 3314,
                          rowid == 1694 ~ 3315,
                          rowid == 1695 | rowid == 1696 ~ 3316,
                          rowid == 1786 ~ 3317,
                          rowid == 1788 ~ 3318,
                          rowid == 1871 | rowid == 1874 | rowid == 1879 ~ 3319,
                          rowid == 1877 ~ 3320,
                          rowid == 161 | rowid == 162 | rowid == 167 ~ 3321,
                          rowid == 166 ~ 3322,
                          rowid == 248 ~ 3323,
                          rowid == 249 | rowid == 250 ~ 3324,
                          TRUE ~ UID3))

write_rds(mm_up3, "mm_up3.rds")

# for groups 
mm_up4 <- mm_up3 %>%
  group_by(UID3) %>%
  filter(perp == min(perp)) %>%
  select(-dist, -t_diff, -m_per_sec)

write_rds(mm_up4, "mm_up4.rds")

ggplot(mm_up4, aes(perp)) +
  geom_histogram()

numUIDs <- mm_up3 %>% group_by(UID3) %>%
  summarize(n = n())
  
filter(mm_up3, UID3 == 137)

# summarize by leg and species
st_summary <- mm_up4 %>% group_by(leg, corspcs) %>% summarize(n())

