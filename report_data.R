library(lubridate)
library(tidyverse)

rm(list = ls())
mm_mm <- read_rds("mm_mm.rds")

report_data <- read_csv("HB1103_cet_numbers_from_2011_report.csv")

#removing extra rows that were extraneously brought in
report_data <- na.omit(report_data)

report_data <- report_data %>%
  mutate(hi_lo_grp_diff = upper_g - lower_g,  
         hi_lo_ind_diff = upper_i - lower_i)

ggplot(report_data) +
  geom_col(aes(com_name, hi_lo_grp_diff))

more_upper <- report_data %>%
  filter(hi_lo_grp_diff > 0)

more_lower <- report_data %>%
  filter(hi_lo_grp_diff < 0)

View(more_lower)
# Compared to the Upper Team, the Lower Team has:
# 2 more sightings of common dolphins, 5 more sightings of fin/sei, 
# 1 more sightings of Mn, 1 more sighting of S. attenuata (upper didn't have a sighting),
# and 12 more sightins of unid. whale.

# Going to look at an analysis of just using the Upper Team's data to reduce confusion
# over duplicate sightings.