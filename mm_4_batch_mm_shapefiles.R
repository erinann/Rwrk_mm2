library(tidyverse)
library(lubridate)
library(stringr)
library(sp) 
library(rgdal)

#source("make_shp.R") #Have to manually source because of changing directory.

#rm(list = setdiff(ls(), "mm_mm"))
rm(list = ls())
mm <- read_rds("mm_up4.rds")


unq_dates <- mm %>%
  group_by(date) %>%
  summarize(n())


setwd("C:\\Users\\ealab\\Documents\\WHOIcruise\\data\\Rwrk_mm2\\date_sightings")

# this saves .rds file for each day
mm %>% 
  group_by(date) %>%
  do(write_rds(., paste0(unique(.$date), "_mmst.rds"))) %>%
  ungroup()


filenames <- list.files(path = str_c("~\\WHOIcruise\\data\\Rwrk_mm2\\date_sightings"), pattern = ".rds")

lapply(filenames, make_shp)

setwd("C:\\Users\\ealab\\Documents\\WHOIcruise\\data\\Rwrk_mm2\\")