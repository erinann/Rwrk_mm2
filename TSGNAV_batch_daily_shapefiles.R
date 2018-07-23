library(tidyverse)
library(lubridate)
library(stringr)
library(sp) 
library(rgdal)

#source("make_shp.R") #Have to manually source because of changing directory.

rm(list = ls())

# CHECK WHICH LEG YOU'RE WORKING ON! 
name <- "lg3"

TSGdata <- read_rds(str_c("TSG_", name, "_all_lonlat.rds"))

#adding date column to group by.
TSGdata <- TSGdata %>%
  mutate(date = date(DT))
head(TSGdata)

unq_dates <- TSGdata %>%
  group_by(date) %>%
  summarize(n())

setwd("C:\\Users\\ealab\\Documents\\WHOIcruise\\data\\Rwrk_NAVTSG\\dailyfiles")

# this saves .rds file for each day
TSGdata %>% 
  group_by(date) %>%
  do(write_rds(., paste0(unique(.$date), "_NAVTSG.rds"))) %>%
  ungroup()


filenames <- list.files(path = str_c("~\\WHOIcruise\\data\\Rwrk_NAVTSG\\dailyfiles"), pattern = ".rds")


setwd("C:\\Users\\ealab\\Documents\\WHOIcruise\\data\\Rwrk_NAVTSG\\")