# 2: Making shapefile for ArcGIS from mm_mm. You can get mm_mm from "cleaning_mm_sightings_1.R"

rm(list = setdiff(ls(), "mm_mm"))
#or
# mm_mm <- read_rds("mm_mm.rds")

library(sp) 
library(rgdal)

#Note - probably should check is mm_mm is in time order if starting with this file

#switching to dataframe to make shapefile
mm_df <- data.frame(mm_mm)

drops <- "time"                                         #need to drop the "time" column because writeOGR can't handel class: hms
mm_df <- mm_df[ , !(names(mm_df) %in% drops)]

mm_coords <- cbind(mm_df$GroupLong, mm_df$GroupLat)     #creating coordinate list 
row.names(mm_coords) <- 1:nrow(mm_coords)               #adding index values to rows             
str(mm_coords)                                          #compact summary of object                 

head(mm_coords)

crs_var <- CRS("+proj=longlat +ellps=WGS84")                #adding WGS84 geoid, 'CRS' comes from sp or rgdal package
mm_sp <- SpatialPoints(mm_coords, proj4string = crs_var)    #turing CTD into SpatialPoints class object, S4
summary(mm_sp)

bbox(mm_sp)                              #returns bounding box
proj4string(mm_sp)                       #returns the projection and ellispoid


# turn the data into a spatial points data frame with data frame and coordinates list
mm_spdf <- SpatialPointsDataFrame(mm_coords, mm_df, proj4string = crs_var, match.ID = TRUE)     
names(mm_spdf)


##### write spatial data frame to shapefile####
writeOGR(mm_spdf, "C://Users//ealab//Documents//ArcGIS//AMAPPS//shp_from_R", layer="XYmm_R_shp",driver="ESRI Shapefile", verbose=TRUE)

#Import the shapefile to file geodatabase, then convert DT from text to datetime using 'Convert Time Field'