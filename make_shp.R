make_shp <- function(flnm){

file <- flnm  #pick a file to load and make a point shapefile 
filename <- str_extract(file, "[^.]+") # "[^.]+" says match one or more charaters until .

#   
data <- read_rds(file)
#data <- mm_up_td  # or using 

  
#switching to dataframe to make shapefile
df <- data.frame(data)

drops <- "time"                                         #need to drop the "time" column because writeOGR can't handel class: hms
df <- df[ , !(names(df) %in% drops)]

coords <- cbind(df$GroupLong, df$GroupLat)     #creating coordinate list 
row.names(coords) <- 1:nrow(coords)               #adding index values to rows             
str(coords)                                          #compact summary of object                 

head(coords)

crs_var <- CRS("+proj=longlat +ellps=WGS84")                #adding WGS84 geoid, 'CRS' comes from sp or rgdal package
sp <- SpatialPoints(coords, proj4string = crs_var)    #turing CTD into SpatialPoints class object, S4
summary(sp)

bbox(sp)                              #returns bounding box
proj4string(sp)                       #returns the projection and ellispoid


# turn the data into a spatial points data frame with data frame and coordinates list
spdf <- SpatialPointsDataFrame(coords, df, proj4string = crs_var, match.ID = TRUE)     
names(spdf)


##### write spatial data frame to shapefile####
writeOGR(spdf, "C://Users//ealab//Documents//ArcGIS//AMAPPS//shp_from_R//mm_by_day", layer= str_c("XY", filename), driver="ESRI Shapefile", verbose=TRUE)

}