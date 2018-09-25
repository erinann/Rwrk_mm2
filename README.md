# Rwrk_mm2
This is R code to clean marine mammal sighting data specific to the data file.

Work through in order.
1. mm_1_first_cleaning_sightings.R
  Load the data file and do lots of housekeeping. 
  
2. mm_2_making_ALL_shapefile_2.R
  Making a shapefile to load in to ArcGIS. This is old code using 'sp'. It should be updated to 'sf' at some point. This step is more of a hold over from my ArcGIS days.  
  
3. mm_3_filtering_upper.R
  Checking that "follow-on" points are actually follow-ons and not new sightings. 
  
4. mm_4_batch_mm_shapefiles.R
  Batch shapefiles by day.
