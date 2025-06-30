#CONVERT UTM DATA INTO LAT LONGS
convertUTM_latlong = function(filename) {
  
  cleanfile4upload = read.csv(filename) #READ FILE
  
  #REPLACE EASTING W/ X
  if("easting" %in% names(cleanfile4upload)) {
    cleanfile4upload = rename(cleanfile4upload,
                              x = easting)
  }
  
  #REPLACE NORTHING W/ Y
  if("northing" %in% names(cleanfile4upload)) {
    cleanfile4upload = rename(cleanfile4upload,
                              y = northing)
  }
  
#CONVERT TO SF OBJECT
    utm_sf = st_as_sf(cleanfile4upload, 
                      coords = c("x", "y"),
                      crs = 26915)
    #TRANSFORM TO LAT LONG
    utm_sf = st_transform(utm_sf, crs=4236) 
    
    #GET THOSE COORDS
    latLongCoords = st_coordinates(utm_sf) 
    
    #CREATER LAT LONG COLUMNS
    utm_sf$latitude = NA
    utm_sf$longitude = NA 
    #FILL WITH THE NEW DATA
    utm_sf$latitude = latLongCoords[, "Y"] 
    utm_sf$longitude = latLongCoords[, "X"]
    #REMOVE SF OBJECT STATUS AND OLD X AND Y COLUMNS
    cleanfile4upload = st_drop_geometry(utm_sf) 
    cleanfile4upload$x = NULL 
    cleanfile4upload$y = NULL
    #REWRITE.
    write.csv(cleanfile4upload, filename)

}
  