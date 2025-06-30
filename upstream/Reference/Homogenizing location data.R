#RIGHT NOW, WE ACCEPT LOCATION DATA IN TWO MAIN FORMS: LAT/LONG, AND UTM (ZONE 15N). IT WOULD BE MUCH MORE CONVENIENT IF WE ONLY KEPT LOCATION DATA IN A SINGLE FORMAT. IN PARTICULAR, IT WOULD BE PREFERABLE TO KEEP IT AS LAT/LONG DATA, AS THESE ARE EASIEST TO WORK WITH WHEN TRYING TO GRAPH DATA VIA LEAFLET. 
#SO, IN THIS FILE, I WILL TRANSFER ALL OUR UTM DATA OVER INTO THE ASSOCIATED LAT-LONG DATA USING SF.

current_db = read_parquet("H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet")

no_loc_dat = current_db %>% 
  filter(is.na(x) & is.na(y) & is.na(latitude) & is.na(longitude))

loc_dat = current_db %>% 
  filter(!is.na(x) | !is.na(y) | !is.na(latitude) | !is.na(longitude))

lat_long_dat = loc_dat %>% 
  filter(!is.na(latitude) & !is.na(longitude))

no_lat_long_dat = loc_dat %>% 
  filter(is.na(latitude) | is.na(longitude))

utm_dat = no_lat_long_dat %>%  #There's honestly not much...
  filter(!is.na(x) & !is.na(y))

#WE'RE JUST GOING TO ASSUME EVERYTHING WAS CRS 26915. I KNOW SOMETHING WAS SUPPOSED TO BE 32615 BUT THOSE RECORDS DON'T SEEM TO HAVE UTM DATA? IT'S WEIRD...

utm_sf = st_as_sf(utm_dat, coords = c("x", "y"), crs=26915) #MAKE THE SF OBJECT

utm_sf = st_transform(utm_sf, crs=4236) #TURN IT INTO LAT/LONG CRS

latLongCoords = st_coordinates(utm_sf) #PULL THE LAT/LONG COLS OUT.

utm_sf$latitude = latLongCoords[, "Y"] #PLACE THOSE DATA WHERE THEY OUGHT TO GO
utm_sf$longitude = latLongCoords[, "X"]

utm_df = st_drop_geometry(utm_sf) #ELIMINATE GEOMETRY

new_db = bind_rows(utm_df, lat_long_dat, no_loc_dat) #RECOMBINE THE SUB-DATASETS.

#CHECK TO ENSURE SUCCESSFULNESS
length(which((
  !is.na(new_db$x) & !is.na(new_db$y))
  & (is.na(new_db$latitude) & is.na(new_db$longitude))
  ))

#ELIMINATE COLUMNS THAT ARE NO LONGER NEEDED.
new_db$x = NULL
new_db$y = NULL
new_db$utm_crs = NULL

write_parquet(new_db, sink = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet") #WRITE
