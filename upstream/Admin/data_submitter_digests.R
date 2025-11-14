current_dbname = "upstream\\db_unified.parquet" #CURRENT FILE'S NAME
current_db = read_parquet(current_dbname) #READ

is_mixed_excludable = function(column) {
  all(column %in% c(0, FALSE, "FALSE", "0", NA), na.rm=T)
}

###COMPILE FOR SENDING ALL DATA IN DB FROM A GIVEN SUBMITTER.
create_sub_digest = function(submitter_name) {
  
  subset = current_db %>% 
    filter(SUBMITTER_NAME == submitter_name) %>% 
    mutate(NA_STA_NBR = ifelse(is.na(sta_nbr), TRUE, FALSE)) %>% 
    mutate(DUPE_STA_NBR = ifelse(duplicated(sta_nbr) | 
                                   duplicated(sta_nbr, fromLast = T), TRUE, FALSE), 
           MISSING_LAT = ifelse(is.na(latitude), TRUE, FALSE),
           MISSING_LONG = ifelse(is.na(longitude), TRUE, FALSE),
           .by = c(DOW, SURVEY_START, subbasin)) %>% 
    dplyr::select_if(~!is_mixed_excludable(.))
  
  write.csv(x = subset, file = paste0("upstream/Admin/PICharter_db_", submitter_name, ".csv"), row.names = F)
  
}
