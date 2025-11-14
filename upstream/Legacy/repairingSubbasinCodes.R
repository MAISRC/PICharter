metadata_id = googledrive::drive_get("https://docs.google.com/spreadsheets/d/1iqJThuAjoMhRs1njvWPi_7pbszM-Dlvl933L6wsPR6k/edit?usp=sharing")$id
metadata_sheet = googlesheets4::read_sheet(ss = metadata_id,
                                           sheet = "submitted_metadata")

subbasin_subs = which(str_sub(metadata_sheet$DOW, 7, 8) != "00")
subbasin_subs = subbasin_subs[subbasin_subs < 1121]

for(i in length(subbasin_subs):1) {
  
  r = subbasin_subs[i]
  
  subbasin_code = str_sub(metadata_sheet$DOW[r], 7, 8)
  
  DOW2match = paste0(str_sub(metadata_sheet$DOW[r], 1, 6), "00")
  survey2match = format(metadata_sheet$SURVEY_DATE[[r]], "%Y-%m-%d")
  
  subbasin_data = current_db$subbasin[current_db$DOW == DOW2match &
                      current_db$SURVEY_START == survey2match]

  subbasin_code = unique(subbasin_data)
  
  if(length(subbasin_data) == 0) {
    #THESE DON'T SEEM TO MATCH ANY RECORDS...NOT SURE WHAT TO DO WITH THESE YET.
    secondaryDOW2match = metadata_sheet$DOW[r]
    
    subbasin_data = current_db$subbasin[current_db$DOW == DOW2match &
                                          current_db$SURVEY_START == survey2match]
    
    subbasin_code = unique(subbasin_data)
    
    if(length(subbasin_data) == 0) {
      print(r)
      print(paste(secondaryDOW2match, survey2match, sep = ", "))
      readline("This record is apparently not in our database!")
      next
    }

  }
  
  if(all(!is.na(subbasin_code))) {
    
    test = metadata_sheet %>% 
      select(DOW, SURVEY_DATE) %>% 
      mutate(SURVEY_DATE = format(SURVEY_DATE, "%Y-%m-%d")) %>% 
      filter(str_sub(DOW,1,6) == str_sub(metadata_sheet$DOW[r], 1, 6), SURVEY_DATE == survey2match)
    
    if(nrow(test == 1)) {
      subbasin_subs = subbasin_subs[-i]
      next
    }
    
    if(nrow(test == 2) & length(unique(subbasin_code)) == 2) {
      subbasin_subs = subbasin_subs[-i]
      next
    } 
    
    print("We're still in here somehow!")
    
  }

  if(any(!is.na(subbasin_code)) &
     any(is.na(subbasin_code))) {

    test = metadata_sheet %>% 
      select(DOW, SURVEY_DATE) %>% 
      mutate(SURVEY_DATE = format(SURVEY_DATE, "%Y-%m-%d")) %>% 
      filter(str_sub(DOW,1,6) == str_sub(metadata_sheet$DOW[r], 1, 6), SURVEY_DATE == survey2match)
    
    if(nrow(test == 2) & length(unique(subbasin_code)) == 2) {
      print("I think this is just a matter of joining in the missing subbasin...")
      current_db[current_db$DOW == DOW2match &
                            current_db$SURVEY_START == survey2match,] %>% 
        dplyr::select_if(~!is_mixed_excludable(.)) %>% 
        View()
      tmp1 = readline("Here's the survey records from the DOW in question.
                      There should really be two surveys in here, one with an NA for subbasin code.
                      Which subbasin code should we give for the survey lacking it?")
      
      if(isTruthy(tmp1)) {
        #WRONG!!! NEEDS TO TARGET ONLY THE NAS!
        # current_db$subbasin[current_db$DOW == DOW2match &
        #                       current_db$SURVEY_START == survey2match] = tmp1
        print("We inserted that subbasin! No need to consider this again so long as you save!")
        subbasin_subs = subbasin_subs[-i]
        next
      }
      
      next
    } else {
      print("I think this must mean the subbasin was insufficiently applied...?")
      next
    }

  }
  
  if(all(is.na(subbasin_code))) {
    
    test = metadata_sheet %>% 
      select(DOW, SURVEY_DATE) %>% 
      mutate(SURVEY_DATE = format(SURVEY_DATE, "%Y-%m-%d")) %>% 
      filter(str_sub(DOW,1,6) == str_sub(metadata_sheet$DOW[r], 1, 6), SURVEY_DATE == survey2match)
    
    if(nrow(test) == 1) {
      print("I think this is just a single, umambiguous record...")
      current_db$subbasin[current_db$DOW == DOW2match &
                            current_db$SURVEY_START == survey2match] = subbasin_code
      print("Made that amendment. We should no longer have to worry about this one so long as we save...")
      subbasin_subs = subbasin_subs[-i]
      next
    }
    
    if(nrow(test) > 1) {
      print(DOW2match)
      print(survey2match)
      readline("This one is a case where we have two different basins that will
               need to (probably) be matched on SUBMIT TIME or, if we're really
               unlucky, manually by row length. Take note!")
      next
    } 
    
    print("This didn't capture all outcomes!!!")
  }
  
  
}
