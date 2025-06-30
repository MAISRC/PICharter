#Take the results file we've made and, for now, get rid of any surveys that show any overlap in station number and/or the same length of records.
# results.comb = results.df %>% 
#   filter(!overlapping, !same_length)
results.comb = results.df

#Convert to date for sorting.
results.comb$SURVEY1 = as.Date(results.comb$SURVEY1)
results.comb$SURVEY2 = as.Date(results.comb$SURVEY2)

#Sort so that we go through records backwards in time so that we will ultimately end up with everything listed as the earliest start date.
results.comb = results.comb %>% 
  arrange(DOW, desc(SURVEY2))

records.overwritten = 0
# records.skipped = 0

##Remove any surveys that are known dupes

# grow.dat = grow.dat %>% 
#   filter(!(DOW == "27009501" & SURVEY_START == "2015-06-09"))

#Go through these records.
for(i in 1:nrow(results.comb)) {
  
  # if(i %% 25 == 0) { print(i) } 
  
  #Filter the first df to be just non-empty columns and from the current lake and the second start date.
  # curr.dat = grow.dat %>% 
  #   filter(DOW == results.comb$DOW[i],
  #          SURVEY_START == as.character(results.comb$SURVEY2[i])) %>% 
  #   dplyr::select_if(~any(!is.na(.) & 
  #                           . != 0 &
  #                           . != FALSE &
  #                           . != "FALSE" &
  #                           . != "" &
  #                           . != " "))
  # 
  # #Filter the second df to be just non-empty columns and from the current lake and the first start date.
  # curr.datb = grow.dat %>% 
  #   filter(DOW == results.comb$DOW[i],
  #          SURVEY_START == as.character(results.comb$SURVEY1[i])) %>% 
  #   dplyr::select_if(~any(!is.na(.) & 
  #                           . != 0 &
  #                           . != FALSE &
  #                           . != "FALSE" &
  #                           . != "" &
  #                           . != " "))
  
  #If the numbers of columns aren't the same and the numbers of rows aren't the same (and no station numbers overlap, otherwise we wouldn't even be here)...
  # if(ncol(curr.dat) != ncol(curr.datb) &&
  #    nrow(curr.dat) != nrow(curr.datb)) {
    
    #Overwrite the survey start date for all records from the second start date with the start date of the first survey instead
    grow.dat$SURVEY_START[grow.dat$SURVEY_START == as.character(results.comb$SURVEY2[i]) &
                            grow.dat$DOW == results.comb$DOW[i]
                          ] = as.character(results.comb$SURVEY1[i])
    
    #Track progress
    records.overwritten = records.overwritten + 1
  # } else {
  # 
  #   #Ok, even if their column numbers are the same, so long as the rows aren't the same and the species lists differ even a little, we can proceed with the overwrite...
  #   if(nrow(curr.dat) != nrow(curr.datb) &&
  #      !all(names(curr.dat) %in% names(curr.datb))) {
  #     
  #     grow.dat$SURVEY_START[grow.dat$SURVEY_START == as.character(results.comb$SURVEY2[i]) &
  #                             grow.dat$DOW == results.comb$DOW[i]
  #     ] = as.character(results.comb$SURVEY1[i])
  #     
  #     records.overwritten = records.overwritten + 1
  #     
  #   } else {
  #     
  #     records.skipped = records.skipped + 1
  #     
  #   }
    
  # }
  
}