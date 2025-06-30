##HIVE THE ORIGINAL DB FILE INTO THE GOOGLE DRIVE STRUCTURE THAT THE RECORDS TAB THEN ACCESSES

#LOAD PACKAGES
library(tidyverse)
library(arrow)

#READ IN CLEANED UP LEGACY DB
data2parquet = read_parquet("H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet")

#CREATE MAIN GDRIVE FOLDER
main_path = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Big Data\\PI_data_parqueted"
dir.create(main_path, recursive = T) #recursive = T creates the directory if it doesn't exist

#LOOP THRU EACH UNIQUE LAKE
uniq_lakes = sort(unique(data2parquet$DOW))

#STATUS COUNTER
status_counter = 0

for(lake in uniq_lakes) {
  
  #Progress counter
  status_counter = status_counter + 1
  if(status_counter %% 100 == 0) { print(paste0("Now on lake ", status_counter, " of ", length(uniq_lakes))) }
  
  #CREATE SUB-DIRECTORY FOR THIS LAKE'S FILES
  dir.create(paste0(main_path, "\\", lake), recursive = T, showWarnings = FALSE)
  
  #SUBSET THE MAIN DATASET TO RECORDS FROM THIS LAKE
  data_subset = data2parquet %>% 
    filter(DOW == lake)
  
  #DETERMINE THE NUMBER OF UNIQUE SURVEY FILES WE HAVE FROM THIS LAKE
  uniq_surveys = sort(unique(data_subset$SURVEY_START))
  
  #LOOP THROUGH EACH UNIQUE SURVEY
  for(date in uniq_surveys) {
    
    #FILTER TO THIS DATE'S DATA
    data_subset_survey = data_subset %>% 
      filter(SURVEY_START == date)
    
    if(nrow(data_subset_survey) > 0 ) {
      
      #CREATE A SUB-FILE FOR THIS ONE SURVEY'S DATA, REPLACING HYPHENS WITH THE MORE USER-FRIENDLY UNDERSCORES
      subfile_path = paste0(main_path, "\\", lake, "\\", 
                            gsub("-", "_", date))
      
      #WRITE OUT FILE AS A PARQUET
      write_parquet(data_subset_survey, subfile_path)
      
    }
  }
}