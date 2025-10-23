db_path = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet"

db = db_path %>% 
  open_dataset() %>% 
  collect()

compilation = db %>% 
  select(SUBMITTER_NAME, DOW, lake_name, SURVEY_START, SURVEYORS, latitude, longitude)

compilation_bysurvey = compilation %>% 
  group_by(DOW, SURVEY_START) %>% 
  summarize(SUBMITTER_NAME = first(SUBMITTER_NAME), 
            lake_name = first(lake_name),
            SURVEYORS = first(SURVEYORS),
            latitude = first(as.character(latitude)),
            longitude = first(as.character(longitude))
)

compilation_nosmalls = compilation_bysurvey %>% 
  filter(!SUBMITTER_NAME %in% c(
  "DNR Fisheries", "DNR Shallow Lakes", "DNR Lakes and Rivers", "Mary Schaffer (Long Lake)", "John Manske", "Comfort Lake - Forest Lake Watershed District", "Beebe Lake Improvement Association", "DNR Invasive Species Program" , "Beltrami County Environmental Services Department", "Ingrid Bey","Michael Verhoeven", "Sharon Natzel,RMB Environmental Laboratories Inc", "Turtle Lake Association"
))

compilation_spatial = compilation_nosmalls %>% 
  mutate(spatial = ifelse(!is.na(latitude) & !is.na(longitude), "Yes", "No")) %>% 
  select(-latitude, -longitude)

(submitting_strings = sort(unique(compilation_spatial$SUBMITTER_NAME)))

create_digest_files = function(submitters, compilation) {
  
  for(i in 1:length(submitters)) {
    
    comp2write = compilation %>% 
      filter(SUBMITTER_NAME == submitters[i]) %>% 
      arrange(DOW, SURVEY_START)
    
    compString = str_sub(submitters[i], 1, 13)
    
    compYear = str_sub(Sys.Date(),1,4)
    
    write.csv(comp2write, file = paste0("upstream/Admin/compilations/", compString, compYear, LETTERS[i], ".csv"))
    
    print(i)
    
  }
  
}

create_digest_files(submitting_strings, compilation_spatial)

all_emails = rep(NA, length(submitting_strings))
for(i in 1:length(submitting_strings)) {
  
  emails = db %>% 
    filter(SUBMITTER_NAME == submitting_strings[i]) %>% 
    select(SUBMITTER_EMAIL) %>% 
    distinct() %>% 
    pull()
  
  emails = emails[!is.na(emails)]
  
  all_emails[i] = paste0(emails, collapse = ", ")
  
}

View(data.frame(submitting_strings, all_emails))
