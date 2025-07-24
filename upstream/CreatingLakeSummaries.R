#CREATING FROM THE DB (AS CREATED BY DBCLEANUPLEGACYJAN2024.R ORIGINALLY AND BY THE SUBMISSION APPROVAL PROCESS THEREAFTER) THE LAKES_SUMMARIES OBJECTS NEEDED FOR THE OVERVIEW TAB OF THE APP. ALSO, THE OBJS NEEDS FOR THE SELECTORS ON THAT TAB AND THE DFS USED ON THE LEADERBOARD TAB.

#LOAD PACKAGES
library(tidyverse)
library(sf)
library(arrow)

#ESTABLISH CONVENIENCE FUNCTIONS
tidyName <- function(x) {
  base::tolower(
    stringr::str_replace_all(x,
                             pattern = "([\\.\\(\\)\\-\\/\\?])|([_]+)|([\\s]+)",
                             replacement = "_")
  )
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

split.str_dupe_hunts = function(x) { 
  tmp1 = str_split_1(x, ",") 
  tmp2 = duplicated(tmp1)
  tmp3 = tmp1[tmp2]
  tmp4 = toString(tmp3)
  return(tmp4)
}

#DO SMART TYPE INFERENCE ON THE COLUMNS OF A DF
convert_column_types <- function(df) {
  suppressWarnings(lapply(df, function(column) {
    #ATTEMPT TO CONVERT TO LOGICAL IF NO NAS
    logical_column <- as.logical(column)
    if (!anyNA(logical_column)) {
      return(logical_column)
    }
    #ATTEMPT TO CONVERT TO NUMERIC NEXT
    numeric_column <- as.numeric(column)
    if (!anyNA(numeric_column)) {
      return(numeric_column)
    }
    
    #OTHERWISE DEFAULT TO CHARACTER
    return(as.character(column))
  }))
}

#READ IN MOST RECENT CLEAN DB--THE PATH TO THIS SHOULD NEVER CHANGE NOW.
data2summ = read_parquet("H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet")

if(any(data2summ$SURVEYORS == "NA")) { stop("Alex, you left NAs in the DB file for surveyors!") }

#READ IN TAXONOMIC/NOT COLUMN NAMES FILE
newfieldnames = utils::read.csv("inputs/Dynamic/column_name_lookup.csv") %>% 
  dplyr::mutate(fieldname = tidyName(fieldname)) %>%
  dplyr::select(fieldname, newfieldname, taxonomic) %>%
  dplyr::distinct()

#READ IN LIST OF PROTECTED SPECIES
protecteds = read.csv("inputs\\Static\\protected_nameskey.csv")

#GET LIST OF NONTAXONOMIC COLS
nontaxonomic = newfieldnames %>% 
  filter(taxonomic == "N") %>% 
  select(newfieldname) %>% 
  mutate(newfieldname = tidyName(newfieldname)) %>% 
  pull() %>%
  unique()

#FIRST STEP--REMOVE ALL COL NAMES NOT TAXONOMIC
data2summ = data2summ %>% 
  select_if(!names(data2summ) %in% nontaxonomic) %>% 
  select(-subbasin, -multipartsurvey, -SUBMITTER_EMAIL, -RAKE_MAX, -SUBMIT_TIME, -sta_nbr.1, -guid, -plant_height.1, -unk_sp, -unk_sp.1) #^^^IF ANY MORE COLUMN NAMES ENTER THE DB THAT ARE NOT IN THE COL_NAME_LOOKUP FILE LIKE THESE, THEY WOULD NEED TO BE ADDED HERE. SOMETHING TO CONSIDER DURING APPROVALS.

#REMOVE ALL TEXT VALS FROM ALL BUT THE METADATA COLS (^^^SHOULD ALWAYS BE COLS 1-4 AT THIS POINT!) BUT BEING CAREFUL TO DO SO NOT IN A WAY THAT WILL ELIMINATE MIKE'S TRUES AND FALSES
#Eliminated TRUEs and FALSEs, so this should no longer be needed. 
# data2summ[,5:ncol(data2summ)] = data.frame(
# lapply(data2summ[,5:ncol(data2summ)], 
#        function(x) {
#          x[!x %in% c("TRUE", "FALSE")] = suppressWarnings(as.numeric(x[!x %in% c("TRUE", "FALSE")]))
#          x[x %in% c("TRUE", "FALSE")] = suppressWarnings(as.numeric(as.logical(x[x %in% c("TRUE", "FALSE")])))
#          return(x)
#        }
#  )
# )

data2summ[,5:ncol(data2summ)] = data.frame(
  suppressWarnings(lapply(data2summ[,5:ncol(data2summ)], as.numeric))
)#CAN NOW SAFELY CONVERT ALL REMAINING COLS TO NUMERIC

#FIND ALL ROWS FOR SURVEYS SUBMITTED BY DNR. 
data2summDNR = data2summ[grepl("DNR", data2summ$SUBMITTER_NAME),]
data2summDNR$SURVEYORS[is.na(data2summDNR$SURVEYORS)] = "NA"

#COUNT UP HOW MANY SURVEYORS WE ARE ANONYMIZING.
test = lapply(data2summDNR$SURVEYORS, function(x) {
  str_split_1(x, ",")
})
test2 = unlist(lapply(test, function(x) {
  length(x)
}))

#FOR EACH DATA RECORD, PASTE TOGETHER AN ANONYMIZED STRING THAT MANY TIMES.
newSurv = sapply(test2, function(x) {
  paste(c(rep("MN DNR Surveyor", times = x)), collapse = ",")
})

data2summ$SURVEYORS[which(grepl("DNR", data2summ$SUBMITTER_NAME))] = newSurv

#GROUP BY LAKE AND SURVEY, COLLAPSE METADATA, COLLAPSE RAKE SCORES FOR EACH TAXON (LAST STEP)
lakes.summ.new1 = data2summ %>% 
  group_by(DOW, SURVEY_START) %>% 
  summarize(SURVEYORS = first(SURVEYORS),
            SUBMITTER_NAME = first(SUBMITTER_NAME),
            across(!c(SUBMITTER_NAME:SURVEYORS), .fns = function(x) { sum(x, na.rm = T) }) #MUST REMAIN UNNAMED.
  ) 

#UNIFY TAXA FOUND COL FOR EACH SURVEY BY FINDING EVERY TAXONOMIC COL W/ DATA >0 AND UNIFYING THEIR NAMES INTO ONE STRING. ^^^ASSUMES COLS 1-4 ARE METADATA. 
taxacol = apply(lakes.summ.new1[,-(1:4)], 1, function(x) {
  y = names(lakes.summ.new1)[-(1:4)] 
  ind = which(x > 0)  
  taxafound = paste(y[ind], collapse=",") 
})
lakes.summ.new1$taxafound = taxacol

#CUT ALL UNNEEDED COLS
lakes.summ.new2 = lakes.summ.new1 %>% 
  select(DOW, SURVEY_START, SURVEYORS, SUBMITTER_NAME, taxafound)

#***Should no longer be needed now.
# #REMOVE BROAD AND NARROW SUBSTRINGS.
# lakes.summ.new2$taxafound = gsub("_narrow", "", lakes.summ.new2$taxafound, fixed = T)
# lakes.summ.new2$taxafound = gsub("_broad", "", lakes.summ.new2$taxafound, fixed = T)
# 
# #Remove floating and emergent substrings
# lakes.summ.new2$taxafound = gsub("_emergent", "", lakes.summ.new2$taxafound, fixed = T)
# lakes.summ.new2$taxafound = gsub("_floating", "", lakes.summ.new2$taxafound, fixed = T)

#FURTHER COLLAPSE FILE TO PRODUCE THE NEW SUMMARIZED FIELDS BY LAKE FOR THE LAKES_SUMMARY FILES.
lakes.summ.new3 = lakes.summ.new2 %>%
  group_by(DOW) %>%
  summarize(
    DOW = first(DOW),
    nyears = length(unique(str_sub(SURVEY_START, start=1, end=4))), #SURVEY YEAR
    nsurveys = length(unique(SURVEY_START)), 
    surveylist = paste(sort(unique(SURVEY_START)), collapse = ","), #ORDERED SURVEY DATES
    datasource = paste(SUBMITTER_NAME, collapse = ","), #SUBMITTER
    taxalist = paste(taxafound, collapse = ","), #ALL TAXA FOUND AT LEAST ONCE, SORTED, DUPES NOT YET REMOVED.
    ntaxa = length(unique(str_split_1(taxalist, ","))), #COUNT UP UNIQUE TAXA FOUND.
    surveyorlist = paste(SURVEYORS, collapse = ",") #SURVEYORS
  )

#Getting the entire lake polygon object from the state hydrology dataset. Kept for reference
# LAKE_POLY = st_read("../dnr_hydro_features_all.shp") #NO NEED TO RUN...
# LAKE_POLY = LAKE_POLY %>%
#   filter(!is.na(LAKE_POLY$dowlknum)) %>%
#   select(dowlknum, map_label, cty_name, center_utm, center_u_1) %>%
#   st_drop_geometry()
# saveRDS(LAKE_POLY, "upstream/UpstreamInputs/LAKE_POLY")

#LOAD LAKES SPATIAL DATA FILE
LAKE_POLY = readRDS("upstream/UpstreamInputs/LAKE_POLY")

#CONVERT DOW DATA TO CHARACTER, REPAIR THOSE LACKING LEADING 0.
lakes.summ.new3$DOW = as.character(lakes.summ.new3$DOW)
lakes.summ.new3$DOW[nchar(lakes.summ.new3$DOW) == 7] = 
  paste0("0", lakes.summ.new3$DOW[nchar(lakes.summ.new3$DOW) == 7])

#JOIN SPATIAL DATA TO SURVEY SUMMARIES BY DOW TO ADD NEW FIELDS.
LAKE_POLY2 = LAKE_POLY %>% 
  filter(!duplicated(.$dowlknum)) #PREVENT MANY-TO-MANY MATCHES
lakes.summ.new4 = left_join(lakes.summ.new3, LAKE_POLY2, by = c("DOW" = "dowlknum")) %>% 
  rename(LAKE_NAME = map_label)

#REPAIR BIG SOB'S METADATA--AD HOC CASE
lakes.summ.new4$center_utm[lakes.summ.new4$DOW == "10027600"] = 441812.1
lakes.summ.new4$center_u_1[lakes.summ.new4$DOW == "10027600"] = 4968990.1
lakes.summ.new4$cty_name[lakes.summ.new4$DOW == "10027600"] = "Carver"
lakes.summ.new4$LAKE_NAME[lakes.summ.new4$LAKE_NAME == "big sob"] = "Zen Pond/Big Sob"

#LOSE LAKES W/O SPATIAL DATA, PRUNE COLS
lakes.summ.new5 = lakes.summ.new4 %>% 
  dplyr::select(DOW, LAKE_NAME, nyears, nsurveys, surveylist, datasource, ntaxa, taxalist, cty_name, center_utm, center_u_1, surveyorlist) %>% 
  filter(complete.cases(.))

#RECONFIG TAXA DATA TO MAKE SPLITTING EASIER.
lakes.summ.new6 = lakes.summ.new5 %>% 
  rowwise() %>% 
  mutate(
    taxalist = paste0(taxalist, ",")
  )

#WORK OVER THE TAXONOMIC DATA
lakes.summ.new6$taxalist = apply(
  data.frame(lakes.summ.new6$taxalist), 
  1,  #FOR EACH ENTRY IN EACH COLUMN
  function(x) {
    gsub("sp(?=,|$)", "spp.", #SWAP SP SUBSTRINGS
                   paste0( #STRING ALL BACK TOGETHER. 
                     toString( 
                       unique( #REMOVE DUPLICATE TAXA
                         sort( #SORT ALPHABETICALLY
                           gsub(".2", "", #ELIMINATE ALL NUMERIC SUB-DESIGNATORS
                                     gsub(".1", "", 
                                          trimws( #TRIM WHITESPACE
                                          gsub("_", " ", #UNDERSCORES TO SPACES
                                               firstup( #CAP FIRST LETTER
                                                 trimws( 
                                                   unlist( #TURN FROM LIST TO VECTOR
                                                     strsplit(x, #SPLIT STRING ALONG COMMAS
                                                       ",")
                                                   ))), fixed = T)),
                                          fixed=T),
                                     fixed=T)))),
                     ","),
         perl = T)
  })

#ANONYMIZE ANY PROTECTED SPECIES, USING PROTECTED SPECIES LOOKUP FILE
for(r in 1:nrow(protecteds)) {
  lakes.summ.new6$taxalist = gsub(pattern = protecteds$TAXON[r], 
                                   replacement = protecteds$new_name[r],
                                   x = lakes.summ.new6$taxalist)
}

#SAFETY CHECK--LOOK FOR DUPE RECORDS ***WOULD THIS ACTUALLY CATCH DUPES, IF ALL THE RECORDS FROM TWO SURVEYS WITH THE SAME METADATA HAD ALREADY BEEN SMOOSHED TOGETHER BY THIS POINT?
duped_records = unlist(apply(data.frame(lakes.summ.new6$surveylist), 1, split.str_dupe_hunts))
dupe.surveys = as.vector(duped_records[(duped_records != "")])
dupe.dows = as.vector(lakes.summ.new6$DOW[(duped_records != "")])
data.frame(dupe.dows, dupe.surveys)

#SOME SURVEYS WILL NOW HAVE SPACE/COMMA/BOTH IN FRONT OF FIRST TAXON NAME, LIKELY DUE TO REMOVAL OF DUPES. REMOVE THESE.
lakes.summ.new6$taxalist[grepl(",", str_sub(lakes.summ.new6$taxalist,1,2))] = 
  str_sub(lakes.summ.new6$taxalist[grepl(",", str_sub(lakes.summ.new6$taxalist,1,2))], 3)


###CREATING SELECTOR CHOICE OBJECTS FOR THE OVERVIEW TAB


#FIND ALL UNIQUE TAXA OBSERVED TO POPULATE TAXON SELECTOR.
selector_taxa = unique(trimws(str_split_1(paste(lakes.summ.new6$taxalist, collapse = ","), ",")))

selector_taxa = selector_taxa[!grepl("Protected Species", selector_taxa)] #REMOVE PROTECTED SPECIES.
selector_taxa = selector_taxa[-which(selector_taxa == "")] #REMOVE ANY BLANKS
db_allSearchableTaxa = sort(selector_taxa) #ARRANGE ALPHABETICALLY
db_allSearchableTaxa = 
  db_allSearchableTaxa[!grepl(" taxa", db_allSearchableTaxa, ignore.case = T)] #REMOVE FAMILIES USING TAXA SUBSTRING

#REPLACE SPP SUBSTRING WITH ANY WHICH WILL ALLOW GENUS NAMES TO PULL BACK RECORDS WITH ANY OBSERVED SPECIES WITHIN THOSE GENERA.
db_allSearchableTaxa = gsub(" spp.", " (any)", db_allSearchableTaxa, fixed = T) 

#FIX SOME AD HOC COUNTY NAME ISSUES. 
lakes.summ.new6$cty_name[lakes.summ.new6$DOW=="32002400"] = "Jackson" #WAS "IOWA"
lakes.summ.new6$cty_name[lakes.summ.new6$DOW=="41011000"] = "Lincoln" #WAS "SD"

#CREATE VEC OF ALL COUNTIES FOR COUNTY SELECTOR.
db_allCounties = sort(unique(lakes.summ.new6$cty_name)) 

#CREATE VEC OF ALL LAKES W/ RECORDS, WITH NAMES SUTURED ON IN ()S TO MAKE SEARCHING EASIER.
d3 = paste0(lakes.summ.new6$DOW, " (", tools::toTitleCase(lakes.summ.new6$LAKE_NAME), ")")
db_allDOWs = sort(unique(d3))

#CREATE SECOND SUMMARY OBJ W/ SPATIAL DATA FOR PLOTTING.
lakes.summary.sf = lakes.summ.new6 %>% 
  st_as_sf(coords = c("center_utm",
                      "center_u_1"), 
           crs = 26915) %>% #UTM Zone 15N projection.
  st_sf() %>% 
  st_transform(crs=4326) #BUT FOR MAPPING, LAT/LONG IS BETTER, SO TRANSFORM.

lakes.summary.definitive = lakes.summ.new6 #SAVE NON-SPATIAL VERSION ALSO.


###LEADERBOARDS OBJECTS

# #FIND ALL ROWS FOR SURVEYS SUBMITTED BY DNR. 
# lakes.summ.definitiveDNR = lakes.summary.definitive[grepl("DNR", lakes.summary.definitive$datasource),]
# #COUNT UP HOW MANY SURVEYORS WE ARE ANONYMIZING.
# test = lapply(lakes.summ.definitiveDNR$surveyorlist, function(x) {
#   str_split(x, ",", simplify = F)
# })
# test2 = unlist(lapply(test, length))
# 
# #FOR EACH DATA RECORD, PASTE TOGETHER AN ANONYMIZED STRING THAT MANY TIMES.
# for(i in 1:nrow(lakes.summ.definitiveDNR)) {
#   lakes.summ.definitiveDNR$surveyorlist[i] = paste(c(rep("MN DNR Surveyor", times = test2[i])), collapse = ",")
# }
# 
# #SPLIT-COMBINE
# lakes.summary.definitiveNonDNR = lakes.summary.definitive[!grepl("DNR", lakes.summary.definitive$datasource),]
# lakes.summary.definitive = bind_rows(lakes.summ.definitiveDNR, lakes.summary.definitiveNonDNR)

#CREATE DF OF SURVEYORS, DELIMITED BY ,S
people.list = paste(lakes.summary.definitive$surveyorlist, collapse = ",")
people.list2 = trimws(unlist(str_split(people.list, ",", simplify = FALSE))) #THEN SPLIT BY COMMAS.

#COUNT UP ALL BY # APPEARANCES.
surveyors.leaderboard.df = data.frame(table(people.list2)) %>% 
  arrange(desc(Freq))

names(surveyors.leaderboard.df) = c("Surveyor", "Number of surveys") #NICE COL NAMES

surveyors.leaderboard.df = surveyors.leaderboard.df %>% 
  filter(Surveyor != "") #ELIMINATE BLANK ENTRY

#CREATE DF FOR SUBMITTER LEADERBOARD 
submitter_leaderboard = {
  tmp1 = toString(paste0(lakes.summary.definitive$datasource, collapse=",")) #UNIFY NAMES IN STRING, DELIMITED BY ,S
  tmp2 = trimws(str_split_1(tmp1, ",")) #SPLIT ALONG ,S INTO VEC AND GET RID OF ANY WHITE SPACES THAT RESULT.
  tmp3 = table(tmp2) #TABULATE # OF UNIQUE APPEARENCES.
  tmp4 = data.frame(tmp3) #TURN TO DF
  tmp5 = tmp4 %>%  #SORT AND RENAME COLS
    arrange(desc(Freq)) %>% 
    rename(Submitter = tmp2, `Number of submissions` = Freq)
  tmp5 = tmp5 %>% #HANDLE SOME AD HOC QUIRKS (LIKE STRINGS SPLIT BY INTENDED COMMAS)
    filter(Submitter != " LLC")
  tmp5$Submitter = as.character(tmp5$Submitter)
  tmp5$Submitter[tmp5$Submitter == "Endangered Resource Services"] = "Endangered Resource Services LLC"
  tmp5
}


### WRITE OUT FINAL FILES


{saveRDS(surveyors.leaderboard.df, "inputs/MadeUpstream/surveyors_leaderboard_df")
  saveRDS(lakes.summary.definitive, "inputs/MadeUpstream/lakes_summary_definitive")
  saveRDS(db_allDOWs, "inputs/MadeUpstream/db_allDOWs")
  saveRDS(db_allCounties, "inputs/MadeUpstream/db_allCounties")
  saveRDS(db_allSearchableTaxa, "inputs/MadeUpstream/db_allSearchableTaxa")
  saveRDS(lakes.summary.sf, "inputs/MadeUpstream/lakes_summary_sf")
  saveRDS(submitter_leaderboard, "inputs/MadeUpstream/submitter_leaderboard")
}

###COPY OVER THE DB FILE TO WHERE IT WILL SHIP WITH THE APP.

file.copy("upstream/db_unified.parquet", "inputs/MadeUpstream/db_unified.parquet", overwrite = TRUE)
