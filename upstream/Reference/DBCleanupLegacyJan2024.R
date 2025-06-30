#GOAL: COMBINE MIKE'S DB WITH PI CHARTERS NEW DB INTO A UNIFIED AND CLEANED STRUCTURE FOR USING GOING FORWARD.

#LOAD LIBRARIES
library(arrow)
library(tidyverse)

#ESTABLISH CONVENIENCE FUNCTIONS
replace_names <- function(strings, toreplace, replacements) {
  replacement.vec <- setNames(replacements, toreplace)
  stringr::str_replace_all(strings, replacement.vec)
}

tidyName <- function(x) {
  base::tolower(
    stringr::str_replace_all(x,
                             pattern = "([\\.\\(\\)\\-\\/\\?])|([_]+)|([\\s]+)",
                             replacement = "_")
  )
}

#LOAD MIKE'S DB
pi_data_mike = read.csv("H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Big Data\\picharterlegacydb.csv")

#LOAD PI CHARTER'S DB AS OF JAN 2024. ***PATH MAY NEED UPDATING, AS I HAVE ARCHIVED THIS FILE.
pi_data_new = read_parquet("H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_new.parquet")

#LOAD OTHER REFERENCE FILES, AS TRADITIONALLY PROCESSED.
dows_lakenames_counties = utils::read.csv("inputs/Static/dows_lakenames_counties.csv", 
                                          colClasses = "character")
newfieldnames = utils::read.csv("inputs/Dynamic/column_name_lookup.csv") %>% 
  dplyr::mutate(fieldname = tidyName(fieldname)) %>%
  dplyr::select(fieldname, newfieldname, taxonomic) %>%
  dplyr::distinct()

#FIX DISPARATE DATA TYPES BTW TWO DBS--THIS IS REPETITIVE BUT "CAREFUL."
pi_data_mike$depth = as.character(pi_data_mike$depth)
pi_data_mike$no_veg_found = as.character(pi_data_mike$no_veg_found)
pi_data_mike$ceratophyllum_demersum = as.character(pi_data_mike$ceratophyllum_demersum)
pi_data_mike$chara_sp = as.character(pi_data_mike$chara_sp)
pi_data_mike$eleocharis_acicularis = as.character(pi_data_mike$eleocharis_acicularis)
pi_data_mike$elodea_canadensis = as.character(pi_data_mike$elodea_canadensis)
pi_data_mike$heteranthera_dubia = as.character(pi_data_mike$heteranthera_dubia)
pi_data_mike$iris_versicolor = as.character(pi_data_mike$iris_versicolor)
pi_data_mike$lemna_sp = as.character(pi_data_mike$lemna_sp)
pi_data_mike$lemna_minor = as.character(pi_data_mike$lemna_minor)
pi_data_mike$lemna_trisulca = as.character(pi_data_mike$lemna_trisulca)
pi_data_mike$myriophyllum_sibiricum = as.character(pi_data_mike$myriophyllum_sibiricum)
pi_data_mike$myriophyllum_spicatum = as.character(pi_data_mike$myriophyllum_spicatum)
pi_data_mike$najas_flexilis = as.character(pi_data_mike$najas_flexilis)
pi_data_mike$najas_guadalupensis = as.character(pi_data_mike$najas_guadalupensis)
pi_data_mike$nuphar_variegata = as.character(pi_data_mike$nuphar_variegata)
pi_data_mike$nymphaea_odorata = as.character(pi_data_mike$nymphaea_odorata)
pi_data_mike$polygonum_amphibium = as.character(pi_data_mike$polygonum_amphibium)
pi_data_mike$potamogeton_amplifolius = as.character(pi_data_mike$potamogeton_amplifolius)
pi_data_mike$potamogeton_crispus = as.character(pi_data_mike$potamogeton_crispus)
pi_data_mike$potamogeton_foliosus = as.character(pi_data_mike$potamogeton_foliosus)
pi_data_mike$potamogeton_friesii = as.character(pi_data_mike$potamogeton_friesii)
pi_data_mike$potamogeton_gramineus = as.character(pi_data_mike$potamogeton_gramineus)
pi_data_mike$potamogeton_illinoensis = as.character(pi_data_mike$potamogeton_illinoensis)
pi_data_mike$potamogeton_nodosus = as.character(pi_data_mike$potamogeton_nodosus)
pi_data_mike$potamogeton_praelongus = as.character(pi_data_mike$potamogeton_praelongus)
pi_data_mike$potamogeton_richardsonii = as.character(pi_data_mike$potamogeton_richardsonii)
pi_data_mike$potamogeton_zosteriformis = as.character(pi_data_mike$potamogeton_zosteriformis)
pi_data_mike$riccia_sp = as.character(pi_data_mike$riccia_sp)
pi_data_mike$sagittaria_sp = as.character(pi_data_mike$sagittaria_sp)
pi_data_mike$scirpus_sp = as.character(pi_data_mike$scirpus_sp)
pi_data_mike$spirodela_polyrhiza = as.character(pi_data_mike$spirodela_polyrhiza)
pi_data_mike$stuckenia_pectinata = as.character(pi_data_mike$stuckenia_pectinata)
pi_data_mike$typha_sp = as.character(pi_data_mike$typha_sp)
pi_data_mike$typha_angustifolia = as.character(pi_data_mike$typha_angustifolia)
pi_data_mike$utricularia_vulgaris = as.character(pi_data_mike$utricularia_vulgaris)
pi_data_mike$vallisneria_americana = as.character(pi_data_mike$vallisneria_americana)
pi_data_mike$wolffia_sp = as.character(pi_data_mike$wolffia_sp)
pi_data_mike$zannichellia_palustris = as.character(pi_data_mike$zannichellia_palustris)

#JOIN DBS TOGETHER
pi_data_joined = bind_rows(pi_data_mike, pi_data_new)

#CLEANUP TO SAVE MEMORY
remove(pi_data_mike, pi_data_new)
gc()

pi_data_joined = dplyr::select(pi_data_joined, -X) #ELIMINATE ROW #S


##CLEAN UP SECTION


#DOWS
#FIX ANY 7 DIGIT DOWS
pi_data_joined$DOW = as.character(pi_data_joined$DOW)
pi_data_joined$DOW[nchar(pi_data_joined$DOW) == 7] = paste0("0", pi_data_joined$DOW[nchar(pi_data_joined$DOW) == 7])

#LAKE NAMES
#FIND LAKE NAMES NOT MATCHING WHAT IS LISTED ELSEWHERE. MIKE HAS CONFIRMED THEY ARE OK.
sort(unique(pi_data_joined$DOW))[which(!sort(unique(pi_data_joined$DOW)) %in% dows_lakenames_counties$DOW)] 
#MODIFYING SOME LAKE NAMES
pi_data_joined$lake_name = gsub("rrwma -", "roseau river WMA", pi_data_joined$lake_name)
pi_data_joined$lake_name = gsub("- east", "east", pi_data_joined$lake_name)
pi_data_joined$lake_name = gsub("- west", "west", pi_data_joined$lake_name)

#SUPERIMPOSE A LAKE NAME WHERE NONE CURRENTLY IS LISTED USING REFERENCE FILES AND REPLACE_NAMES. 
pi_data_joined$lake_name[is.na(pi_data_joined$lake_name)] = 
  pi_data_joined$DOW[is.na(pi_data_joined$lake_name)]
pi_data_joined$lake_name[is.na(pi_data_joined$lake_name)] = 
  replace_names(pi_data_joined$lake_name[is.na(pi_data_joined$lake_name)],
                dows_lakenames_counties$DOW, tolower(dows_lakenames_counties$NAME))

#SURVEY_START
#CONVERT ALL DATES TO A CONSISTENT FORMAT
 pi_data_joined$SURVEY_START[grepl("/", pi_data_joined$SURVEY_START)] = 
   as.character(as.Date(pi_data_joined$SURVEY_START[grepl("/", pi_data_joined$SURVEY_START)], 
                        format = "%m/%d/%Y"))
length(which(nchar(pi_data_joined$SURVEY_START) != 10))

#DEPTH
#WHICH VALS NON-NUMERIC?
pi_data_joined$depth[which(is.na(as.numeric(pi_data_joined$depth)))][
  !is.na(pi_data_joined$depth[which(is.na(as.numeric(pi_data_joined$depth)))])
  ]

pi_data_joined$depth[pi_data_joined$depth == "Depth ft"] = NA #FIX THIS ONE
#OTHERS ALL INCLUDE + SYMBOLS--^^^RETAIN BUT KEEP IN MIND FOR RECORDS TAB.

#DEPTH UNITS
pi_data_joined$DEPTH_UNITS[is.na(pi_data_joined$DEPTH_UNITS)] = "Feet" #INSERT FEET FOR ALL 

#RAKE MAX
pi_data_joined$RAKE_MAX[is.na(pi_data_joined$RAKE_MAX)] = 3 #MIKE HAS SAID THAT ALL HIS RAKE SCORE DATA ARE RELATIVIZED TO A MAX RAKE SCORE OF 3.^^^BUT HE COULD SEND US AN UNRELATIVIZED VERSION WHEREVER APPLICABLE.

#ELIMINATE ERRANT COLUMN THAT IS JUST A DUPLICATE. MOVE DATA OVER TO MAIN COLUMN, THEN REMOVE.
pi_data_joined$depth.1[!is.na(pi_data_joined$`"depth.1`)] =
  pi_data_joined$`"depth.1`[!is.na(pi_data_joined$`"depth.1`)]
pi_data_joined = pi_data_joined %>% select(-`\"depth.1\"`) 

#REMOVE AD HOC COLS THAT SHOULDN'T HAVE BEEN INITIALLY ACCEPTED THRU.
pi_data_joined = pi_data_joined %>% select(-please_provide_comment,
                                           -dreissena_polymorpha,
                                           -sample_type)

#RENAME SOME COLUMNS IN AN ADHOC MANNER.
pi_data_joined = pi_data_joined %>% 
  rename(lemnaceae_taxa = lemnaceae)

#REMOVE SOME ERRANT DUPLICATE COLUMNS.
pi_data_joined = pi_data_joined %>% 
  select(-cyperaceae_sp, -characeae, -certatyphylum_echinatum)

#TAXONOMIC COLS
#ALL NON-TAX COLS--RECOGNIZE?
non_taxa = which(!names(pi_data_joined) %in% tidyName(newfieldnames$newfieldname)[newfieldnames$taxonomic == "Y"])
names(pi_data_joined)[non_taxa]

#AD HOC LAKE NAME CHANGE.
pi_data_joined$lake_name[pi_data_joined$DOW == 34015800] = "monongalia"

#SURVEYORS CLEANUP
#REPLACE ODD SUBSTRINGS WITH OUR ANONYMOUS STRING FOR SURVEYORS.
pi_data_joined$SURVEYORS = gsub("Unk", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS)
pi_data_joined$SURVEYORS = gsub("NA", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS)
pi_data_joined$SURVEYORS[pi_data_joined$SURVEYORS == "" | is.na(pi_data_joined$SURVEYORS) | pi_data_joined$SURVEYORS == "NA"] = "Unnamed hardworking surveyor(s)"
pi_data_joined$SURVEYORS = gsub(",,", ",", pi_data_joined$SURVEYORS)
pi_data_joined$SURVEYORS = gsub(",$", "", pi_data_joined$SURVEYORS) #ELIM TERMINAL ,S
pi_data_joined$SURVEYORS = gsub(",(?![ ])", ", ", pi_data_joined$SURVEYORS, perl = TRUE) #REPLACE , WITHOUT SPACE WITH , WITH SPACE

#CLEAN UP OF ADDITIONAL SURVEYORS NAMES WHEN ORIGINAL WAS AN ORG.
{pi_data_joined$SURVEYORS = gsub("DNR Fisheries", "MN DNR", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County - Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County 􀍴 Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County – Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County - Parks & Recreation, Soil and Water Conservation District", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County Soil & Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County Parks and Recreation - Soil & Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County – Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Ramsey County Parks and Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("DNR Staff", "MN DNR", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("?,", "Unnamed hardworking surveyor(s),", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("MN DNR,", "Unnamed hardworking surveyor(s),", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("Ramsey Co conservation district", "Unnamed hardworking surveyor(s),", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("Endangered Resource Services,", "Unnamed hardworking surveyor(s),", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("RWMWD Staff,", "Unnamed hardworking surveyor(s),", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("Ramsey Conservation District,", "Unnamed hardworking surveyor(s),", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("Three Rivers Park District,", "Unnamed hardworking surveyor(s),", pi_data_joined$SURVEYORS, fixed = T)}

#CLEANUP OF RANDOM SPACES/PUNCTUATION IN THE SURVEYORS COL
{pi_data_joined$SURVEYORS = gsub("  ", " ", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(" ,", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(";", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",,,", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",,", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(", ", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",  ", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(" and ", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(".", "", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(" with ", ",", pi_data_joined$SURVEYORS, fixed=T)}

#FIX INCONSISTENT SURVEYORS NAMES. XXX MARKS BEST GUESSES
{pi_data_joined$SURVEYORS = gsub("A Doll", "Adam Doll", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("B Hummel", "Brittany Hummel", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("A Londo", "April Londo", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Eric F,", "Eric Fieldseth,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Angie Les", "Angela Les", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Hallie M Jensen", "Hallie Jensen", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Josh Curtin", "Joshua Curtin", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("J Bjorklund", "Jill Sweet", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("M Kocian", "Matt Kocian", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Jill B,", ",Jill Sweet,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Brunell,", "Brunelle,", pi_data_joined$SURVEYORS, fixed=T)#XXX
  pi_data_joined$SURVEYORS = gsub("James A Johnson", "James Johnson", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("JA Johnson", "James Johnson", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Emmy Hauck", "Emelia Hauck Jacobs", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Jonathan D JaKa", "John Jaka", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Josh Knopik", "Joshua Knopik", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Joshua M Knopik", "Joshua Knopik", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Katie W,", "Katie Wigen,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("K Lund", "Keegan Lund", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Kelly D,", "Kelly Dooley,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Kris Carlson", "Kristin Carlson", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("K Bloodsworth", "Kylie Bloodsworth", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("K Cattoor", "Kylie Cattoor", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Kailey K,", "Kailey Kreatz,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("M Verhoeven", "Michael Verhoeven", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Mike Verhoeven", "Michael Verhoeven", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Nik Myrha,", "Nik Myrha-Edwards,", pi_data_joined$SURVEYORS, fixed=T)#XXX
  pi_data_joined$SURVEYORS = gsub("Yvette C,", "Yvette Christianson,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Nick Whichello", "Nick Wichello", pi_data_joined$SURVEYORS, fixed=T)#XXX
  pi_data_joined$SURVEYORS = gsub("Todd P,", "Todd Piepho,", pi_data_joined$SURVEYORS, fixed=T)#XXX
  pi_data_joined$SURVEYORS = gsub("Marty Evans", "Martin Evans", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("R Contreras,", "Rafa Contreras,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("R Roche", "Rochelle Roche", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Rafa Contrera,", "Rafa Contreras,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Allly", "Ally", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Carstenen", "Carstensen", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Jill,", "Jill Sweet,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Crowell,", ",Wendy Crowell,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Kailey,", ",Kailey Kreatz,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("LW,", "Lucas Wandrie,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Drake", ",Christa Drake", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Einck", ",Alan Einck", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Hennen", ",Matt Hennen", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",JN,", ",Joe Norman,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Mottl", ",E Mottl", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Proulx", ",Nick Proulx", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Sean,", ",Sean Sissler,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Travis", ",Mitch Travis", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Uhler", ",Kyle Uhler", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Yvetter,", "Yvette Christianson,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Eric,", ",Eric Fieldseth,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Adam,", ",Adam Doll,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Eisterhold", ",Joe Eisterhold", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(",Haworth,", ",Matt Horwath,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Matt,", "Matt Hennen,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Mike Veerhoven", "Michael Verhoeven", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("MVerhoeven", "Michael Verhoeven", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Scott M,", "Scott Mackenthun,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("AR,", "Adam Rollins,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("CP,", "Christine Powell,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("M Bockman,", "M Bokman,", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub("Jill Bjorklund,", "Jill Sweet,", pi_data_joined$SURVEYORS, fixed=T)}

#ELIMINATE ODDS AND ENDS SUBSTRINGS IN SURVEYORS FIELD
{pi_data_joined$SURVEYORS = gsub("LLC", "", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub(" (first part of day)", "", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub(" (UMN)", "", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("(7/16 only)", "", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("/", ",", pi_data_joined$SURVEYORS, fixed=T)
  pi_data_joined$SURVEYORS = gsub(" (Lincoln County)", "", pi_data_joined$SURVEYORS, fixed = T)
  pi_data_joined$SURVEYORS = gsub("(Scott Co)", "", pi_data_joined$SURVEYORS, fixed = T)}

#REPLACE UNREPAIRABLE SINGLE NAME STRINGS IN SURVEYOR COLUMN WITH THE ANONYMOUS STRING.
{pi_data_joined$SURVEYORS = gsub("(?<=,|^)Marcie(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Wenck(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Alex(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Andy(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Tom(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Chase(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Mulu(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Jake(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Bri(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Chris(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Marissa(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Weston(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Johanna(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)DA(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)RP(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Crowell(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Adam(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)A Edgcumbe(?=,|$)", "Andrew Edgcumbe", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)AR(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Blue Water Science(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)CP(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Eisterhold(?=,|$)", "Joe Eisterhold", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Eric(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Hennen(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Jill(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)JN(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Kailey(?=,|$)", "Kailey Kreatz", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Kailey K(?=,|$)", "Kailey Kreatz", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Katie W(?=,|$)", "Katie Wigen", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Kelly D(?=,|$)", "Kelly Dooley", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Kylie Bloodsworth (?=,|$)", "Kylie Bloodsworth", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Liz(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)LW(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)M Bockman(?=,|$)", "Melissa Bokman", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)M Bokman(?=,|$)", "Melissa Bokman", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Matt Kocian (?=,|$)", "Matt Kocian", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)MN DNR(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Nate(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Ramsey Conservation District(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Ramsey Co conservation district(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)RWMWD Staff(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Ray Newman(?=,|$)", "Raymond Newman", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Scott M(?=,|$)", "Scott Mackenthun", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Sean(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Three Rivers Park District(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Yvetter(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Yvette C(?=,|$)", "Yvette Christianson", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)E(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Jason Brunell(?=,|$)", "Jason Brunelle", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Jill B(?=,|$)", "Jill Bjorklund", pi_data_joined$SURVEYORS, perl = T)
pi_data_joined$SURVEYORS = gsub("(?<=,|^)Nik Myrha(?=,|$)", "Nik Myrha-Edwards", pi_data_joined$SURVEYORS, perl = T)
}

#SUBMITTERS
#FIX DISCREPANCIES IN ONE SUBMITTER'S NAME
pi_data_joined$SUBMITTER_NAME[grepl("Three Rivers Parks District", pi_data_joined$SUBMITTER_NAME)] = 
                                           "Three Rivers Park District"

#Write out the file as CSV
# write.csv(pi_data_joined, "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_joined_cleaned.csv", row.names = FALSE)
#WRITE OUT AS PARQUET FILE
write_parquet(pi_data_joined, "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet")
