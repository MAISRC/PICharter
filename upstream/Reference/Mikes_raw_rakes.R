# #LOAD PACKAGES (IT MAY ALSO BE NECESSARY TO LOAD SOME FILES FROM GLOBAL.R)
library(tidyverse)

#LOAD IN MIKE'S MORE RECENT FILE WITH THE RAW RAKE DATA.
raw_rakes = read.csv("H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Big Data\\plants_pointaggd_mixtypes.csv") 

#CLIP OUT UNNEEDED COLUMNS
raw_rakes2 = raw_rakes %>% 
  select(-Secchi_m, -SECCHI_DATE, -SURVEY_ID, -order_ID, 
         -SECCHI_m_ACCEPTED, -proplight, -richness, -nat_richness,
         -shannon_div, -simpsons_div, -shannon_div_nat, -simpsons_div_nat)

#LOAD IN COMMON/SCI NAME LOOKUP TABLE
commonsciNameLookup = read.csv("inputs/Static/commonsci_name_lookup.csv")

#CHECK FOR DATA ANOMOLIES
which(is.na(raw_rakes2$DOW)) #850 OBSERVATIONS HAVE NA FOR DOW.
#SAVE BIG SOB
raw_rakes2$DOW[raw_rakes2$LAKE_NAME == "big sob"] = "10027600"
#EXCLUDE THE REST. MAYBE WE CAN SAVE THEM SOME DAY, BUT THEY ARE DOW-LESS WATERBODIES... 
pi_data_mike = raw_rakes2 %>% 
  filter(!is.na(DOW))

#FIX ANY 7 DIGIT DOWS
pi_data_mike$DOW = as.character(pi_data_mike$DOW)
pi_data_mike$DOW[nchar(pi_data_mike$DOW) == 7] = paste0("0", 
                                                        pi_data_mike$DOW[nchar(pi_data_mike$DOW) == 7])

#RENAME COLUMNS TO BE IN LINE WITH WHAT THE APP NOW EXPECTS
names(pi_data_mike) = firstup(names(pi_data_mike))

pi_data_mike = pi_data_mike %>% 
  rename(acorus_sp = Acorus,
         alisma_sp = Alisma,
         alnus_sp = Alnus,
         asteraceae_taxa = Asteraceae,
         bidens_sp = Bidens,
         callitriche_sp = Callitriche,
         carex_sp = Carex,
         ceratophyllum_sp = Ceratophyllum,
         chara_sp = Chara,
         characeae_taxa = Characeae,
         cicuta_sp = Cicuta,
         cyperaceae_taxa = Cyperaceae,
         drepanocladus_sp = Drepanocladus,
         elatine_sp = Elatine,
         eleocharis_sp = Eleocharis,
         elodea_sp = Elodea,
         equisetum_sp = Equisetum,
         eragrostis_sp = Eragrostis,
         eutrochium_sp = Eutrochium,
         hypericum_sp = Hypericum,
         impatiens_sp = Impatiens,
         iris_sp = Iris,
         isoetes_sp = Isoetes,
         juncus_sp = Juncus,
         lamiaceae_taxa = Lamiaceae,
         lemna_sp = Lemna,
         lysimachia_sp = Lysimachia,
         myriophyllum_sp = Myriophyllum,
         najas_sp = Najas,
         nitella_sp = Nitella,
      #   nitellopsis_sp = Nitellopsis,
         nuphar_sp = Nuphar,
         nymphaea_sp = Nymphaea,
         nymphaeaceae_taxa = Nymphaeaceae,
         persicaria_sp = Persicaria,
         poaceae_taxa = Poaceae,
         potamogeton_sp = Potamogeton,
         potamogeton_sp_broad = Potamogeton..broad.,
         potamogeton_sp_narrow = Potamogeton..narrow.,
         ranunculus_sp = Ranunculus,
         riccia_sp = Riccia,
         sagittaria_sp = Sagittaria,
         salix_sp = Salix,
         schoenoplectus_sp = Schoenoplectus,
         scirpus_sp = Scirpus,
         scutellaria_sp = Scutellaria,
         solidago_sp = Solidago,
         sparganium_sp = Sparganium,
         sparganium_sp_emergent = Sparganium..emergent.,
         sparganium_sp_floating = Sparganium..floating.,
         sphagnum_sp = Sphagnum,
         stuckenia_sp = Stuckenia,
         typha_sp = Typha,
         utricularia_sp = Utricularia,
         verbena_sp = Verbena,
         wolffia_sp = Wolffia,
         zizania_sp = Zizania,
         zosterella_sp = Zosterella)

names(pi_data_mike) = tidyName(names(pi_data_mike)) #TIDY ALL NAMES

#REVIEW NEW AND OLD COLUMN NAMES TO CHECK FOR DISCREPANCIES (THIS PATH LIKELY NEEDS UPDATING, AS I HAVE ARCHIVED THIS FILE)
#db_path = "H:/Shared drives/MAISRC/Quantification, Data, and Computation/Projects/Statewide Plant Surveys App/PI Charter/upstream/db_unified.parquet"

newDB = read_parquet(db_path) #READ NEW DB FILE

#FIND MISMATCHING NAMES
names(pi_data_mike)[!names(pi_data_mike) %in% names(newDB)] #SHOULD RETURN NOTHING AFTER NEXT COMMAND.

#RENAME ADDITIONAL COLUMNS
pi_data_mike = pi_data_mike %>% 
  rename(DOW = dow,
         SURVEY_START = survey_date,
         SUBMITTER_NAME = survey_datasource,
         SURVEYORS = surveyor,
         RAKE_MAX = rake_scale_used,
         sta_nbr = point_id,
         depth = depth_ft, 
         rumex_britannica = rumex_orbiculatus
  )

#REMOVE COLUMNS WITH NO ACTUAL DATA
pi_data_mike2 = pi_data_mike %>% 
  dplyr::select_if(~any(!is.na(.) & 
                          . != 0 &
                          . != FALSE &
                          . != "FALSE"))

names(pi_data_mike)[!names(pi_data_mike) %in% names(pi_data_mike2)]

#FIND LAKE NAMES NOT MATCHING WHAT IS LISTED ELSEWHERE. MIKE HAS CONFIRMED THEY ARE OK.
sort(unique(pi_data_mike2$DOW))[which(!sort(unique(pi_data_mike2$DOW)) %in% dows_lakenames_counties$DOW)] 

pi_data_mike2$lake_name = gsub("rrwma -", "roseau river WMA", pi_data_mike2$lake_name)
pi_data_mike2$lake_name = gsub("- east", "east", pi_data_mike2$lake_name)
pi_data_mike2$lake_name = gsub("- west", "west", pi_data_mike2$lake_name)

#SUPERIMPOSE A LAKE NAME WHERE NONE CURRENTLY IS LISTED USING REFERENCE FILES AND REPLACE_NAMES. 
pi_data_mike2$lake_name[is.na(pi_data_mike2$lake_name)] = 
  pi_data_mike2$DOW[is.na(pi_data_mike2$lake_name)]
pi_data_mike2$lake_name[is.na(pi_data_mike2$lake_name)] = 
  replace_names(pi_data_mike2$lake_name[is.na(pi_data_mike2$lake_name)],
                dows_lakenames_counties$DOW, tolower(dows_lakenames_counties$NAME))

#SURVEY_START
#CONVERT ALL DATES TO A CONSISTENT FORMAT
pi_data_mike2$SURVEY_START[grepl("/", pi_data_mike2$SURVEY_START)] = 
  as.character(as.Date(pi_data_mike2$SURVEY_START[grepl("/", pi_data_mike2$SURVEY_START)], 
                       format = "%m/%d/%Y"))

#DEPTH
#WHICH VALS NON-NUMERIC?
pi_data_mike2$depth[which(is.na(as.numeric(pi_data_mike2$depth)))][
  !is.na(pi_data_mike2$depth[which(is.na(as.numeric(pi_data_mike2$depth)))])
]

#DEPTH UNITS
pi_data_mike2$DEPTH_UNITS = "Feet" #INSERT FEET FOR ALL 

#TAXONOMIC COLS
#ALL NON-TAX COLS--RECOGNIZE?
non_taxa = which(!names(pi_data_mike2) %in% tidyName(newfieldnames$newfieldname)[newfieldnames$taxonomic == "Y"])
names(pi_data_mike2)[non_taxa]

#AD HOC LAKE NAME CHANGE.
pi_data_mike2$lake_name[pi_data_mike2$DOW == 34015800] = "monongalia"

#SURVEYORS CLEANUP
#REPLACE ODD SUBSTRINGS WITH OUR ANONYMOUS STRING FOR SURVEYORS.
pi_data_mike2$SURVEYORS = gsub("Unk", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS)
pi_data_mike2$SURVEYORS = gsub("NA", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS)
pi_data_mike2$SURVEYORS[pi_data_mike2$SURVEYORS == "" | is.na(pi_data_mike2$SURVEYORS) | pi_data_mike2$SURVEYORS == "NA"] = "Unnamed hardworking surveyor(s)"
pi_data_mike2$SURVEYORS = gsub(",,", ",", pi_data_mike2$SURVEYORS)
pi_data_mike2$SURVEYORS = gsub(",$", "", pi_data_mike2$SURVEYORS) #ELIM TERMINAL ,S
pi_data_mike2$SURVEYORS = gsub(",(?![ ])", ", ", pi_data_mike2$SURVEYORS, perl = TRUE) #REPLACE , WITHOUT SPACE WITH , WITH SPACE

#CLEAN UP OF ADDITIONAL SURVEYORS NAMES WHEN ORIGINAL WAS AN ORG.
{pi_data_mike2$SURVEYORS = gsub("DNR Fisheries", "MN DNR", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County - Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County 􀍴 Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County – Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County - Parks & Recreation, Soil and Water Conservation District", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County Soil & Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County Parks and Recreation - Soil & Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County – Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey County Parks and Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("DNR Staff", "MN DNR", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("?,", "Unnamed hardworking surveyor(s),", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("MN DNR,", "Unnamed hardworking surveyor(s),", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey Co conservation district", "Unnamed hardworking surveyor(s),", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("Endangered Resource Services,", "Unnamed hardworking surveyor(s),", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("RWMWD Staff,", "Unnamed hardworking surveyor(s),", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("Ramsey Conservation District,", "Unnamed hardworking surveyor(s),", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("Three Rivers Park District,", "Unnamed hardworking surveyor(s),", pi_data_mike2$SURVEYORS, fixed = T)}

#CLEANUP OF RANDOM SPACES/PUNCTUATION IN THE SURVEYORS COL
{pi_data_mike2$SURVEYORS = gsub("  ", " ", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(" ,", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(";", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",,,", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",,", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(", ", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",  ", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(" and ", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(".", "", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(" with ", ",", pi_data_mike2$SURVEYORS, fixed=T)}

#FIX INCONSISTENT SURVEYORS NAMES. XXX MARKS BEST GUESSES
{pi_data_mike2$SURVEYORS = gsub("A Doll", "Adam Doll", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("B Hummel", "Brittany Hummel", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("A Londo", "April Londo", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Eric F,", "Eric Fieldseth,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Angie Les", "Angela Les", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Hallie M Jensen", "Hallie Jensen", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Josh Curtin", "Joshua Curtin", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("J Bjorklund", "Jill Sweet", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("M Kocian", "Matt Kocian", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Jill B,", ",Jill Sweet,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Brunell,", "Brunelle,", pi_data_mike2$SURVEYORS, fixed=T)#XXX
  pi_data_mike2$SURVEYORS = gsub("James A Johnson", "James Johnson", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("JA Johnson", "James Johnson", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Emmy Hauck", "Emelia Hauck Jacobs", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Jonathan D JaKa", "John Jaka", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Josh Knopik", "Joshua Knopik", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Joshua M Knopik", "Joshua Knopik", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Katie W,", "Katie Wigen,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("K Lund", "Keegan Lund", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Kelly D,", "Kelly Dooley,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Kris Carlson", "Kristin Carlson", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("K Bloodsworth", "Kylie Bloodsworth", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("K Cattoor", "Kylie Cattoor", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Kailey K,", "Kailey Kreatz,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("M Verhoeven", "Michael Verhoeven", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Mike Verhoeven", "Michael Verhoeven", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Nik Myrha,", "Nik Myrha-Edwards,", pi_data_mike2$SURVEYORS, fixed=T)#XXX
  pi_data_mike2$SURVEYORS = gsub("Yvette C,", "Yvette Christianson,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Nick Whichello", "Nick Wichello", pi_data_mike2$SURVEYORS, fixed=T)#XXX
  pi_data_mike2$SURVEYORS = gsub("Todd P,", "Todd Piepho,", pi_data_mike2$SURVEYORS, fixed=T)#XXX
  pi_data_mike2$SURVEYORS = gsub("Marty Evans", "Martin Evans", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("R Contreras,", "Rafa Contreras,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("R Roche", "Rochelle Roche", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Rafa Contrera,", "Rafa Contreras,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Allly", "Ally", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Carstenen", "Carstensen", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Jill,", "Jill Sweet,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Crowell,", ",Wendy Crowell,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Kailey,", ",Kailey Kreatz,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("LW,", "Lucas Wandrie,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Drake", ",Christa Drake", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Einck", ",Alan Einck", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Hennen", ",Matt Hennen", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",JN,", ",Joe Norman,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Mottl", ",E Mottl", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Proulx", ",Nick Proulx", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Sean,", ",Sean Sissler,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Travis", ",Mitch Travis", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Uhler", ",Kyle Uhler", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Yvetter,", "Yvette Christianson,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Eric,", ",Eric Fieldseth,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Adam,", ",Adam Doll,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Eisterhold", ",Joe Eisterhold", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(",Haworth,", ",Matt Horwath,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Matt,", "Matt Hennen,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Mike Veerhoven", "Michael Verhoeven", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("MVerhoeven", "Michael Verhoeven", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Scott M,", "Scott Mackenthun,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("AR,", "Adam Rollins,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("CP,", "Christine Powell,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("M Bockman,", "M Bokman,", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub("Jill Bjorklund,", "Jill Sweet,", pi_data_mike2$SURVEYORS, fixed=T)}

#ELIMINATE ODDS AND ENDS SUBSTRINGS IN SURVEYORS FIELD
{pi_data_mike2$SURVEYORS = gsub("LLC", "", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub(" (first part of day)", "", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub(" (UMN)", "", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("(7/16 only)", "", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("/", ",", pi_data_mike2$SURVEYORS, fixed=T)
  pi_data_mike2$SURVEYORS = gsub(" (Lincoln County)", "", pi_data_mike2$SURVEYORS, fixed = T)
  pi_data_mike2$SURVEYORS = gsub("(Scott Co)", "", pi_data_mike2$SURVEYORS, fixed = T)}

#REPLACE UNREPAIRABLE SINGLE NAME STRINGS IN SURVEYOR COLUMN WITH THE ANONYMOUS STRING.
{pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Marcie(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Wenck(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Alex(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Andy(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Tom(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Chase(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Mulu(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Jake(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Bri(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Chris(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Marissa(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Weston(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Johanna(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)DA(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)RP(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Crowell(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Adam(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)A Edgcumbe(?=,|$)", "Andrew Edgcumbe", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)AR(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Blue Water Science(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)CP(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Eisterhold(?=,|$)", "Joe Eisterhold", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Eric(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Hennen(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Jill(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)JN(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Kailey(?=,|$)", "Kailey Kreatz", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Kailey K(?=,|$)", "Kailey Kreatz", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Katie W(?=,|$)", "Katie Wigen", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Kelly D(?=,|$)", "Kelly Dooley", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Kylie Bloodsworth (?=,|$)", "Kylie Bloodsworth", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Liz(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)LW(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)M Bockman(?=,|$)", "Melissa Bokman", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)M Bokman(?=,|$)", "Melissa Bokman", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Matt Kocian (?=,|$)", "Matt Kocian", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)MN DNR(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Nate(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Ramsey Conservation District(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Ramsey Co conservation district(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)RWMWD Staff(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Ray Newman(?=,|$)", "Raymond Newman", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Scott M(?=,|$)", "Scott Mackenthun", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Sean(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Three Rivers Park District(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Yvetter(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Yvette C(?=,|$)", "Yvette Christianson", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)E(?=,|$)", "Unnamed hardworking surveyor(s)", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Jason Brunell(?=,|$)", "Jason Brunelle", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Jill B(?=,|$)", "Jill Bjorklund", pi_data_mike2$SURVEYORS, perl = T)
  pi_data_mike2$SURVEYORS = gsub("(?<=,|^)Nik Myrha(?=,|$)", "Nik Myrha-Edwards", pi_data_mike2$SURVEYORS, perl = T)
}

#SUBMITTERS
#FIX DISCREPANCIES IN ONE SUBMITTER'S NAME
pi_data_mike2$SUBMITTER_NAME[grepl("Three Rivers Parks District", pi_data_mike2$SUBMITTER_NAME)] = 
  "Three Rivers Park District"

#MAKE ALL THE COLUMNS IN BOTH DBS THE EXACT SAME FOR PROPER MATCHING.
proper_types = rep(NA, length.out = ncol(pi_data_mike2))
all_types = unname(unlist(lapply(newDB, class)))

#UNIFY THE COLUMN CLASSES.
for(i in 1:ncol(pi_data_mike2)) {
  
  cur_col = names(pi_data_mike2)[i]
  cur_match = which(names(newDB) == cur_col)
  proper_types[i] = all_types[cur_match]
  
}
pi_data_mike3 = pi_data_mike2

for(j in 1:ncol(pi_data_mike3)) {
  
  proper_class = proper_types[j]
  if(proper_class == "numeric") {
    pi_data_mike3[[j]] <- as.numeric(pi_data_mike3[[j]])
  } else if(proper_class == "integer") {
    pi_data_mike3[[j]] <- as.integer(pi_data_mike3[[j]])
  } else if(proper_class == "character") {
    pi_data_mike3[[j]] <- as.character(pi_data_mike3[[j]])
  }
  
}

#MAKE MIKE'S DB INTO A VERSION WITH ALL THE NEW DB'S COLUMNS.

fakeDB = data.frame(newDB[0,])
mike_2join = bind_rows(fakeDB, pi_data_mike3)

#WHAT SURVEYS NEED LINKING?
surveys2link = mike_2join %>% 
  group_by(DOW, SURVEY_START) %>% 
  distinct(DOW, SURVEY_START)

#EQUALITY OPERATOR THAT DOESN'T GET PHASED BY NAS
`%==%` <- function(x, y) {
  ifelse(is.na(x) & is.na(y), TRUE, 
         ifelse(is.na(x) | is.na(y), FALSE,
                x==y))
}

#FIGURE OUT THE TAXONOMIC COLUMNS--WE'LL ONLY WORRY ABOUT MATCHING THOSE. 
taxonomic_names = tidyName(
  unique(newfieldnames$newfieldname[newfieldnames$taxonomic == "Y"])
)
taxonomic_names_real = taxonomic_names[taxonomic_names %in% names(newDB)]

those_columns = which(names(newDB)[names(newDB) != "RAKE_MAX"] %in% taxonomic_names_real)

#THIS CODE TELLS ME THAT SO LONG AS ROW NUMBERS MATCH AND STATION NUMBERS MATCH, THEN ALL TAXONOMIC DATA ALWAYS MATCHES. BUT THOSE FIRST TWO CONDITIONS ARE NOT ALWAYS MET--ABOUT 600 OR SO SURVEYS DON'T MEET THEM. FOR THE REST, WE COULD ONE-TO-ONE SUB PRETTY SAFELY, SO LONG AS THE STATION NUMBERS TRULY DO MATCH PERFECTLY IN BOTH DIRECTIONS. 
# checks = rep(FALSE, nrow(surveys2link)) 
# for(k in 1:nrow(surveys2link)) {
#   
#   if(k %% 100 == 0) {
#     print(k)
#   }
#   
#   curr.datDB = newDB %>% 
#     filter(DOW == surveys2link$DOW[k] &
#              SURVEY_START == surveys2link$SURVEY_START[k]) 
# 
#   curr.datMike = mike_2join %>% 
#     filter(DOW == surveys2link$DOW[k] &
#              SURVEY_START == surveys2link$SURVEY_START[k]) 
# 
#   check1 = nrow(curr.datDB) == nrow(curr.datMike)
#   
#   if(check1) {
#     check2 = all(curr.datMike$sta_nbr %in% curr.datDB$sta_nbr)
#     
#     if(check2) {
#       
#       curr.datDB = curr.datDB %>% 
#         arrange(sta_nbr) %>% 
#         select(-RAKE_MAX) %>% 
#         mutate(across(all_of(taxonomic_names_real), ~ ifelse(is.na(.) | . == 0 | . == "" | . == " " | is.null(.), 0, 1)))
#       
#       curr.datMike = curr.datMike %>% 
#         arrange(sta_nbr) %>% 
#         tibble() %>% 
#         select(-RAKE_MAX)  %>% 
#         mutate(across(all_of(taxonomic_names_real), ~ ifelse(is.na(.) | . == 0 | . == "" | . == " " | is.null(.), 0, 1)))
#       
#       check3 = all(curr.datDB[,taxonomic_names_real] %==% curr.datMike[, taxonomic_names_real])
#       
#       if(check3) {
#         checks[k] = TRUE
#       } else {
#         checks[k] = names(curr.datMike[, taxonomic_names_real])[which(apply(curr.datDB[,taxonomic_names_real] %==% curr.datMike[, taxonomic_names_real], 2, function(x) {
#           any(x == FALSE)
#         }) == TRUE)]
#       }
#     }
#   }
# 
# }

#HAVING CONFIRMED THAT ALL ROW NUMBERS MATCH AND ALL STATION NUMBERS MATCH IN BOTH DIRECTIONS FOR 2921 SURVEYS (SEE COMMENTED OUT CODE BELOW), WE CAN ONE TO ONE SUB THOSE SURVEYS INTO THE NEW DB FROM MIKE'S DB. WE DO THIS BY FILTERING OUT THEIR MATCHES FROM THE DB AND THEN BIND_ROWING IN THEIR COUNTERPARTS FROM MIKE'S DB.
#checks = rep(FALSE, nrow(surveys2link)) 

for(k in 1:nrow(surveys2link)) {
  
  if(k %% 25 == 0) {
    print(k)
  }
  
  #GRAB BOTH CURRENT SUBSETS (THE SURVEY DATA FROM EACH DB)
  curr.datDB = newDB %>% 
    filter(DOW == surveys2link$DOW[k] &
             SURVEY_START == surveys2link$SURVEY_START[k]) 
  
  curr.datMike = mike_2join %>% 
    filter(DOW == surveys2link$DOW[k] &
             SURVEY_START == surveys2link$SURVEY_START[k]) 
  
  #IF THE ROWS MATCH IN NUMBER
  check1 = nrow(curr.datDB) == nrow(curr.datMike)
  
  if(check1) {
    #AND ALL STATION NUMBERS MATCH
    check2 = all(curr.datMike$sta_nbr %in% curr.datDB$sta_nbr) &&
                   all(curr.datDB$sta_nbr %in% curr.datMike$sta_nbr)
    
    if(check2) {
    # checks[k] = TRUE
      
      #YANK OUT THE OLD RECORDS FROM THE DB
      newDB = newDB %>% 
        filter(!(DOW == surveys2link$DOW[k] &
                 SURVEY_START == surveys2link$SURVEY_START[k]))
      
      #DO THE JOIN WITH MIKE'S REPLACEMENT DATA
      newDB = bind_rows(newDB, curr.datMike)
  
      #CUT THE JOINED-IN RECORDS FROM THE NEW RECORDS SO WE DON'T ENCOUNTER THEM AGAIN.    
      mike_2join = mike_2join %>% 
        filter(!(DOW == surveys2link$DOW[k] &
                 SURVEY_START == surveys2link$SURVEY_START[k]))
      
    }
  }
  
}

#NOW, WE HAVE TO FIGURE OUT WHY THOSE 600ISH SURVEYS FAIL
#INTERMEDIATE OBJECTS AND A REFRESH OF SURVEYS REMAINING TO JOIN
newDB2 = newDB
mike_2join2 = mike_2join

surveys2link2 = mike_2join2 %>% 
  group_by(DOW, SURVEY_START) %>% 
  distinct(DOW, SURVEY_START) %>% 
  arrange(DOW, as_date(SURVEY_START))

#A LOT OF THESE ARE JUST SURVEYS THAT HAVE GOTTEN LUMPED TOGETHER UNDER AN EARLIER START DATE.
#THOSE NEED TO GET MEANINGFULLY COMBINED, AND THEN WE COULD RUN THE PREVIOUS CODE AGAIN FOR THOSE.
#HOWEVER, THERE WILL STILL BE SOME STRAGGLERS THAT WILL NEED TO BE DEALT WITH, PERHAPS AD HOC. 

#FIX SOME COMBINATION ERRORS I SPOTTED IN THE DB WHILE DOING ALL THIS.
newDB$SURVEY_START[newDB$DOW == "40011700" & newDB$SURVEY_START == "2015-06-23"] = "2015-06-15"
newDB$SURVEY_START[newDB$DOW == "27019200" & newDB$SURVEY_START == "2009-05-15"] = "2009-05-10"


#THIS CODE IS VERBATIM FROM INVESTIGATING_CONSECUTIVE_SURVEYS.R--WE JUST NEED TO SEE WHAT SURVEYS ARE REALLY ALL ONE SURVEY.
results.df = data.frame(DOW = character(), 
                        SURVEY1 = character(), 
                        SURVEY2 = character(), 
                        overlapping = logical(),
                        same_length = logical(),
                        daydiff = numeric())

mike_2join2 = mike_2join2 %>% 
  arrange(DOW, as.Date(SURVEY_START))

for(i in 1:length(unique(mike_2join2$DOW))) {
  
  if(i %% 100 == 0) {print(i)}

  curr.lake = unique(mike_2join2$DOW)[i]
  
  curr.dat1 = mike_2join2 %>% 
    filter(DOW == curr.lake)
  
  if(curr.lake == "62023100") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2023-07-17" &
               SURVEY_START != "2022-07-26")
    
  }
  
  if(curr.lake == "27104501") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2020-04-18")
    
  }
  
  if(curr.lake == "27104200") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2020-04-18")
    
  }
  
  if(curr.lake == "27007800") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2017-06-20" &
               SURVEY_START != "2019-06-03")
    
  }
  
  if(curr.lake == "10004800") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2023-07-14" &
               SURVEY_START != "2020-07-16" &
               SURVEY_START != "2021-05-28" &
               SURVEY_START != "2022-09-02")
    
  }
  
  if(curr.lake == "27104200") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2020-04-25")
    
  }
  
  if(curr.lake == "27004400") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2020-04-25")
    
  }
  
  if(curr.lake == "27003900") {
    
    curr.dat1 = curr.dat1 %>%
      filter(SURVEY_START != "2015-06-04")
    
  }
  
  curr.dat2 = curr.dat1 %>% 
    filter(!is.na(sta_nbr))
  
  if(length(unique(curr.dat1$SURVEY_START)) != 1) {
    
    if(length(unique(curr.dat1$SURVEY_START)) == 2) {
      
      if(abs(time_length(difftime(as.Date(unique(curr.dat1$SURVEY_START)[1]),
                                  as.Date(unique(curr.dat1$SURVEY_START)[2])),
                         "days")) < 21) {
        
        daydifference = abs(time_length(difftime(as.Date(unique(curr.dat1$SURVEY_START)[1]),
                                                 as.Date(unique(curr.dat1$SURVEY_START)[2])),
                                        "days"))
        
        overlap = FALSE
        samelength = FALSE
        
        if(nrow(curr.dat2) > 0) {
          
          curr.dat3a = curr.dat2 %>% 
            filter(SURVEY_START == unique(curr.dat1$SURVEY_START)[1])
          
          if(nrow(curr.dat3a) > 0) {
            
            curr.dat3b = curr.dat2 %>% 
              filter(SURVEY_START == unique(curr.dat1$SURVEY_START)[2])
            
            if(nrow(curr.dat3b) > 0) {
              
              if(any(curr.dat3a$sta_nbr %in% curr.dat3b$sta_nbr)) {
                overlap = TRUE
              }
              
              if(nrow(curr.dat3a) == nrow(curr.dat3b)) {
                samelength = TRUE
              }
              
            }
            
          }
          
        }
        
        results.df = rbind(results.df, 
                           data.frame(DOW = curr.dat1$DOW[1], 
                                      SURVEY1 = unique(curr.dat1$SURVEY_START)[1],
                                      SURVEY2 = unique(curr.dat1$SURVEY_START)[2],
                                      SUBMITTER = curr.dat1$SUBMITTER_NAME[1],
                                      overlapping = overlap,
                                      same_length = samelength,
                                      daydiff = daydifference))
        
      }
      
    } else {
      
      uniq_times = unique(curr.dat1$SURVEY_START)
      
      for(j in 1:(length(uniq_times) - 1)) {
        
        if(abs(time_length(difftime(as.Date(uniq_times[j]),
                                    as.Date(uniq_times[j+1])),
                           "days")) < 21) {
          
          daydifference = abs(time_length(difftime(as.Date(uniq_times[j]),
                                                   as.Date(uniq_times[j+1])),
                                          "days"))
          
          overlap = FALSE
          samelength = FALSE
          
          if(nrow(curr.dat2) > 0) {
            
            curr.dat3a = curr.dat2 %>% 
              filter(SURVEY_START == uniq_times[j])
            
            if(nrow(curr.dat3a) > 0) {
              
              curr.dat3b = curr.dat2 %>% 
                filter(SURVEY_START == uniq_times[j + 1])
              
              if(nrow(curr.dat3b) > 0) {
                
                if(any(curr.dat3a$sta_nbr %in% curr.dat3b$sta_nbr)) {
                  overlap = TRUE
                }
                
                if(nrow(curr.dat3a) == nrow(curr.dat3b)) {
                  samelength = TRUE
                }
                
              }
              
            }
            
          }
          
          
          results.df = rbind(results.df, 
                             data.frame(DOW = curr.dat1$DOW[1], 
                                        SURVEY1 = uniq_times[j],
                                        SURVEY2 = uniq_times[j+1],
                                        SUBMITTER = curr.dat1$SUBMITTER_NAME[1],
                                        overlapping = overlap,
                                        same_length = samelength,
                                        daydiff = daydifference))
          
          
        }
        
      }
      
    }
    
  } 
  
}


#NOW, THIS CODE IS VERBATIM FROM FIXED_CONSEC_SURVEYS.R. WE REPAIR THE SURVEY_START DATES IN MIKE'S NEW DATA AS WITH IN THE OLD DB SO THEY WILL HOPEFULLY MATCH. 
results.comb = results.df

results.comb$SURVEY1 = as.Date(results.comb$SURVEY1)
results.comb$SURVEY2 = as.Date(results.comb$SURVEY2)

results.comb = results.comb %>% 
  arrange(DOW, desc(SURVEY2))

records.overwritten = 0

for(i in 1:nrow(results.comb)) {
 
  mike_2join2$SURVEY_START[mike_2join2$SURVEY_START == as.character(results.comb$SURVEY2[i]) &
                             mike_2join2$DOW == results.comb$DOW[i]
  ] = as.character(results.comb$SURVEY1[i])

  records.overwritten = records.overwritten + 1
  
}


#WE RUN THE FIND AND REPLACE CODE AGAIN HERE JUST AS WITH ABOVE--HOPEFULLY, THIS TIME, WE MATCH PROPERLY BY SURVEY_START DATE.
for(k in 1:nrow(surveys2link2)) {
  
  if(k %% 25 == 0) {
    print(k)
  }
  
  curr.datDB = newDB2 %>% 
    filter(DOW == surveys2link2$DOW[k] &
             SURVEY_START == surveys2link2$SURVEY_START[k]) 
  
  curr.datMike = mike_2join2 %>% 
    filter(DOW == surveys2link2$DOW[k] &
             SURVEY_START == surveys2link2$SURVEY_START[k]) 
  
  check1 = nrow(curr.datDB) == nrow(curr.datMike)
  
  if(check1) {
    check2 = all(curr.datMike$sta_nbr %in% curr.datDB$sta_nbr) &&
      all(curr.datDB$sta_nbr %in% curr.datMike$sta_nbr)
    
    if(check2) {

      newDB2 = newDB2 %>% 
        filter(!(DOW == surveys2link2$DOW[k] &
                   SURVEY_START == surveys2link2$SURVEY_START[k]))
      
      newDB2 = bind_rows(newDB2, curr.datMike)
         
      mike_2join2 = mike_2join2 %>% 
        filter(!(DOW == surveys2link2$DOW[k] &
                   SURVEY_START == surveys2link2$SURVEY_START[k]))
      
    }
  }
  
}

#MORE TEMP OBJECTS AND MORE INVESTIGATING FOR THE REMAINING 21 OR SO THAT STILL HAVEN'T MATCHED.
newDB3 = newDB2
mike_2join3 = mike_2join2

surveys2link3 = mike_2join3 %>% 
  group_by(DOW, SURVEY_START) %>% 
  distinct(DOW, SURVEY_START) %>% 
  arrange(DOW, as_date(SURVEY_START))

#FIX ANOTHER SET OF ERRORS--THREE OF THESE NON MATCHES ARE A QUIRK WITH STATION NUMBERING. 
newDB3$substrate[newDB3$substrate == ""] = NA
newDB3$sta_nbr[newDB3$sta_nbr == "1e+05"] = "100000"
newDB3$sta_nbr[newDB3$sta_nbr == "2e+05"] = "200000"
newDB3$sta_nbr[newDB3$sta_nbr == "3e+05"] = "300000"

#THE REMAINING SURVEYS HAVE A MISMATCH, SOMETIMES SUBSTANTIAL, IN THE NUMBER OF ROWS AND THE MIX OF STATION NUMBERS. IN ALL BUT ONE, MIKE'S NEW DATA IS LONGER. MIKE AND I DECIDED THAT IN ALL THESE REMAINING CASES (18 OF THEM) WE'RE JUST GOING TO GO WITH THE LONGER OF THE TWO DATASETS FOR NOW, WHICH WILL USUALLY BE MIKE'S NEWER ONES. I WILL APPEND A COMMENT IN THE APPROPRIATE COLUMN (SURVEY NOTES) TO NOTE THE QUESTIONABLENESS OF THIS, BUT THESE ARE ALL DNR RECORDS THAT WILL BE REVISITED WHEN THE DNR DUMPS THEIR DATA ANYWAY. 

# checksA = rep(NA, nrow(surveys2link3))
# checksB = rep(NA, nrow(surveys2link3))
for(k in 1:nrow(surveys2link3)) {
  
  if(k %% 25 == 0) {
    print(k)
  }
  
  curr.datDB = newDB3 %>% 
    filter(DOW == surveys2link3$DOW[k] &
             SURVEY_START == surveys2link3$SURVEY_START[k]) 
  
  curr.datMike = mike_2join3 %>% 
    filter(DOW == surveys2link3$DOW[k] &
             SURVEY_START == surveys2link3$SURVEY_START[k]) 
  
  if(nrow(curr.datMike) > nrow(curr.datDB)) {
    
    newDB3 = newDB3 %>% 
      filter(!(DOW == surveys2link3$DOW[k] &
                 SURVEY_START == surveys2link3$SURVEY_START[k]))
    
    curr.datMike$survey_notes = "MAISRC was apparently sent two versions of this survey's data set. These data are from the longer of the two sets; we do not know if it is the 'correct' set."
    
    newDB3 = bind_rows(newDB3, curr.datMike)
    
    mike_2join3 = mike_2join3 %>% 
      filter(!(DOW == surveys2link3$DOW[k] &
                 SURVEY_START == surveys2link3$SURVEY_START[k]))
    
  } else {
    
    mike_2join3 = mike_2join3 %>% 
      filter(!(DOW == surveys2link3$DOW[k] &
                 SURVEY_START == surveys2link3$SURVEY_START[k]))
    
  }
  
  # check1 = nrow(curr.datDB) == nrow(curr.datMike)
  # 
  # if(check1) {
  #   check2 = all(curr.datMike$sta_nbr %in% curr.datDB$sta_nbr) &&
  #     all(curr.datDB$sta_nbr %in% curr.datMike$sta_nbr)
  #   
  #   if(check2) {
      
      newDB3 = newDB3 %>% 
      filter(!(DOW == surveys2link3$DOW[k] &
                    SURVEY_START == surveys2link3$SURVEY_START[k]))
       
      newDB3 = bind_rows(newDB3, curr.datMike)
          
      mike_2join3 = mike_2join3 %>% 
        filter(!(DOW == surveys2link3$DOW[k] &
                   SURVEY_START == surveys2link3$SURVEY_START[k]))
      
  #   } else {
  #     checksA[k] = paste0(c(curr.datMike$sta_nbr[!curr.datMike$sta_nbr %in% curr.datDB$sta_nbr], 
  #                          curr.datDB$sta_nbr[!curr.datDB$sta_nbr %in% curr.datMike$sta_nbr]), collapse = ",")
  #   }
  # } else {
  #   checksB[k] = nrow(curr.datMike) - nrow(curr.datDB)
  # }
  
}

#FIX ONE MORE NEW DISCREPANCY.
newDB3$RAKE_MAX[is.na(newDB3$RAKE_MAX)] = "Unknown"
newDB3$RAKE_MAX[newDB3$RAKE_MAX == "Something else (explain later please!)"] = "Unknown"
#I WRITE OUT THE FINAL NEWDB HERE AT THIS POINT USING CODE IN THE APPROVALS FILES.