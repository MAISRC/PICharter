#THIS FILE SHOULD NO LONGER NEED TO BE RUN--IT BROUGHT MIKE'S ORIGINAL DATABASE FILE UP TO THE "MODERN STANDARDS" UTILIZED BY FILES OUTPUTTED BY PI CHARTER.

# #LOAD PACKAGES (IT MAY ALSO BE NECESSARY TO LOAD SOME FILES FROM GLOBAL.R)
library(tidyverse)
library(arrow)

#GET PATH TO MIKE'S DB FILE
file_path = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Big Data\\plants_env_data_wide.csv"

#LOAD DATA INTO MEMORY, IGNORING UNNEEDED COLUMNS ON LOAD
pi_data_mike = read.csv(file_path) %>% 
  select(-Secchi_m, -SECCHI_DATE, -SURVEY_ID, -TAXON, -OBS_ID, -order_ID, 
         -SECCHI_m_ACCEPTED, -proplight, -richness, -nat_richness)

#LOAD IN COMMON/SCI NAME LOOKUP TABLE
commonsciNameLookup = read.csv("inputs/Static/commonsci_name_lookup.csv")

#CHECK FOR DATA ANOMOLIES
which(is.na(pi_data_mike$DOW)) #850 OBSERVATIONS HAVE NA FOR DOW.
#SAVE BIG SOB
pi_data_mike$DOW[pi_data_mike$LAKE_NAME == "big sob"] = "10027600"
#EXCLUDE THE REST. MAYBE WE CAN SAVE THEM SOME DAY, BUT THEY ARE DOW-LESS WATERBODIES... 
pi_data_mike = pi_data_mike %>% 
  filter(!is.na(DOW))

#FIX ANY 7 DIGIT DOWS
pi_data_mike$DOW = as.character(pi_data_mike$DOW)
pi_data_mike$DOW[nchar(pi_data_mike$DOW) == 7] = paste0("0", 
                                                        pi_data_mike$DOW[nchar(pi_data_mike$DOW) == 7])

#RENAME COLUMNS TO BE IN LINE WITH WHAT THE APP NOW EXPECTS
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
         nitellopsis_sp = Nitellopsis,
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
db_path = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_new.parquet"

newDB = read_parquet(db_path) #READ NEW DB FILE

#FIND MISMATCHING NAMES
names(pi_data_mike)[!names(pi_data_mike) %in% names(newDB)]

#RENAME ADDITIONAL COLUMNS
pi_data_mike = pi_data_mike %>% 
  rename(DOW = dow,
         SURVEY_START = survey_date,
         SUBMITTER_NAME = survey_datasource,
         SURVEYORS = surveyor,
         sta_nbr = point_id,
         depth = depth_ft, 
         sta_nbr = point_id,
  )

# #FIGURE OUT WHICH TAXA, IF ANY, MIGHT BE TYPOS BY SCANNING OUR COLUMN NAME LOOKUP TABLE
# names(pi_data_mike)[!names(pi_data_mike) %in% tidyName(newfieldnames$newfieldname)]
#DID THIS, CORRECTED ALL MISSING ENTRIES

#HOW MANY ARE MISSING FROM THE LOOKUP TABLE FOR COMMON NAMES?
# names(pi_data_mike)[!names(pi_data_mike) %in% commonsciNameLookup$newfieldname]
#DID THIS, CORRECTED ALL MISSING ENTRIES

#WRITE FILE
write.csv(pi_data_mike, "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Big Data\\picharterlegacydb.csv")