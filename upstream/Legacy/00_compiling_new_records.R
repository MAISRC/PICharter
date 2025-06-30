#This file brings in and aggregates all the files currently sitting in the "Approved" subfolder of the submitted_cleandata folder on the GDrive, compiling them in a mini-database file that could one day be joined to the larger, complete database file. It also creates a lake-level summary for use in the App on the Browse and Leaderboard tabs.

stop("Alex, did you remember to save the current version of the database file as an archived version??")

# Load packages -----------------------------------------------------------

library(lubridate)
library(sf)
library(gargle)
library(googledrive)
library(googlesheets4)
library(data.table)
library(tidyverse)


# Access the GDrive's Approved folder and discover contents ---------------


#Gets the ID of the folders on the Gdrive--some we need to acknowledge and others we need to ignore.
approved.id = googledrive::drive_get("https://drive.google.com/drive/u/0/folders/17YNVoTJ6cTVjaTzuDNLy48DsL1VhSQ_L")$id #Acknowledge and access.
rejected.folder = googledrive::drive_get("https://drive.google.com/drive/folders/1xIhlsGgU-JYh4qeKoBqTTcpm7NJDH3HI")$id #Ignore.
compiled.folder = googledrive::drive_get("https://drive.google.com/drive/folders/1DgBorlz_n0RLPKCpQsA0OAlOgXt8UG_A")$id #Ignore.
needloc.folder = googledrive::drive_get("https://drive.google.com/drive/folders/1DpG5FCZka1sXx-ztuEOitVA8MpRdx5r4")$id #Ignore.
locreq.folder = googledrive::drive_get("https://drive.google.com/drive/folders/1y23V14XXWNY0sb-0ri6-Sx250uKYxT9H")$id #Ignore.

#Get a list of the files (as IDs) in the approved folder. We will load and process them one at a time.
content.ids = drive_ls(approved.id)$id #The contents of the folder as Gdrive IDs.
content.ids = content.ids[!as.character(content.ids) %in%  #Eliminate the folders to ignore from further consideration.
                            as.character(c(rejected.folder, compiled.folder, 
                                           needloc.folder, locreq.folder))]


# Load, process, and compile approved Gdrive files into dataframe- --------


#For every file, select its ID, load it in as a csv file, and glue it using rbindlist to the growing stack of such files to create a single compiled dataframe object.
for(n in 1:length(content.ids)) {
  if(n %% 5 == 0) { print(paste0("Now on ", n, " of ", length(content.ids))) } #Progress bar.
  id <- content.ids[n] #Current ID
  current.import = drive_read_string(id, encoding = "UTF-8") %>% #Read the file temporarily as a string, then read it as a csv. This step is described somewhere deep in the instructions and is the only way to easily do this!
    read.csv(text = .)
  
  #Put in a break if we find any files that claim to have a DOW of NA--those must contain errors and will need fixing. ****Should pitch all-NA rows automatically here, since it looks like I'm the one introducing these often...
  if(any(is.na(current.import$DOW))) { print(n); stop("No DOWs of NA allowed!") }
  
  #Some mechanisms for catching typos or mistaken column names identified downstream
  if(any(c("Dulichium.arundinaceum","Eleocharis", "potamogeton_zostriformis", "ultricularia_intermedia", "dulichium.arundinaceum") %in% names(current.import))) { print(n); stop("Bad name!")}
  
  #Grow the compiled file.
  if(n == 1) {
    grow.dat = current.import
  } else {
    grow.dat = rbindlist(list(grow.dat, current.import), fill=TRUE) #For doc on rbindlist's usages-->https://stackoverflow.com/questions/18003717/efficient-way-to-rbind-data-frames-with-different-columns
  }
}


# Post-compilation tidying and error-checking ------------------------------------------------


#For the filtering about to happen, it's useful if any blank values get replaced with NAs.
grow.dat[grow.dat == ""] = NA

#Move the metadata to the beginning of the file from wherever it gets shoved in.
grow.dat = grow.dat %>% #For details on using everything and select in this way-->https://stackoverflow.com/questions/22286419/move-a-column-to-first-position-in-a-data-frame
  select(SUBMITTER_NAME, SUBMITTER_EMAIL, DOW, SURVEY_START, 
         DEPTH_UNITS, RAKE_MAX, SUBMIT_TIME, SURVEYORS, everything()) %>% 
  filter(rowMeans(is.na(.)) < 1) #Remove all rows that are completely blank. I'm guessing these occasionally come in at the ends of data files. For a sense of how this trick works-->https://stackoverflow.com/questions/41609912/remove-rows-where-all-variables-are-na-using-dplyr

#At this point, I need to do some visual assessments to check to see if the imported data contain any sneaky errors or quirks. Here are some things to look out for:
# Text in columns--while initially undesirable, we're softening our stance on this. Still, good to know when/where it exists.
# Invalid or unlikely values, such as 6s in RAKE_MAX.
# DOWs with very small numbers of entries.
# Odd depth units.
#Below is just a sampling of commands that could be run here...these are the ones that have felt sufficiently informative for me so far.
stop("Check these Alex!")
summary(grow.dat)
table(grow.dat$DOW, grow.dat$SURVEY_START)[table(grow.dat$DOW, grow.dat$SURVEY_START) != 0]
table(grow.dat$DEPTH_UNITS)


# Join to the pre-existing "new" database file ----------------------------


#Pull in the growing "new" portion of the comprehensive database file (as opposed to Mike's) by reading in the db_new file and joining it to the grow.dat object.We add in an if check here to make sure that we always have a grow.dat2 object so that, if any changes need to be made because we spot errors downstream, we don't have to rerun what we've already run to fix it.  
if(any(list.files() %in% "db_new")) {
db.new = readRDS("db_new")
grow.dat2 = rbindlist(list(grow.dat, db.new), fill = T) 
} else {
  grow.dat2 = grow.dat
}

#Store this unified database file as the newest version. Hopefully, I've remembered to archive the old version prior to running  all this! Hence the stop command earlier.
saveRDS(grow.dat2, file = "upstream/db_new")


# Post-archival processing to generate lake-level summary files for the browse and leaderboard tabs --------


#Start building from this our lakes.summ objects, aka lake-level summaries for use in the browse and leaderboard tabs. Remove some unneeded columns from growdat, then group by each unique survey, and then create some summary columns which are uniques of all the data from the individual surveys (aka aggregating to the survey level.)
lakes.summ.new1 = grow.dat2 %>% 
  select(-SUBMITTER_EMAIL, -DEPTH_UNITS, -RAKE_MAX, -SUBMIT_TIME) %>% 
  data.frame()

stop("Alex, pause here to visually inspect the file to make sure we know all the non-taxonomic columns and the next object is comprehensive. Also a good chance to check for typos")
sort(names(lakes.summ.new1))

#*****Here, we need to remove all columns from the database that are not taxonomic columns! This list will need to be periodically updated as new such columns emerge, which should occur relatively rarely...
extracols = c(
  "dreissena_polymorpha", "unk_spp", "sta_nbr", "depth", "substrate", "no_veg_found", "latitude", "longitude", "x", "y", "whole_rake_density", "depth.1", "please_provide_comment", "guid", "sta_nbr.1", "sample_type", "percent_biovolume", "plant_height", "plant_height.1")
removethese = which(names(lakes.summ.new1) %in% extracols)
lakes.summ.new1 = lakes.summ.new1[1:nrow(lakes.summ.new1),-(removethese)]

#We need to remove text values from all taxonomic columns at this point.
#*****This works as long as the metadata always remain the first four columns:

lakes.summ.new1[,5:ncol(lakes.summ.new1)] = data.frame(
  lapply(lakes.summ.new1[,5:ncol(lakes.summ.new1)], 
         function(x) {
           suppressWarnings(as.numeric(as.character(x)))
           }
         )
  )

lakes.summ.new1 = lakes.summ.new1 %>% 
  group_by(DOW, SURVEY_START) %>% 
    summarize(SURVEYORS = first(SURVEYORS),
              SUBMITTER_NAME = first(SUBMITTER_NAME),
              across(!c(SUBMITTER_NAME:SURVEYORS), sum, na.rm=T) 
              ) 

#Build the unified taxa column for each survey by finding all taxonomic columns with positive data over that survey and unifying just those into a string. This step is why the previous one needs regular updating--otherwise, non-taxa end up in here, which just requires me to fish them out again downstream. 
#***For one-off functions like this one, is it ok to make it anonymous? Or should I name it and store it in global.R?
taxacol = apply(lakes.summ.new1[,-(1:4)], 1, function(x) {
  y = names(lakes.summ.new1)[-(1:4)] #Collect all the column names, which represent different taxa.
  ind = which(x > 0) #Figure out which ones were actually found within a given lake's survey records. 
  taxafound = paste(y[ind], collapse=",") #Attach them together into one string per survey.
})

#Joining in this column we just manufactured
lakes.summ.new1$taxafound = taxacol

#Cutting out additional unneeded columns at this stage to shrink complexity.
lakes.summ.new1 = lakes.summ.new1 %>% 
  select(DOW, SURVEY_START, SURVEYORS, SUBMITTER_NAME, taxafound)

#Reformatting the date column as such for better parsing and displaying later.
lakes.summ.new1$SURVEY_START = as_date(lakes.summ.new1$SURVEY_START,
        format = c("%Y-%m-%d", "%m/%d/%Y", "%y-%m-%d")) #Several different formats were viable early on, but now should be restricted to just the middle one I think. Not hurting anything to have them all in here though.

### VISUAL CONFIRMATION STEP--Confirm here that all the dates look operable
unique(lakes.summ.new1$SURVEY_START)

#Further condensing the file into a lake-level summary now by creating metadata columns for displaying information about each lake's survey history, such as when and how many times each lake has been surveyed, collapsing this information into comma-delimited strings. 
lakes.summ.new2 = lakes.summ.new1 %>%
  group_by(DOW) %>%
  summarize(
    DOW = first(DOW),
    nyears = length(unique(str_sub(SURVEY_START, start=1, end=4))), #Pull just the year out of the dates. Lubridate might offer more elegant means...
    nsurveys = length(unique(SURVEY_START)), 
    surveylist = paste(sort(unique(SURVEY_START)), collapse = ","), #Put the survey dates in order.
    datasource = paste(SUBMITTER_NAME, collapse = ","), #This is the submitter.
    taxalist = paste(taxafound, collapse = ","), #All the taxa found across any survey done, with duplicates to be removed later.
    surveyorlist = paste(SURVEYORS, collapse = ",") #The folks who actually physically did the survey.
  )

#Getting the entire lake polygon object from the state hydrology dataset.
# LAKE_POLY = st_read("../dnr_hydro_features_all.shp") #Won't run now...
# LAKE_POLY = LAKE_POLY %>%
#   filter(!is.na(LAKE_POLY$dowlknum)) %>%
#   select(dowlknum, map_label, cty_name, center_utm, center_u_1) %>%
#   st_drop_geometry()
# saveRDS(LAKE_POLY, "upstream/UpstreamInputs/LAKE_POLY")


# Adding additional data to the lake-level summary objects ----------------

#Add in spatial data, such as the lake and county names as well the spatial data for the lake polygon centroid.
LAKE_POLY = readRDS("upstream/UpstreamInputs/LAKE_POLY") #A minimal file holding only the necessary info, as built above.

#Convert the DOW data to character and repair any lacking a leading 0.
lakes.summ.new2$DOW = as.character(lakes.summ.new2$DOW)
lakes.summ.new2$DOW[nchar(lakes.summ.new2$DOW) == 7] = 
  paste0("0", lakes.summ.new2$DOW[nchar(lakes.summ.new2$DOW) == 7])

#Join in the lake spatial data using DOWs to join by to get spatial locations of lakes. Eliminate any duplicates using distinct, as joining seems to often introduce these. 
lakes.summ.new2 = left_join(lakes.summ.new2, LAKE_POLY, by = c("DOW" = "dowlknum")) %>% 
  distinct(DOW, surveylist, .keep_all = TRUE)

#Write this object to file
saveRDS(lakes.summ.new2, file = "upstream/new_db_summary")
