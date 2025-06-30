# Load packages  --------------------

library(stringi) #For removing empty values from character vectors
library(tools) #I forget what this is for now...oops.


# Build convenience functions ---------------------------------------------

#CAPITALIZES THE FIRST CHARACTER OF A STRING
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#FIND AND REMOVE DUPLICATE ENTRIES IN A STRING WHERE COMMAS ARE THE DELIMITERS
split.str_dupe_hunts = function(x) { 
  tmp1 = str_split_1(x, ",") 
  tmp2 = duplicated(tmp1)
  tmp3 = tmp1[tmp2]
  tmp4 = toString(tmp3)
  return(tmp4)
}


# Load Mike's database file -----------------------------------------------

#Load in the database-derived, lake-level summary file compiled by Mike from his time building the database. I still need to receive the code to actually build this myself.
lakes.summ = read.csv("upstream/old_db_summary.csv")

#Load in the new database-derived, lake-level summary file compiled by Alex since booting up the new App and collection system--made in the 00 file upstream. 
lakes.summ.new2 = readRDS("upstream/new_db_summary")

# Hack up Mike's "old" lake-level summary object into a version that looks more like mine --------

#Let's replace all NAs in the surveyors column with a standard "Anonymous" entry across both files, which cannot yet be safely unified.
lakes.summ$surveyorlist = apply(
  data.frame(lakes.summ$surveyorlist), 
  1,  #For each entry in this column...
  function(x) {
    toString( #String it back together into 1 string.
      sort( #Sort the resulting vector
        gsub("Unk", "Unnamed hardworking surveyor(s)", #gsub out all our "unknown" surveyors.
        gsub("NA", "Unnamed hardworking surveyor(s)", 
             paste( #Paste the result back into one vector...can't remember why this is needed now.
        trimws( #Get rid of leading white spaces.
          unlist( #Turn that split string into a vector from the list it becomes.
            strsplit(x, #Split the string at the commas.
                     ",")))
        ),
        fixed = T),
        fixed = T)
        ))
  })
#Mine can be simpler because I'm being more rigorous and consistent about how these get coded in cleaned files in the App than Mike was before the app existed.
lakes.summ.new2$surveyorlist = gsub("NA", "Unnamed hardworking surveyor(s)", 
                                    lakes.summ.new2$surveyorlist)

#There are some records in Mike's data file that are missing some IDing info. As we resolve these, I will add in that data so those records aren't lost.
lakes.summ$DOW[lakes.summ$LAKE_NAME == "big sob"] = "10027600" #This lake also goes by the name "Zen Pond"
lakes.summ$center_utm[lakes.summ$LAKE_NAME == "big sob"] = 441812.1
lakes.summ$center_u_1[lakes.summ$LAKE_NAME == "big sob"] = 4968990.1
lakes.summ$cty_name[lakes.summ$LAKE_NAME == "big sob"] = "Carver"
lakes.summ$LAKE_NAME[lakes.summ$LAKE_NAME == "big sob"] = "Zen Pond/Big Sob"

##Filter out and condense some of the data to make a tidier object for working with. Until and unless we hear back from the DNR Shallow Lakes program, we'll lose 17 other records here due to the complete.cases() call--we don't know their DOWs, counties, or location data at present. 


lakes.summ2 = lakes.summ %>% 
  dplyr::select(DOW, LAKE_NAME, nyears, nsurveys, surveylist, datasource, ntaxa, taxalist, cty_name, center_utm, center_u_1, surveyorlist) %>% 
  filter(complete.cases(.))

##Re-configure the taxonomic data to make them more comma-delimited for later convenience.

lakes.summ2 = lakes.summ2 %>% 
  rowwise() %>% 
  mutate(
    taxalist = paste0(taxalist, ",")
  )

#For the submitter's leaderboard, I need to take Mike's (very differently) formatted datasource data and reformat them into comma-delimined strings with consistent properties, which is what the below does. In particular, it will make counting up how many submissions came from each source more intuitive because there will be one entity to count for each survey each datasource submitted. 
revised.datasources = rep(NA, nrow(lakes.summ2)) #Storage vector.
for(i in 1:nrow(lakes.summ2)) {
  data.sources.temp = toString( #The whole thing here feels pretty convoluted, but basically, if source X submitted three surveys for a lake, they'd only be listed once, and we want them listed three times instead so that each time can be counted, so I have to "expand" the data Mike has provided here to get it into that format.
    trimws(
      str_split_1(
        paste0(
          rep(lakes.summ2$datasource[i], 
              times = lakes.summ2$nsurveys[i]), 
          collapse=","),
        ",")
      ))
  revised.datasources[i] = gsub(", ", ",", data.sources.temp)
}
lakes.summ2$datasource = revised.datasources #Replace the column with this reworked version.


# Clean up Mike's taxonomic data ------------------------------------------


#Mike's taxa are represented somewhat differently than the "new" ones are, so for starters we do a giant gsub pileup here to replace just plain genus names with genus spp. to be more formal. Might not be quite an exhaustive list and may need periodic updating.
{lakes.summ2$taxalist = gsub("Chara,", "Chara spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Carex,", "Carex spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Nitella,", "Nitella spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Najas,", "Najas spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Sagittaria,", "Sagittaria spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Sparganium,", "Sparganium spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Equisetum,", "Equisetum spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Schoenoplectus,", "Schoenoplectus spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Persicaria,", "Persicaria spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Drepanocladus,", "Drepanocladus spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Potamogeton,", "Potamogeton spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Ranunculus,", "Ranunculus spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Lemna,", "Lemna spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Eleocharis,", "Eleocharis spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Typha,", "Typha spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Isoetes,", "Isoetes spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Wolffia,", "Wolffia spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Nymphaeaceae,", "Nymphaeaceae taxa,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Utricularia,", "Utricularia spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Iris,", "Iris spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Salix,", "Salix spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Elodea,", "Elodea spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Hypericum,", "Hypericum spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Impatiens,", "Impatiens spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Myriophyllum,", "Myriophyllum spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Poaceae,", "Poaceae taxa,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Stuckenia,", "Stuckenia spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Scirpus,", "Scirpus spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Eragrostris,", "Eragrostris spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Scutellaria,", "Scutellaria spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Juncus,", "Juncus spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Cyperaceae,", "Cyperaceae taxa,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Cicuta,", "Cicuta spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Eutrochium,", "Eutrochium spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Acorus,", "Acorus spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Zizania,", "Zizania spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Elatine,", "Elatine spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Bidens,", "Bidens spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Nymphaea,", "Nymphaea spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Nuphar,", "Nuphar spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Characeae,", "Characeae taxa,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Lamiaceae,", "Lamiaceae taxa,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Sphagnum,", "Sphagnum spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Alnus,", "Alnus spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Verbena,", "Verbena spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Zosterella,", "Zosterella spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Solidago,", "Solidago spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Asteraceae,", "Asteraceae taxa,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Callitriche,", "Callitriche spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Alisma,", "Alisma spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Riccia,", "Riccia spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Nitellopsis,", "Nitellopsis spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Lysimachia,", "Lysimachia spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Sparganium,", "Sparganium spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Ceratophyllum,", "Ceratophyllum spp.,", lakes.summ2$taxalist)
lakes.summ2$taxalist = gsub("Sparganium (floating)", "Sparganium spp.", lakes.summ2$taxalist, fixed = T)
lakes.summ2$taxalist = gsub("Sparganium (emergent)", "Sparganium spp.", lakes.summ2$taxalist, fixed = T)
lakes.summ2$taxalist = gsub("Potamogeton RTE", "Potamogeton spp.", lakes.summ2$taxalist, fixed = T)
lakes.summ2$taxalist = gsub("Potamogeton (broad)", "Potamogeton spp.", lakes.summ2$taxalist, fixed = T)
lakes.summ2$taxalist = gsub("Potamogeton (narrow)", "Potamogeton spp.", lakes.summ2$taxalist, fixed = T)
lakes.summ2$taxalist = gsub("Eragrostis", "Eragrostis spp.", lakes.summ2$taxalist, fixed = T)
lakes.summ2$taxalist = gsub("Drepanocladus fontinalis", "Drepanocladus or Fontinalis", lakes.summ2$taxalist, fixed = T)}


#Then, for later uses, it's helpful to take those taxonomic data and, for each record, sort them alphabetically, which requires once again chopping them at the commas and converting them to a vector before stringing them back together again. I also chose to do some additional gsubbing here to fix how Mike is representing species that are protected and need to be anonymized within the App.
lakes.summ2$taxalist = apply(
  data.frame(lakes.summ2$taxalist), 
  1,  #For each entry in this column...
  function(x) {
      toString( #String it back together.
        sort( #Sort the resulting vector
          gsub("ProtectedSpecies", "Protected Species", #Insert a space for appearances.
          gsub("_", " ", #Get rid of the underscores on the protected species.
          trimws( #Get rid of leading whitespace.
            unlist( #Turn back into a vector from a list.
              strsplit(x, #Split the string by commas.
                       ","))),
          fixed=T),
          fixed=T)))
})



# Clean up the new taxonomic data -----------------------------------------


#Then, I take the new data I've been pulling in and do a similar working-over of them, gsubbing out some pesky formatting quirks and typos and such as well as capitalizing the first letters of every entry, etc.
lakes.summ.new2$taxalist = apply(
  data.frame(lakes.summ.new2$taxalist), 
  1,  #For each entry in this column...
  function(x) {
    gsub(" sp ", " spp. ",
    gsub("sp,", "spp.,", 
    gsub("Valisneria", "Vallisneria", 
         paste0(
    toString( #String it back together.
      unique( #Get rid of the copied values for here, as they aren't meaningful at this resolution. 
      sort( #Sort the resulting vector
        gsub(".3", "", #Get rid of any of these numerical designations
        gsub(".2", "", 
        gsub(".1", "", 
        gsub("_", " ",    #Get rid of the underscore.
          firstup( #Capitalize just the first letter.
            trimws( #Get rid of leading whitespace.
              unlist( #Turn back into a vector from a list.
                strsplit(#Split the string by commas.
                  paste0( #Place a trailing comma for easier subs for sp
                  x, ","),
                  ",")
                ))), fixed = T),
        fixed=T),
        fixed=T),
        fixed=T)))),
    ","),
    fixed=T),
    fixed=T),
    fixed=T)
  })

#Eliminate some additional pesky words that shouldn't really be represented at the summary level or else are taxonomically misleading that I've noticed have slipped through.
{lakes.summ.new2$taxalist = gsub(" broad", "", lakes.summ.new2$taxalist, fixed = T)
lakes.summ.new2$taxalist = gsub(" narrow", "", lakes.summ.new2$taxalist, fixed = T)
lakes.summ.new2$taxalist = gsub("Drepanocladus fontinalis", "Drepanocladus or Fontinalis", lakes.summ.new2$taxalist, fixed = T)}


# Fixing other metadata in both summary objects ---------------------------


#Fix the faulty lake names in Mike's file:
d1 = lakes.summ2$DOW 
d1[nchar(d1) == 7] = paste0("0",d1[nchar(d1) == 7]) #Append the leading 0 wherever needed.
lakes.summ2$DOW = d1 #Overwrite the original column

#Fix the dates in the new data set and the column name of the lake_name column
lakes.summ.new2$surveylist = as.character(lakes.summ.new2$surveylist)
lakes.summ.new2 = lakes.summ.new2 %>% 
  rename(LAKE_NAME = map_label)


# Begin creating output objects for passing along to the App --------------


#Formally combine the two summary objects into one!
combined_summ_df = lakes.summ2 %>% 
  st_drop_geometry() %>% 
  bind_rows(lakes.summ.new2) %>% 
  data.frame()

#Get rid of some entries that ought not be included moving past this point. *****It looks like this code is still needed. Maybe these errant values could be stripped out of the database once and then wouldn't come through again...especially now that they should be being caught upstream in the 00 file. In the meantime, they aren't hurting anything here and are at least still solving a confirmed problem.
{combined_summ_df$taxalist = gsub(" Please provide comment,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
combined_summ_df$taxalist = gsub(" Dreissena polymorpha,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
combined_summ_df$taxalist = gsub(" X,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
combined_summ_df$taxalist = gsub(" Y,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
combined_summ_df$taxalist = gsub(" Whole rake density,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
combined_summ_df$taxalist = gsub(" Latitude,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
combined_summ_df$taxalist = gsub(" Depth,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
combined_summ_df$taxalist = gsub(" Unk spp.,", replacement = "", combined_summ_df$taxalist, ignore.case = T)
}

#At this point, we need to gsub in "Protected Species Z" for the 12 (at time of writing) Protected Species that Mike had been told we had to anonymize wherever they may exist in a public-facing way on the App. This just uses a convenience lookup table to do the subs.
protecteds = read.csv("upstream/UpstreamInputs/protected_nameskey.csv")
for(r in 1:nrow(protecteds)) {
  combined_summ_df$taxalist = gsub(pattern = protecteds$TAXON[r], 
                     replacement = protecteds$new_name[r],
                     x = combined_summ_df$taxalist)
}


# Combining survey records from across the new and old summaries ----------


#It's possible we now have "new" and "old" lake-level summaries of the same lake as separate rows now, having come from the two different sources. We will now combine them. This is a huge nuisance that requires chopping up each string into vectors, combining them, sorting them, uniquing them, etc. (or some subset, as the particular case may be), then stitching them back together again. 
combined_summ_df = combined_summ_df %>% 
  group_by(DOW) %>% 
  summarize(DOW = first(DOW),
            LAKE_NAME = first(LAKE_NAME),
            surveylist = toString(sort(trimws(str_split_1(paste(surveylist, collapse = ","), ",")))), #No unique! Helps find dupes
            datasource = paste(datasource, collapse = ","), #Acknowledge multiples for later counting, so no unique!
            taxalist = toString(sort(unique(stri_remove_empty(trimws(str_split_1(paste(taxalist, collapse = ", "), ",")),na_empty=FALSE)))), #Need the unique here--we don't need to acknowledge here the same taxon found at a lake multiple times.
            cty_name = first(cty_name),
            center_utm = first(center_utm),
            center_u_1 = first(center_u_1),
            surveyorlist = paste(surveyorlist, collapse = ","),  #Acknowledge multiples for later counting, so no unique!
            ntaxa = length(unique(str_split_1(taxalist, ","))), 
            nyears = length(unique(substr(str_split_1(surveylist, ","), start=1, stop=4))),
            nsurveys = length(unique(str_split_1(surveylist, ",")))
            )


# Checking for any duplicated surveys -------------------------------------

#It's possible a "new" survey submitted to us is actually one we already had! We don't want any in here twice, so we'll now check to see if any duplicates can be found.
#This first bit splits each row's surveylist value by its commas, looks for duplicates, and only keeps the duplicates, concatenating back into strings.
duped_records = unlist(apply(data.frame(combined_summ_df$surveylist), 1, split.str_dupe_hunts))

#Assemble into a nice df for easy visual assessment. 
dupe.surveys = as.vector(duped_records[(duped_records != "")])
dupe.dows = as.vector(combined_summ_df$DOW[(duped_records != "")])
data.frame(dupe.dows, dupe.surveys)
#Warn me explicitly about these and ahlt proceedings if there are any beyond the one we know exists already in Mike's data, which I can't yet clean out. 
if(length(dupe.surveys) > 1 ) { stop("Hey Alex! Duplicate surveys detected my dude!")}


# Creating info for the selectors on the browse tab -----------------------

#Find all the unique taxonomic entries on offer so that we can build a selector to filter our summary records by which lakes have had an observation of a given taxon.
selector_taxa = unique(trimws(str_split_1(paste(combined_summ_df$taxalist, collapse = ","), ",")))
#Get rid of some entries that ought not be included moving past this point. 

selector_taxa = selector_taxa[!grepl("Protected Species", selector_taxa)] #No need to have the Protected Species show up in the selector.

selector_taxa = selector_taxa[-which(selector_taxa == "")] #Take out any errant blank entries that might still be kicking around.

db_allSearchableTaxa = sort(selector_taxa) #Arrange the taxa available alphabetically.

#Remove families from this selector's options cuz they're too much work to reasonable implement. They can still show up in the actual tables though. Families are indicated using the " taxa" tag. 
db_allSearchableTaxa = 
  db_allSearchableTaxa[!grepl(" taxa", db_allSearchableTaxa, ignore.case = T)] 

#Replace spp., as they are represented correctly in the table, with (any)--this will allow me to instead have the selector allow for filtering by any member of a genus, not just "taxa not identified to a lower taxonomic level than genus." In other words, using the same entry in two very different ways to mean different things.
db_allSearchableTaxa = gsub("spp.", "(any)", db_allSearchableTaxa, ignore.case = T) 

#Create a simple vector for a selector of all the counties for which we have at least one record.
db_allCounties = sort(unique(combined_summ_df$cty_name)) 

#Similar situation but for all lakes (as represented by DOWs) for which we have at least survey record. We join in the lake names here in parentheses for a more polished look in the selector.
d3 = paste0(combined_summ_df$DOW, " (", toTitleCase(combined_summ_df$LAKE_NAME), ")")
db_allDOWs = sort(unique(d3))

#We create an alternate version of the combined summary object here that has explicit spatial geometry data attached so that we can plot lake records on the map on the browse tab using this version. The geometry column is a nuisance, so it's useful to have a version with and without it so we can only reference it exactly and only where it's needed. 
lakes.summary.sf = combined_summ_df %>% 
  st_as_sf(coords = c("center_utm",
                      "center_u_1"), 
           crs = 26915) %>% #The data are not in Lat/Long format but rather UTM Zone 15N projection.
  st_sf() %>% 
  st_transform(crs=4326) #But for mapping, it seems Lat/Long is much better, so we convert to it.

lakes.summary.definitive = combined_summ_df #The "plain" file for using when a geometry column is not needed.


# Building and reformatting objects for the leaderboards ------------------

### Resolve any organization name issues for the submitter leaderboard on the leaderboard tab. Almost everything else we do for this leaderboard happens over in the server itself. 
lakes.summary.definitive$datasource = gsub("Three Rivers Parks District", 
                                           "Three Rivers Park District", 
                                           lakes.summary.definitive$datasource)

#For the surveyors leaderboard, we first slam the entire surveyorlist column into a single vector, delimited by commas. 
people.list = paste(combined_summ_df$surveyorlist, collapse = ",")

#We then do a truly massive set of gsubs to polish up this list. 
#First, replacing non-descript things or institution names with our "anonymous" string.
{people.list = gsub("DNR Fisheries", "MN DNR", people.list, fixed=T)
people.list = gsub("Ramsey County - Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("Ramsey County 􀍴 Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("Ramsey County – Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("Ramsey County - Parks & Recreation, Soil and Water Conservation District", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("Ramsey County Soil & Water Conservation Division", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("Ramsey County Parks and Recreation - Soil & Water Conservation Division", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("Ramsey County – Parks & Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("Ramsey County Parks and Recreation, Soil and Water Conservation Division", "Unnamed hardworking surveyor(s)", people.list, fixed=T)
people.list = gsub("DNR Staff", "MN DNR", people.list, fixed=T)
people.list = gsub("?,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("MN DNR,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Blue Water Science,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Ramsey Co conservation district", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Endangered Resource Services,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("RWMWD Staff,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Ramsey Conservation District,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Three Rivers Park District,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)}

#Next, we replace random gaps and random punctuation quirks with clean single commas for nice delimiting.
{people.list = gsub("  ", " ", people.list, fixed=T)
people.list = gsub(" ,", ",", people.list, fixed=T)
people.list = gsub(";", ",", people.list, fixed=T)
people.list = gsub(",,,", ",", people.list, fixed=T)
people.list = gsub(",,", ",", people.list, fixed=T)
people.list = gsub(", ", ",", people.list, fixed=T)
people.list = gsub(",  ", ",", people.list, fixed=T)
people.list = gsub(" and ", ",", people.list, fixed=T)
people.list = gsub(".", "", people.list, fixed=T)
people.list = gsub(" with ", ",", people.list, fixed=T)}

#Third, we replace inconsistent names with consistent, complete names. A handful of these are just educated guesses, since I don't know who these people are...The guesses are marked below.
{people.list = gsub("A Doll", "Adam Doll", people.list, fixed=T)
people.list = gsub("B Hummel", "Brittany Hummel", people.list, fixed=T)
people.list = gsub("A Londo", "April Londo", people.list, fixed=T)
people.list = gsub("Eric F,", "Eric Fieldseth,", people.list, fixed=T)
people.list = gsub("Angie Les", "Angela Les", people.list, fixed=T)
people.list = gsub("Hallie M Jensen", "Hallie Jensen", people.list, fixed=T)
people.list = gsub("Josh Curtin", "Joshua Curtin", people.list, fixed=T)
people.list = gsub("J Bjorklund", "Jill Sweet", people.list, fixed=T)
people.list = gsub("M Kocian", "Matt Kocian", people.list, fixed=T)
people.list = gsub(",Jill B,", ",Jill Sweet,", people.list, fixed=T)
people.list = gsub("Brunell,", "Brunelle,", people.list, fixed=T)#XXX
people.list = gsub("James A Johnson", "James Johnson", people.list, fixed=T)
people.list = gsub("JA Johnson", "James Johnson", people.list, fixed=T)
people.list = gsub("Emmy Hauck", "Emelia Hauck Jacobs", people.list, fixed=T)
people.list = gsub("Jonathan D JaKa", "John Jaka", people.list, fixed=T)
people.list = gsub("Josh Knopik", "Joshua Knopik", people.list, fixed=T)
people.list = gsub("Joshua M Knopik", "Joshua Knopik", people.list, fixed=T)
people.list = gsub("Katie W,", "Katie Wigen,", people.list, fixed=T)
people.list = gsub("K Lund", "Keegan Lund", people.list, fixed=T)
people.list = gsub("Kelly D,", "Kelly Dooley,", people.list, fixed=T)
people.list = gsub("Kris Carlson", "Kristin Carlson", people.list, fixed=T)
people.list = gsub("K Bloodsworth", "Kylie Bloodsworth", people.list, fixed=T)
people.list = gsub("K Cattoor", "Kylie Cattoor", people.list, fixed=T)
people.list = gsub("Kailey K,", "Kailey Kreatz,", people.list, fixed=T)
people.list = gsub("M Verhoeven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("Mike Verhoeven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("Nik Myrha,", "Nik Myrha-Edwards,", people.list, fixed=T)#XXX
people.list = gsub("Yvette C,", "Yvette Christianson,", people.list, fixed=T)
people.list = gsub("Nick Whichello", "Nick Wichello", people.list, fixed=T)#XXX
people.list = gsub("Todd P,", "Todd Piepho,", people.list, fixed=T)#XXX
people.list = gsub("Marty Evans", "Martin Evans", people.list, fixed=T)
people.list = gsub("R Contreras,", "Rafa Contreras,", people.list, fixed=T)
people.list = gsub("R Roche", "Rochelle Roche", people.list, fixed=T)
people.list = gsub("Rafa Contrera,", "Rafa Contreras,", people.list, fixed=T)
people.list = gsub("Allly", "Ally", people.list, fixed=T)
people.list = gsub("Carstenen", "Carstensen", people.list, fixed=T)
people.list = gsub("Jill,", "Jill Sweet,", people.list, fixed=T)
people.list = gsub(",Crowell,", ",Wendy Crowell,", people.list, fixed=T)
people.list = gsub(",Kailey,", ",Kailey Kreatz,", people.list, fixed=T)
people.list = gsub("LW,", "Lucas Wandrie,", people.list, fixed=T)
people.list = gsub(",Drake", ",Christa Drake", people.list, fixed=T)
people.list = gsub(",Einck", ",Alan Einck", people.list, fixed=T)
people.list = gsub(",Hennen", ",Matt Hennen", people.list, fixed=T)
people.list = gsub(",JN,", ",Joe Norman,", people.list, fixed=T)
people.list = gsub(",Mottl", ",E Mottl", people.list, fixed=T)
people.list = gsub(",Proulx", ",Nick Proulx", people.list, fixed=T)
people.list = gsub(",Sean,", ",Sean Sissler,", people.list, fixed=T)
people.list = gsub(",Travis", ",Mitch Travis", people.list, fixed=T)
people.list = gsub(",Uhler", ",Kyle Uhler", people.list, fixed=T)
people.list = gsub("Yvetter,", "Yvette Christianson,", people.list, fixed=T)
people.list = gsub("Eric,", ",Eric Fieldseth,", people.list, fixed=T)
people.list = gsub(",Adam,", ",Adam Doll,", people.list, fixed=T)
people.list = gsub(",Eisterhold", ",Joe Eisterhold", people.list, fixed=T)
people.list = gsub(",Haworth,", ",Matt Horwath,", people.list, fixed=T)
people.list = gsub("Matt,", "Matt Hennen,", people.list, fixed=T)
people.list = gsub("Mike Veerhoven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("MVerhoeven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("Scott M,", "Scott Mackenthun,", people.list, fixed=T)
people.list = gsub("AR,", "Adam Rollins,", people.list, fixed=T)
people.list = gsub("CP,", "Christine Powell,", people.list, fixed=T)
people.list = gsub("M Bockman,", "M Bokman,", people.list, fixed=T)
people.list = gsub("Jill Bjorklund,", "Jill Sweet,", people.list, fixed=T)}

#Next, we get rid of some odd substrings that have popped up, mostly in Mike's original data, here and there that look bad.
{people.list = gsub("LLC", "", people.list, fixed = T)
people.list = gsub(" (first part of day)", "", people.list, fixed = T)
people.list = gsub(" (UMN)", "", people.list, fixed = T)
people.list = gsub("(7/16 only)", "", people.list, fixed = T)
people.list = gsub("/", ",", people.list, fixed=T)
people.list = gsub(" (Lincoln County)", "", people.list, fixed = T)
people.list = gsub("(Scott Co)", "", people.list, fixed = T)}

#Create a vector now out of the string, chopping at every single comma.
people.list2 = unlist(str_split(people.list, ",", simplify = FALSE))

#Clear up some lingering random issues not already cleared up by the above.
{people.list2 = people.list2[people.list2 != ""]
  people.list2 = people.list2[people.list2 != "E"]
  people.list2[people.list2 == "Jill"] = "Jill Sweet"
  people.list2[people.list2 == "Kailey"] = "Kailey Kreatz"
  people.list2[people.list2 == "Ramsey Co conservation district"] = "Unnamed hardworking surveyor(s)" }

#Take all our first-name-only entries and replace with our "anonymous" string since we have no clue who these folks are by their partial names alone.
people.list2[people.list2 %in% c("Marcie", "Wenck", "Alex", "Andy", "Tom", "Chase", "Mulu", "Jake", "Bri", "Chris", "Marissa","Weston", "Johanna", "DA", "RP", "Crowell")] = "Unnamed hardworking surveyor(s)" 

#Count up everybody by number of appearances and format into a dataframe. 
surveyors.leaderboard.df = data.frame(table(people.list2)) %>% 
  arrange(desc(Freq))

#Name the resulting columns something nice.
names(surveyors.leaderboard.df) = c("Surveyor", "Number of surveys")


# Finalizing the submitter's leaderboard ----------------------------------
#The leaderboard for submitters isn't actually technically built yet by this point, so we'll build it here.
submitter_leaderboard = {
  tmp1 = toString(paste0(lakes.summary.definitive$datasource, collapse=",")) #Get a single string of every submitter, delimitted by commas.
  tmp2 = str_split_1(tmp1, ",") #Break along those commas into a vector
  tmp3 = table(tmp2) #Use table to count up how often each appears.
  tmp4 = data.frame(tmp3) #Make that table into a data frame
  tmp5 = tmp4 %>%  #Arrange in descending order and rename the columns.
    arrange(desc(Freq)) %>% 
    rename(Submitter = tmp2, `Number of submissions` = Freq)
  tmp5 = tmp5[tmp5$Submitter != "LLC",] #Remove any errant entries that may still have squeaked through.
}


# Writing out final files for leaderboard and browse tabs -----------------

#Write all the goodies to file.
{saveRDS(surveyors.leaderboard.df, "inputs/MadeUpstream/surveyors_leaderboard_df")
  saveRDS(lakes.summary.definitive, "inputs/MadeUpstream/lakes_summary_definitive")
  saveRDS(db_allDOWs, "inputs/MadeUpstream/db_allDOWs")
  saveRDS(db_allCounties, "inputs/MadeUpstream/db_allCounties")
  saveRDS(db_allSearchableTaxa, "inputs/MadeUpstream/db_allSearchableTaxa")
  saveRDS(lakes.summary.sf, "inputs/MadeUpstream/lakes_summary_sf")
  saveRDS(submitter_leaderboard, "inputs/MadeUpstream/submitter_leaderboard")
}
