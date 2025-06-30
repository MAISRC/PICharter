#PURPOSE: WE WILL OBSCURE RECORDS FROM ANY LAKES ON TRIBAL LANDS BY DEFAULT. THUS, WE NEED A VECTOR OF THE DOWS OF ALL SUCH LAKES.

#LOAD PACKAGES
library(tidyverse)
library(sf)

#PATH TO TRIBAL BOUNDARIES FILES
path = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\Tribal_Government_in_Minnesota.shp"

#READ AND INSPECT
tribes = st_read(path)
plot(tribes$geometry)

#PATH TO MN DNR HYDROGRAPHY OF ALL LAKES
path2 = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\dnr_hydro_features_all.shp"

#READ, TRANSFORM TO MATCHING CRS.
LAKE_POLY = st_read(path2) %>% filter(!is.na(dowlknum))
LAKE_POLY = st_transform(LAKE_POLY, crs = st_crs(tribes))

#FIND ALL INTERSECTIONS (OVERLAP)
tribal_lakes = which(lengths(st_intersects(LAKE_POLY, tribes)) > 0)
#Exploration--what tribes have boundaries that intersect with our lakes?
# ourlakes = lakes.summary.sf$DOW
# LAKE_POLY_OURS = LAKE_POLY[LAKE_POLY$dowlknum %in% ourlakes,]
# tribes_wlakes = which(lengths(st_intersects(tribes, LAKE_POLY_OURS)) > 0)
# tribes2contact = tribes[tribes_wlakes,] %>% st_drop_geometry() %>% select(TRIBAL_NAM, LEGAL_STAT, TRIBAL_GOV)
# tribes_wlakes_l = lengths(st_intersects(tribes, LAKE_POLY_OURS))
# tribes2contact$numlakes = tribes_wlakes_l[tribes_wlakes_l != 0]

#EXTRACT AND PLOT THE INTERSECTIONS
tribal_lakes = LAKE_POLY[tribal_lakes,]
plot(tribal_lakes$geometry) #SEEMS PLAUSIBLE

#SAVE THE RESULTING DOW LIST.
tribal_DOWs = sort(tribal_lakes$dowlknum)
saveRDS(tribal_DOWs, "upstream/UpstreamInputs/tribal_DOWs.rds")
