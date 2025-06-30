blah = newfieldnames %>% distinct(newfieldname)
blah2 = blah %>% 
  filter(!grepl("Ambiguous", newfieldname)) %>% 
  filter(!newfieldname %in% c("depth", "depth.1", "latitude", "longitude", "STA_NBR", "sta_nbr", "substrate", "whole_rake_density", "x", "y", "Plant height m", "DATASOURCE", "DOWLKNUM", "LAKE_NAME", "no_veg_found", "not_sampled", "SURVEY_DATE", "SURVEY_ID", "unk_sp", "X", "Y", "unk_sp ", "SURVEY_NOTES", "SURVEYOR", "EASTING", "Amibugous N guadalupensis or N gracillima please comment", "NORTHING", "SAMPLE_NOTES", "sample_taken", "Added_point", "SUBSTRATE", "SURFACE_GROWTH", "sample_type", "plant_height", "percent_biovolume"))

blah3 = data.frame(newfieldname = apply(blah2, 1, tidyName))

plant_lookup = read.csv("inputs/Static/plant_name_lookup.csv")
plant_lookup$ScientificName = tidyName(plant_lookup$ScientificName)
plant_lookup$CommonName = firstup(plant_lookup$CommonName)

blah4 = left_join(blah3, plant_lookup, by = c("newfieldname" = "ScientificName"))

tofix = blah4 %>% 
  filter(is.na(CommonName))

write.csv(tofix, "tofix.csv")

blah5 = read.csv("tofix2.csv")

blah6 = rbind(blah5, blah4) %>% 
  filter(!is.na(CommonName))

write.csv(blah6[,1:2], "commonsci_name_lookup.csv")

blah7 = read.csv("inputs/Static/commonsci_name_lookup.csv")
blah8 = left_join(blah7, plant_lookup, by = c("newfieldname" = "ScientificName"))
