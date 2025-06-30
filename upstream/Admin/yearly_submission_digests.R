compilation = db_path %>% 
  open_dataset() %>% 
  collect()

compilation = compilation %>% 
  select(SUBMITTER_NAME, DOW, lake_name, SURVEY_START, SURVEYORS, latitude, longitude)

compilation_blah = compilation %>% 
  group_by(DOW, SURVEY_START) %>% 
  summarize(SUBMITTER_NAME = first(SUBMITTER_NAME), 
            lake_name = first(lake_name),
            SURVEYORS = first(SURVEYORS),
            latitude = first(as.character(latitude)),
            longitude = first(as.character(longitude))
)

compilation2 = compilation_blah %>% 
  filter(!SUBMITTER_NAME %in% c(
  "DNR Fisheries", "DNR Shallow Lakes", "DNR Lakes and Rivers", "Mary Schaffer (Long Lake)", "John Manske", "Comfort Lake - Forest Lake Watershed District", "Beebe Lake Improvement Association", "DNR Invasive Species Program" 
))

compilation2 = compilation2 %>% 
  mutate(spatial = ifelse(!is.na(latitude) & !is.na(longitude), "Yes", "No")) %>% 
  select(-latitude, -longitude)

unique(compilation2$SUBMITTER_NAME)

newman = compilation2 %>% 
  filter(grepl("Newman", SUBMITTER_NAME))

write.csv(newman, "PICharter_NewmanLab.csv", row.names = FALSE)

rcwd = compilation2 %>% 
  filter(grepl("Rice", SUBMITTER_NAME))

write.csv(rcwd, "PICharter_RCWD.csv", row.names = FALSE)

bws = compilation2 %>% 
  filter(grepl("Blue Water", SUBMITTER_NAME))

write.csv(bws, "PICharter_BWS.csv", row.names = FALSE)

vlawmo = compilation2 %>% 
  filter(grepl("Vadnais", SUBMITTER_NAME))

write.csv(vlawmo, "PICharter_vlawmo.csv", row.names = FALSE)

fss = compilation2 %>% 
  filter(grepl("Freshwater", SUBMITTER_NAME))

write.csv(fss, "PICharter_FSS.csv", row.names = FALSE)

beltrami = compilation2 %>% 
  filter(grepl("Beltrami", SUBMITTER_NAME))

write.csv(beltrami, "PICharter_beltrami.csv", row.names = FALSE)


carver = compilation2 %>% 
  filter(grepl("Carver", SUBMITTER_NAME))

write.csv(carver, "PICharter_carver.csv", row.names = FALSE)


mcwd = compilation2 %>% 
  filter(grepl("Minnehaha", SUBMITTER_NAME))

write.csv(mcwd, "PICharter_MCWD.csv", row.names = FALSE)

trpd = compilation2 %>% 
  filter(grepl("Three Rivers", SUBMITTER_NAME))

write.csv(trpd, "PICharter_TRPD.csv", row.names = FALSE)


larkin = compilation2 %>% 
  filter(grepl("Larkin", SUBMITTER_NAME))

write.csv(larkin, "PICharter_larkin.csv", row.names = FALSE)


eor = compilation2 %>% 
  filter(grepl("Emmons", SUBMITTER_NAME))

write.csv(eor, "PICharter_eor.csv", row.names = FALSE)


ers = compilation2 %>% 
  filter(grepl("Endangered", SUBMITTER_NAME))

write.csv(ers, "PICharter_ers.csv", row.names = FALSE)

douglas = compilation2 %>% 
  filter(grepl("Douglas", SUBMITTER_NAME))

write.csv(douglas, "PICharter_douglas.csv", row.names = FALSE)


limnopro = compilation2 %>% 
  filter(grepl("Limnopro", SUBMITTER_NAME))

write.csv(limnopro, "PICharter_limnopro.csv", row.names = FALSE)

mprb = compilation2 %>% 
  filter(grepl("Minneapolis Park", SUBMITTER_NAME))

write.csv(mprb, "PICharter_mprb.csv", row.names = FALSE)


edge = compilation2 %>% 
  filter(grepl("Edge ", SUBMITTER_NAME))

write_csv(x = edge, file = "PICharter_edge.csv")


rmb = compilation2 %>% 
  filter(grepl("RMB ", SUBMITTER_NAME))

write_csv(x = rmb, file = "PICharter_rmb.csv")


asp = compilation2 %>% 
  filter(grepl("Aquatic Survey", SUBMITTER_NAME))

write_csv(x = asp, file = "PICharter_asp.csv")


lakeco = compilation2 %>% 
  filter(grepl("Lake County", SUBMITTER_NAME))

write_csv(x = lakeco, file = "PICharter_lakeco.csv")


ramseyco = compilation2 %>% 
  filter(SUBMITTER_NAME == "Ramsey County")

write_csv(x = ramseyco, file = "PICharter_ramseyco.csv")

rwmwd = compilation2 %>% 
  filter(grepl("Ramsey-Washington", SUBMITTER_NAME))

write_csv(x = rwmwd, file = "PICharter_rwmwd.csv")

ramseypr = compilation2 %>% 
  filter(grepl("Ramsey County Parks and Recreation", SUBMITTER_NAME))

write_csv(x = ramseypr, file = "PICharter_ramseypr.csv")


crwd = compilation2 %>% 
  filter(grepl("Capitol Region", SUBMITTER_NAME))

write_csv(x = crwd, file = "PICharter_crwd.csv")


acs = compilation2 %>% 
  filter(grepl("AIS Consulting", SUBMITTER_NAME))

write_csv(x = acs, file = "PICharter_acs.csv")
