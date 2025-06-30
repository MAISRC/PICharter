#PRIOR TO MAY 2024, DEPTH DATA WERE ESSENTIALLY SPREAD ACROSS THREE COLUMNS (depth, depth.1, and DEPTH_UNITS). THIS FELT INEFFICIENT AND MADE THE DATA LESS USABLE DOWNSTREAM, ESP. IN RE: THE RECORDS TAB. 

#SO, THIS SCRIPT DOCUMENTS HOW THESE DATA WERE CONSOLIDATED.

#FIRST, I ASSUME THAT depth.1 ONLY CONTAINS DATA WHEN EITHER 1) 2 DEPTH COLUMNS WERE SUBMITTED, ONE IN FEET AND ANOTHER IN METERS, AND THIS COLUMN STORES WHICHEVER WAS SECOND IN THE SUBMITTED FILE OR 2) THE SUBMISSION INCLUDED A SECOND DEPTH COLUMN FOR SOME OTHER REASON, SUCH AS IN THE EVENT IT WAS A DUPLICATE OF THE FIRST. 

#SO, THE FIRST STEP IS TO ASSESS IF THESE ASSUMPTIONS HOLD. THE EASIEST TO CHECK IS TO SEE WHICH RECORDS HOLD FULLY DUPLICATE INFORMATION BETWEEN THE TWO COLUMNS. 

#WE'LL EMPLOY A SPLIT-OPERATE-COMBINE WORKFLOW HERE.

toKeep = current_db %>% 
  filter(is.na(depth.1))

#AT TIME OF WRITING, IT'S ONLY ABOUT 11K RECORDS W/ DATA HERE.
toModify = current_db %>% 
  filter(!is.na(depth.1))

#235 INSTANCES HAVE THE TWO COLUMNS IDENTICAL--THOSE CAN SURELY GO.
length(which(toModify$depth == toModify$depth.1))
toModify$depth.1[which(toModify$depth == toModify$depth.1)] = NA

#MOST (11K) EXPLICITLY CLAIM TO HAVE COLUMNS FOR BOTH UNITS, SO I SHOULD BE ABLE TO JUST DELETE THE LOWER SET OF THE TWO, UNDER THE ASSUMPTION THAT THOSE ARE THE METERS ONES.
length(which(toModify$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet"))

removeSecond = which(toModify$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet" &
        toModify$depth > toModify$depth.1) #IF DEPTH IS HIGHER THAN DEPTH.1, THEN FEET MUST BE IN DEPTH.

toModify$depth.1[removeSecond] = NA #REMOVE THESE.

removeFirst = which(toModify$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet" &
                       toModify$depth < toModify$depth.1) #IF DEPTH.1 IS HIGHER THAN DEPTH, THEN FEET MUST BE IN DEPTH.1

toModify$depth[removeFirst] = toModify$depth.1[removeFirst] #REMOVE THESE FIRST...
toModify$depth.1[removeFirst] = NA #THEN WIPE THESE

length(which(!is.na(toModify$depth.1))) #407 RECORDS REMAIN...WHAT'S THEIR DEAL?
toModify$DEPTH_UNITS[which(!is.na(toModify$depth.1))] #A MIX OF SOME FEET, SOME METERS...
View(toModify[which(!is.na(toModify$depth.1)), c("depth", "depth.1")]) #TO ME, IT LOOKS LIKE PART OF THE ISSUE IS THAT DEPTH IS A CHARACTER AND DEPTH.1 IS NUMERIC. A LOT OF DEPTH.1 VALUES ARE ALSO 0s, WHICH COULD BE SAFELY REMOVED.
which(as.integer(toModify$depth.1) == 0) #THESE ONES
toModify$depth.1[which(as.integer(toModify$depth.1) == 0)] = NA

length(which(!is.na(toModify$depth.1))) #DOWN TO JUST 128 NOW...THE COMPARISONS ARE JUST GETTING MESSED UP BY THE FIRST COL BEING A CHARACTER.

removeFirst2 = which(toModify$depth.1 > as.numeric(toModify$depth)) #58
removeSecond2 = which(toModify$depth.1 < as.numeric(toModify$depth)) #70, SO THIS IS THE REST OF THE PROBLEMATIC DATA

toModify$depth[removeFirst2] = toModify$depth.1[removeFirst2] #REMOVE THESE FIRST...
toModify$depth.1[removeFirst2] = NA #THEN WIPE THESE

toModify$depth.1[removeSecond2] = NA #NOW REMOVE THESE.
#ALL ENTRIES IN DEPTH.1 ARE NOW GONE. ALL REMAINING DATA IN toModify IS FEET DATA IN DEPTH

toModify$depth.1 = NULL #REMOVE THIS COLUMN
toModify$DEPTH_UNITS = NULL #AND THIS ONE
toModify = toModify %>% 
  rename(depth_ft = depth) #RENAME THIS COLUMN TO DEPTH_FT TO ACKNOWLEDGE THE UNITS


#NOW, WE PIVOT TO toKeep. HERE, THERE ARE NO DATA IN THE DEPTH.1 COLUMN ALREADY, SO WE CAN START BY NIXING THOSE.
toKeep$depth.1 = NULL

#NEXT, WE NEED TO KNOW WHICH UNITS WE HAVE IN PLAY HERE...
unique(toKeep$DEPTH_UNITS) #Feet, Meters, None, and (oddly) both. I BET NEWMAN LAB IS TO BLAME FOR THE BOTHS...

#WELL, WE CAN SAFELY IGNORE THE BOTHS, NONES, AND FEETS, AS THESE ARE ALL ALREADY AS GOOD AS THEY'RE GONNA BE. 
#INSTEAD, WE JUST NEED TO TAKE THE METERS ONES (THAT AREN'T TEXT) AND CONVERT THEM TO THE EQUIVALENT FEET. THE ONES WITH TEXT WILL BE AWKWARD BECAUSE "3.5X", e.g., WOULD NOT CONVERT WELL...WE'LL HAVE TO SEE HOW MANY OF THOSE THERE ARE.

length(which(toKeep$DEPTH_UNITS == "Meters")) #ABOUT 25K TOTAL RECORDS IN METERS
length(which(is.na(as.numeric(toKeep$depth[which(toKeep$DEPTH_UNITS == "Meters")])))) #LOOKS LIKE ABOUT 244 HAVE SOME TEXT IN THEM...WE'LL NEED TO IGNORE THESE ONES FOR THE MOMENT.

toKeep$depth[which(toKeep$DEPTH_UNITS == "Meters" &
             !is.na(as.numeric(toKeep$depth)))] = 
  round(as.numeric(toKeep$depth)[which(toKeep$DEPTH_UNITS == "Meters" &
                             !is.na(as.numeric(toKeep$depth)))] * 3.28084, 2) #MULTIPLY BY 3.28 AND THEN ROUND OFF TO CONVERT TO FEET FROM METERS.

toKeep$depth[which(toKeep$DEPTH_UNITS == "Meters" &
                     is.na(as.numeric(toKeep$depth)))] #AHA, MOST OF THESE ARE ACTUALLY JUST NA, BUT THERE IS ONE "3..3" IN HERE, WHICH I PRESUME SHOULD BE 3.3, AND THEN WE'LL BE ALL SET.

toKeep$depth[which(toKeep$DEPTH_UNITS == "Meters" & toKeep$depth == "3..3")] = round(3.3 * 3.28084, 2) #FIX THAT ONE ENTRY.

#NOW, WE SHOULD BRIEFLY EXPLORE WHAT THE DEAL IS WITH THE BOTHS AND NONES...IS THERE ANYTHING SUSPICIOUS THERE?
length(which(toKeep$DEPTH_UNITS == "None" & !is.na(toKeep$depth))) #THERE ARE 60 OF THESE WHERE UNITS ARE NONE BUT WE HAVE DATA OF SOME KIND...
toKeep$depth[which(toKeep$DEPTH_UNITS == "None" & !is.na(toKeep$depth))] #TO ME, THESE LOOK LIKELIER TO BE METERS THAN FEET, AS THE MAX IS 3 SOMETHING...
toKeep$DOW[which(toKeep$DEPTH_UNITS == "None" & !is.na(toKeep$depth))] #THIS IS COMO, W/ MAX DEPTH OF 15.5 FT, SO YES, THESE ARE MOST LIKELY FEET, BUT TO BE SURE...

toKeep$depth[which(toKeep$DOW == "62005500" & toKeep$DEPTH_UNITS == "Feet")] #YES, NUMBERS ARE GENERALLY HIGHER.

#OVERWRITE THOSE
toKeep$depth[which(toKeep$DEPTH_UNITS == "None" &
                     !is.na(toKeep$depth))] = round(as.numeric(toKeep$depth)[which(toKeep$DEPTH_UNITS == "None" &
                                                                                     !is.na(toKeep$depth))] * 3.28084, 2)

#NOW, WHAT ABOUT THESE BOTHS?
length(which(toKeep$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet" &
               !is.na(toKeep$depth))) #THERE ARE 4 OF THESE...WHICH IS A SUPER ODD NUMBER COME TO THINK OF IT.
toKeep$depth[which(toKeep$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet" &
                     !is.na(toKeep$depth))] #THESE ALSO LOOK LIKE METER DATA TO ME...
toKeep$DOW[which(toKeep$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet" &
                   !is.na(toKeep$depth))] #YEP ALSO COMO...

#OVERWRITE THOSE
toKeep$depth[which(toKeep$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet" &
                     !is.na(toKeep$depth))] = round(as.numeric(toKeep$depth)[which(toKeep$DEPTH_UNITS == "The data set has a column each for depths in meters and in feet" &
                                                                                     !is.na(toKeep$depth))] * 3.28084, 2)


#AT THIS POINT, THERE SHOULD BE NO FURTHER USEFUL INFO IN DEPTH_UNITS AND IT CAN BE CUT.
toKeep$DEPTH_UNITS = NULL

toKeep = toKeep %>% 
  rename(depth_ft = depth) #RENAME THIS COLUMN TO DEPTH_FT TO ACKNOWLEDGE THE NEW UNITS

combined_db = bind_rows(toKeep, toModify)

write_parquet(combined_db, sink = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet")
