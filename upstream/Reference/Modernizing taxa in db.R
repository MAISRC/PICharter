#MICA KROMREY AT WI DNR GRACIOUSLY REVIEWED OUR TAXONOMIC DISTINCTIONS IN OUR DB FILE AND FOUND SEVERAL ISSUES THAT I WILL CORRECT IN THIS FILE. ADVANTAGEOUSLY, NOT ONLY WILL THESE CHANGES MAKE OUR DATA MORE INTEROPERABLE, BUT THEY WILL ALSO REMOVE SEVERAL COLUMNS PERMANENTLY FROM THE DB (MAKING IT SMALLER). IT WILL ALSO FIX SEVERAL WEIRD EDGE CASES AND WORKAROUNDS THAT ARE CURRENTLY NECESSARY TO PRESENT AND HANDLE THE DATA. THIS WORK IS BEING DONE MAY 17TH 2024, AND A SPECIAL BACKUP OF THE DB WILL BE MADE FIRST.

current_dbname = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet" #CURRENT FILE'S NAME
current_db = read_parquet(current_dbname) #READ

#CONVENIENCE FUNCTION FOR DETERMINING IF CELLS ARE EITHER NA OR 0 OR BLANK
is_empty = function(x) {
  x == "" | is.na(x)
}
is_empty_or0 = function(x) {
  x == "" | is.na(x) | x == 0
}

which2move = function(cells2move, cellsReceiving) {
  
  moveSafely = which(is_empty(cellsReceiving) & !is_empty(cells2move)) #The thing being moved is not empty (but might be 0) and the receiving cell is empty.
  
  moveCarefully = which(!is_empty_or0(cells2move) & !is_empty(cellsReceiving))  #Be careful if the thing being moved is something and the receiving cell is something or 0. 
  
  probablyDontMove = which(cells2move == 0 & !is_empty_or0(cellsReceiving)) #Kind of a weird edge case where the 0 being moved is *probably* meaningless but maybe we don't know that for sure.
  
  dontMove = which(is_empty(cells2move) | #Don't bother if the cell to move is empty.
             (cells2move == 0 & cellsReceiving == 0)) #Or if what is being moved is a 0 and so too is the receiving cell. 
  
  return(list(moveSafely = moveSafely, 
              moveCarefully = moveCarefully,
              probablyDontMove = probablyDontMove,
              dontMove = dontMove))
  
}


##MAJOR DECISION: ARE 0S THE SAME AS NAS, FUNCTIONALLY? DO I NEED TO MOVE 0S?
#CHOICE: I THINK SO. IN OTHER CONTEXTS, YES, I TREAT NAS AS 0S. GRANTED, SOME FOLKS USE 0S TO MARK VISUAL OBSERVATIONS BUT A) THAT'S NOT A GOOD PRACTICE AND B) THOSE VISUAL DATA ARE "OPTIONAL" AT BEST. SO, I DON'T FEEL A STRONG NEED TO "PRESERVE 0S." THAT SAID, MAYBE IT WOULDN'T BE ALL THAT HARD TO PRESERVE 0S...HOW WOULD WE DO THAT? IF MOVING A NON-NA VALUE, THEY CAN ONLY REPLACE NON-NA VALUES, UNLESS WHAT IS BEING MOVED AND REPLACED ARE BOTH 0S, IN WHICH CASE WE COULD SKIP THOSE. SO, TO RECAP, WE NEVER MOVE NAS, WE SKIP MOVING 0S TO SPOTS WITH 0S ALREADY, BUT WE DO MOVE 0S INTO NAS, AND WE ONLY MOVE SOMETHINGS INTO SPOTS THAT AREN'T HOLDING 0S OR SOMETHINGS ALREADY.  

###FILAMENTOUS ALGAE

algae_moves = which2move(current_db$filamentous_algae, current_db$algae) #EVAL MOVEMENT
lapply(algae_moves, length) #All can be moved safely. 

#MOVE THE DATA
current_db$algae[algae_moves$moveSafely] = current_db$filamentous_algae[algae_moves$moveSafely]

#NOTHING LEFT, DELETE OLD COLUMN
current_db$filamentous_algae[algae_moves$moveSafely] = NA
current_db$filamentous_algae = NULL

algae_moves2 = which2move(current_db$filamentous_algae.1, current_db$algae) #EVAL MOVEMENT
lapply(algae_moves2, length) #33 can be moved safely, but 2 must be moved carefully.

#MOVE THE SAFE DATA
current_db$algae[algae_moves2$moveSafely] = current_db$filamentous_algae.1[algae_moves2$moveSafely]

#CONFIRM WITH WIPE
#current_db$filamentous_algae.1[algae_moves2$moveSafely] = NA

#CHECK TO CONFIRM THAT THESE CAN MOVE SAFELY TO THE SECONDARY ALGAE COLUMN
algae_moves3 = which2move(current_db$filamentous_algae.1, current_db$algae.1) #EVAL MOVEMENT
lapply(algae_moves3, length) #Both can

#MOVE THE SAFE DATA
current_db$algae.1[algae_moves3$moveSafely] = current_db$filamentous_algae.1[algae_moves3$moveSafely]

#CONFIRM W/ WIPE
#current_db$filamentous_algae.1[algae_moves3$moveSafely] = NA

#NOTHING LEFT, DELETE OLD COLUMN
current_db$filamentous_algae.1 = NULL


###A. GLAUCOPHYLLA
andgla_moves = which2move(current_db$andromeda_glaucophylla, current_db$andromeda_polifolia) #EVAL MOVEMENT
lapply(andgla_moves, length) #4 could move, and 2 of them actually need to move carefully, so we might as well move all 4 because a secondary column is needed anyway.

current_db$andromeda_polifolia.1 = NA #Add this column

#MOVE THE DATA
current_db$andromeda_polifolia.1[c(andgla_moves$moveCarefully, andgla_moves$probablyDontMove)] =
  current_db$andromeda_glaucophylla[c(andgla_moves$moveCarefully, andgla_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$andromeda_glaucophylla[c(andgla_moves$moveCarefully, andgla_moves$probablyDontMove)] = NA
current_db$andromeda_glaucophylla = NULL


###E SMALLII
Esma_moves = which2move(current_db$eleocharis_smallii, current_db$eleocharis_palustris) #EVAL MOVEMENT
lapply(Esma_moves, length) #63 can move carefully, another 91 might as well move. So another .1 column is needed.

current_db$eleocharis_palustris.1 = NA #Add this column

#MOVE
current_db$eleocharis_palustris.1[c(Esma_moves$moveCarefully, Esma_moves$probablyDontMove)] =
  current_db$eleocharis_smallii[c(Esma_moves$moveCarefully, Esma_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$eleocharis_smallii[c(Esma_moves$moveCarefully, Esma_moves$probablyDontMove)] = NA
current_db$eleocharis_smallii = NULL


#WEIRD DUPE COLUMN
Esma_moves2 = which2move(current_db$eleocharis_palustris_, current_db$eleocharis_palustris) #EVAL MOVEMENT
lapply(Esma_moves2, length) #Can move safely.

#MOVE
current_db$eleocharis_palustris[Esma_moves2$moveSafely] =
  current_db$eleocharis_palustris_[Esma_moves2$moveSafely]

#WIPE OUT OLD
#current_db$eleocharis_palustris_[Esma_moves2$moveSafely] = NA
current_db$eleocharis_palustris_ = NULL


###I TENELLA
Iten_moves = which2move(current_db$isoetes_tenella, current_db$isoetes_echinospora) #EVAL MOVEMENT
lapply(Iten_moves, length) #All can move safely

#MOVE
current_db$isoetes_echinospora[Iten_moves$moveSafely] =
  current_db$isoetes_tenella[Iten_moves$moveSafely]

#WIPE OUT OLD
#current_db$isoetes_tenella[Iten_moves$moveSafely] = NA
current_db$isoetes_tenella = NULL


###M EXALBESCENS
Mexa_moves = which2move(current_db$myriophyllum_exalbescens, current_db$myriophyllum_sibiricum) #EVAL MOVEMENT
lapply(Mexa_moves, length) #21 can move safely, and 16 can move carefully. Another 27K could also move with the carefullies.

current_db$myriophyllum_sibiricum.1 = NA #Add this column

#MOVE SAFELIES
current_db$myriophyllum_sibiricum[Mexa_moves$moveSafely] =
  current_db$myriophyllum_exalbescens[Mexa_moves$moveSafely]

#MOVE CAREFULLIES
current_db$myriophyllum_sibiricum.1[c(Mexa_moves$moveCarefully, Mexa_moves$probablyDontMove)] =
  current_db$myriophyllum_exalbescens[c(Mexa_moves$moveCarefully, Mexa_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$myriophyllum_exalbescens[Mexa_moves$moveSafely] = NA
#current_db$myriophyllum_exalbescens[c(Mexa_moves$moveCarefully, Mexa_moves$probablyDontMove)] = NA
current_db$myriophyllum_exalbescens = NULL


###N TUBEROSA
Ntub_moves = which2move(current_db$nymphaea_tuberosa, current_db$nymphaea_odorata) #EVAL MOVEMENT
lapply(Ntub_moves, length) #4 Can move carefully, and another 18K might as well move if they can.
Ntub_moves2 = which2move(current_db$nymphaea_tuberosa, current_db$nymphaea_odorata.1) #EVAL MOVEMENT
lapply(Ntub_moves2, length) #All can safely move this way.

#MOVE
current_db$nymphaea_odorata.1[Ntub_moves2$moveSafely] =
  current_db$nymphaea_tuberosa[Ntub_moves2$moveSafely]

#WIPE OUT OLD
#current_db$nymphaea_tuberosa[Ntub_moves2$moveSafely] = NA
current_db$nymphaea_tuberosa = NULL


###P AMPHIBIUM
Pamp_moves = which2move(current_db$polygonum_amphibium.1, current_db$persicaria_amphibia) #EVAL MOVEMENT
lapply(Pamp_moves, length) #2 Can shift safely and then this extra column can go.

current_db$persicaria_amphibia[Pamp_moves$moveSafely] =
  current_db$polygonum_amphibium.1[Pamp_moves$moveSafely]

#WIPE OUT OLD
#current_db$polygonum_amphibium.1[Pamp_moves$moveSafely] = NA
current_db$polygonum_amphibium.1 = NULL


Pamp_moves2 = which2move(current_db$polygonum_amphibium, current_db$persicaria_amphibia) #EVAL MOVEMENT
lapply(Pamp_moves2, length) #Many can move safely, but not all.

#MOVE SAFELIES
current_db$persicaria_amphibia[Pamp_moves2$moveSafely] =
  current_db$polygonum_amphibium[Pamp_moves2$moveSafely]

#WIPE OUT OLD
#current_db$polygonum_amphibium[Pamp_moves2$moveSafely] = NA

current_db$persicaria_amphibia.1 = NA #CREATE THIS COLUMN

#MOVE THESE CAREFULLIES
current_db$persicaria_amphibia.1[c(Pamp_moves2$moveCarefully, Pamp_moves2$probablyDontMove)] =
  current_db$polygonum_amphibium[c(Pamp_moves2$moveCarefully, Pamp_moves2$probablyDontMove)]

#WIPE OUT OLD
#current_db$polygonum_amphibium[c(Pamp_moves2$moveCarefully, Pamp_moves2$probablyDontMove)] = NA
current_db$polygonum_amphibium = NULL


###P BIC
Pbic_moves = which2move(current_db$potamogeton_bicapulatus, current_db$potamogeton_bicupulatus) #EVAL MOVEMENT
lapply(Pbic_moves, length) #All can move safely.

#MOVE
current_db$potamogeton_bicupulatus[Pbic_moves$moveSafely] =
  current_db$potamogeton_bicapulatus[Pbic_moves$moveSafely]

#WIPE OUT OLD
#current_db$potamogeton_bicapulatus[Pbic_moves$moveSafely] = NA
current_db$potamogeton_bicapulatus = NULL


###P SP
Pspb_moves = which2move(current_db$potamogeton_sp_broad, current_db$potamogeton_sp) #EVAL MOVEMENT
lapply(Pspb_moves, length) #Only a couple can move safely...let's start there.

#MOVE
current_db$potamogeton_sp[Pspb_moves$moveSafely] =
  current_db$potamogeton_sp_broad[Pspb_moves$moveSafely]

#WIPE OLD
#current_db$potamogeton_sp_broad[Pspb_moves$moveSafely] = NA

#THE REST HAS TO GO TO AN OVERFLOW COL
current_db$potamogeton_sp.1 = NA #MAKE THIS COL

#MOVE THESE CAREFULLIES
current_db$potamogeton_sp.1[c(Pspb_moves$moveCarefully, Pspb_moves$probablyDontMove)] =
  current_db$potamogeton_sp_broad[c(Pspb_moves$moveCarefully, Pspb_moves$probablyDontMove)]

#WIPE
#current_db$potamogeton_sp_broad[c(Pspb_moves$moveCarefully, Pspb_moves$probablyDontMove)] = NA
current_db$potamogeton_sp_broad = NULL


Pspb_moves2 = which2move(current_db$potamogeton_sp_narrow.1, current_db$potamogeton_sp_narrow) #EVAL MOVEMENT
lapply(Pspb_moves2, length) #Some can be moved safely here...

#MOVE THE SAFELIES
current_db$potamogeton_sp_narrow[Pspb_moves2$moveSafely] =
  current_db$potamogeton_sp_narrow.1[Pspb_moves2$moveSafely]

#WIPE
#current_db$potamogeton_sp_narrow.1[Pspb_moves2$moveSafely] = NA

Pspb_moves3 = which2move(current_db$potamogeton_sp_narrow.1, current_db$potamogeton_sp) #EVAL MOVEMENT
lapply(Pspb_moves3, length) #All can be moved safely here...

#MOVE THE SAFELIES
current_db$potamogeton_sp[Pspb_moves3$moveSafely] =
  current_db$potamogeton_sp_narrow.1[Pspb_moves3$moveSafely]

#WIPE
#current_db$potamogeton_sp_narrow.1[Pspb_moves3$moveSafely] = NA
current_db$potamogeton_sp_narrow.1 = NULL

Pspb_moves4 = which2move(current_db$potamogeton_sp_narrow, current_db$potamogeton_sp) #EVAL MOVEMENT
lapply(Pspb_moves4, length) #Some can be moved safely here...

#MOVE THE SAFELIES
current_db$potamogeton_sp[Pspb_moves4$moveSafely] =
  current_db$potamogeton_sp_narrow[Pspb_moves4$moveSafely]

#WIPE
#current_db$potamogeton_sp_narrow[Pspb_moves4$moveSafely] = NA

Pspb_moves5 = which2move(current_db$potamogeton_sp_narrow, current_db$potamogeton_sp.1) #EVAL MOVEMENT
lapply(Pspb_moves5, length) #Only a handful of 0s will be lost here (61). I think that's ok.

#MOVE THE SAFELIES
current_db$potamogeton_sp.1[Pspb_moves5$moveSafely] =
  current_db$potamogeton_sp_narrow[Pspb_moves5$moveSafely]

#WIPE
#current_db$potamogeton_sp_narrow[Pspb_moves5$moveSafely] = NA
current_db$potamogeton_sp_narrow = NULL


###P PAL
Ppal_moves = which2move(current_db$potentilla_palustris, current_db$comarum_palustre) #EVAL MOVEMENT
lapply(Ppal_moves, length) #All can move safely.

#MOVE
current_db$comarum_palustre[Ppal_moves$moveSafely] =
  current_db$potentilla_palustris[Ppal_moves$moveSafely]

#WIPE OUT OLD
#current_db$potentilla_palustris[Ppal_moves$moveSafely] = NA
current_db$potentilla_palustris = NULL


###R LONGIROSTRIS
Rlon_moves = which2move(current_db$ranunculus_longirostris, current_db$ranunculus_aquatilis) #EVAL MOVEMENT
lapply(Rlon_moves, length) #A whole mix here, so a secondary column will be needed.

current_db$ranunculus_aquatilis.1 = NA #MAKE IT

#MOVE SAFELIES
current_db$ranunculus_aquatilis[Rlon_moves$moveSafely] =
  current_db$ranunculus_longirostris[Rlon_moves$moveSafely]

#MOVE CAREFULLIES
current_db$ranunculus_aquatilis.1[c(Rlon_moves$moveCarefully, Rlon_moves$probablyDontMove)] =
  current_db$ranunculus_longirostris[c(Rlon_moves$moveCarefully, Rlon_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$ranunculus_longirostris[Rlon_moves$moveSafely] = NA
#current_db$ranunculus_longirostris[c(Rlon_moves$moveCarefully, Rlon_moves$probablyDontMove)] = NA
current_db$ranunculus_longirostris = NULL


###S AMERICANUS
Same_moves = which2move(current_db$schoenoplectus_americanus, current_db$schoenoplectus_sp) #EVAL MOVEMENT
lapply(Same_moves, length) #2 need to move carefully, so we need the overflow column.

current_db$schoenoplectus_sp.1 = NA #MAKE IT

#MOVE
current_db$schoenoplectus_sp.1[c(Same_moves$moveCarefully, Same_moves$probablyDontMove)] =
  current_db$schoenoplectus_americanus[c(Same_moves$moveCarefully, Same_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$schoenoplectus_americanus[c(Same_moves$moveCarefully, Same_moves$probablyDontMove)] = NA
current_db$schoenoplectus_americanus = NULL


###S VALIDUS
Sval_moves = which2move(current_db$scirpus_validus, current_db$schoenoplectus_tabernaemontani) #EVAL MOVEMENT
lapply(Sval_moves, length) #17 need to move carefully, so we need the overflow column.

current_db$schoenoplectus_tabernaemontani.1 = NA #MAKE IT

#MOVE
current_db$schoenoplectus_tabernaemontani.1[c(Sval_moves$moveCarefully, Sval_moves$probablyDontMove)] =
  current_db$scirpus_validus[c(Sval_moves$moveCarefully, Sval_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$scirpus_validus[c(Sval_moves$moveCarefully, Sval_moves$probablyDontMove)] = NA
current_db$scirpus_validus = NULL


###S SPECIES
SspE_moves = which2move(current_db$sparganium_sp_emergent, current_db$sparganium_sp) #EVAL MOVEMENT
lapply(SspE_moves, length) #Some need careful movement but none safe, so overflow needed.

current_db$sparganium_sp.1 = NA #MAKE IT

#MOVE
current_db$sparganium_sp.1[c(SspE_moves$moveCarefully, SspE_moves$probablyDontMove)] =
  current_db$sparganium_sp_emergent[c(SspE_moves$moveCarefully, SspE_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$sparganium_sp_emergent[c(SspE_moves$moveCarefully, SspE_moves$probablyDontMove)] = NA
current_db$sparganium_sp_emergent = NULL

SspE_moves2 = which2move(current_db$sparganium_sp_floating, current_db$sparganium_sp) #EVAL MOVEMENT
lapply(SspE_moves2, length) #None can safely go here.
SspE_moves3 = which2move(current_db$sparganium_sp_floating, current_db$sparganium_sp.1) #EVAL MOVEMENT
lapply(SspE_moves3, length) #Some can move safely, but some carefully still, so a second overflow is needed.

current_db$sparganium_sp.2 = NA #MAKE IT

#MOVE SAFELIES
current_db$sparganium_sp.1[SspE_moves3$moveSafely] =
  current_db$sparganium_sp_floating[SspE_moves3$moveSafely]

#MOVE CAREFULLIES
current_db$sparganium_sp.2[c(SspE_moves3$moveCarefully, SspE_moves3$probablyDontMove)] =
  current_db$sparganium_sp_floating[c(SspE_moves3$moveCarefully, SspE_moves3$probablyDontMove)]

#WIPE
#current_db$sparganium_sp_floating[SspE_moves3$moveSafely] = NA
#current_db$sparganium_sp_floating[c(SspE_moves3$moveCarefully, SspE_moves3$probablyDontMove)] = NA
current_db$sparganium_sp_floating = NULL


###U MACRORHIZA
Umac_moves = which2move(current_db$utricularia_macrorhiza, current_db$utricularia_vulgaris) #EVAL MOVEMENT
lapply(Umac_moves, length) #A whole mix again...

current_db$utricularia_vulgaris.1 = NA #MAKE IT

#MOVE SAFELIES
current_db$utricularia_vulgaris[Umac_moves$moveSafely] =
  current_db$utricularia_macrorhiza[Umac_moves$moveSafely]

#MOVE CAREFULLIES
current_db$utricularia_vulgaris.1[c(Umac_moves$moveCarefully, Umac_moves$probablyDontMove)] =
  current_db$utricularia_macrorhiza[c(Umac_moves$moveCarefully, Umac_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$utricularia_macrorhiza[Umac_moves$moveSafely] = NA
#current_db$utricularia_macrorhiza[c(Umac_moves$moveCarefully, Umac_moves$probablyDontMove)] = NA
current_db$utricularia_macrorhiza = NULL


###Scirpoides
Ssp_moves = which2move(current_db$scirpoides_sp, current_db$unk_sp) #EVAL MOVEMENT
lapply(Ssp_moves, length) #The one can move safely.

#MOVE
current_db$unk_sp[Ssp_moves$moveSafely] =
  current_db$scirpoides_sp[Ssp_moves$moveSafely]

#WIPE OUT OLD
#current_db$scirpoides_sp[Ssp_moves$moveSafely] = NA
current_db$scirpoides_sp = NULL


###Typha glauca
Tgla_moves = which2move(current_db$typha_glauca, current_db$typha_x_glauca) #EVAL MOVEMENT
lapply(Tgla_moves, length) #All can move safely.

#MOVE
current_db$typha_x_glauca[Tgla_moves$moveSafely] =
  current_db$typha_glauca[Tgla_moves$moveSafely]

#WIPE OUT OLD
#current_db$typha_glauca[Tgla_moves$moveSafely] = NA
current_db$typha_glauca = NULL

###S pol
Spol_moves = which2move(current_db$spirodela_polyrhiza.1, current_db$spirodela_polyrrhiza) #EVAL MOVEMENT
lapply(Spol_moves, length) #Some could move safely, others not so much.

current_db$spirodela_polyrrhiza.1 = NA #MAKE NEW COL

#MOVE SAFELIES
current_db$spirodela_polyrrhiza[Spol_moves$moveSafely] =
  current_db$spirodela_polyrhiza.1[Spol_moves$moveSafely]

#MOVE CAREFULLIES
current_db$spirodela_polyrrhiza.1[c(Spol_moves$moveCarefully, Spol_moves$probablyDontMove)] =
  current_db$spirodela_polyrhiza.1[c(Spol_moves$moveCarefully, Spol_moves$probablyDontMove)]

#WIPE OUT OLD
#current_db$spirodela_polyrhiza.1[Spol_moves$moveSafely] = NA
#current_db$spirodela_polyrhiza.1[c(Spol_moves$moveCarefully, Spol_moves$probablyDontMove)] = NA
current_db$spirodela_polyrhiza.1 = NULL

##RENAMES
current_db = current_db %>% 
  rename(azolla_microphylla = azolla_mexicana,
         aquatic_moss = drepanocladus_or_fontinalis_sp,
         eutrochium_dubium = eupatorium_dubium,
         eutrochium_maculatum = eupatorium_maculatum,
         rhododendron_groenlandicum = ledum_groenlandicum,
         spirodela_polyrrhiza = spirodela_polyrhiza,
         heteranthera_sp = zosterella_sp)


#12 COLUMNS WERE NET CUT OUT OF THIS PROCESS...NOT BAD! THAT'S THIS MANY CELLS...
484895*12 #5.8 million!

write_parquet(current_db, sink = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet") #WRITE