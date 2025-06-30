results.df = data.frame(DOW = character(), 
                        SURVEY1 = character(), 
                        SURVEY2 = character(), 
                        SUBMITTER = character(),
                        overlapping = logical(),
                        same_length = logical(),
                        daydiff = numeric())

for(i in 1:length(unique(grow.dat$DOW))) {
  
  if(i %% 100 == 0) {print(i)}
  
  #CURR LAKE
  curr.lake = unique(grow.dat$DOW)[i]
  
  #Curr dat1
  curr.dat1 = grow.dat %>% 
    filter(DOW == curr.lake)
  
  #Remove specific cases we've ruled out.
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

results.df = results.df %>% arrange(SUBMITTER)
