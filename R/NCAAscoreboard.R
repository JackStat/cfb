#' @title Scrape NCAA scoreboard
#' 
#' @import dplyr foreach jsonlite
#' 
#' @example NCAAscoreboard(2016, 1)
#' 
#' @export

NCAAscoreboard <- function(year, week){

  Week = ifelse(nchar(week) ==1, paste0(0,week), week)
  
  url = paste0('http://data.ncaa.com//jsonp//scoreboard//football//fbs//', year, "//", Week, '//scoreboard.html')
    
    
  raw<-suppressWarnings(readLines(url))
  
  rawfromJSON <- fromJSON((gsub('callbackWrapper\\(','',gsub('\\}\\)\\;','\\}',raw))))
  
  
  foreach(k = 1:length(rawfromJSON$scoreboard$games), .combine = 'rbind') %do% {
    
  #   HomeStuff <- rawfromJSON$scoreboard$games[[k]]$home
  #   colnames(HomeStuff) = paste0('home.', colnames(HomeStuff))
  #   
  #   homeQuarters <- data.frame(do.call(rbind, HomeStuff$home.scoreBreakdown))
  #   
  #   colnames(homeQuarters) = c('home.quarter1.score','home.quarter2.score','home.quarter3.score','home.quarter4.score')
  #   
  #   
  #   AwayStuff <- rawfromJSON$scoreboard$games[[k]]$home
  #   colnames(AwayStuff) = paste0('away.', colnames(AwayStuff))
  #   
  #   awayQuarters <- data.frame(do.call(rbind, AwayStuff$away.scoreBreakdown))
  #   
  #   colnames(awayQuarters) = c('away.quarter1.score','away.quarter2.score','away.quarter3.score','away.quarter4.score')
  #   
  #   HomeAway <- data.frame(
  #     HomeStuff %>% dplyr::select(-home.scoreBreakdown)
  #     ,sapply(homeQuarters, function(x){as.numeric(as.character(x))})
  #     ,AwayStuff %>% dplyr::select(-away.scoreBreakdown)
  #     ,sapply(awayQuarters, function(x){as.numeric(as.character(x))})
  #   )
  #   
    jsonLinks <- foreach(i = 1:length(rawfromJSON$scoreboard$games[[k]]$tabsArray), .combine = 'rbind') %do% {
      
      tabsarray <- rawfromJSON$scoreboard$games[[k]]$tabsArray[[i]][[1]]
      
        recapURL = tabsarray$file[tabsarray$title == 'Recap']
        boxScoreURL = tabsarray$file[tabsarray$title == 'Box Score']
        scoringURL = tabsarray$file[tabsarray$title == 'Scoring']
        teamStatsURL = tabsarray$file[tabsarray$title == 'Team Stats']
        pbpURL = tabsarray$file[tabsarray$title == 'Play-By-Play']
      
        data.frame(
          recapURL = 
            gsub('/sites/default/files/data/'
                 ,'http://data.ncaa.com/jsonp/'
                 ,ifelse(length(recapURL) == 0, NA, recapURL))
          ,boxScoreURL = gsub('/sites/default/files/data/'
                 ,'http://data.ncaa.com/jsonp/'
                 ,ifelse(length(boxScoreURL) == 0, NA, boxScoreURL))
          ,scoringURL = gsub('/sites/default/files/data/'
                 ,'http://data.ncaa.com/jsonp/'
                 ,ifelse(length(scoringURL) == 0, NA, scoringURL))
          ,teamStatsURL = gsub('/sites/default/files/data/'
                 ,'http://data.ncaa.com/jsonp/'
                 ,ifelse(length(teamStatsURL) == 0, NA, teamStatsURL))
          ,pbpURL = gsub('/sites/default/files/data/'
                 ,'http://data.ncaa.com/jsonp/'
                 ,ifelse(length(pbpURL) == 0, NA, pbpURL))
        )
        
    }
    rawfromJSON$scoreboard$games[[k]]$tabsArray
    
    
    
      data.frame(
        ncaa_id          = rawfromJSON$scoreboard$games[[k]]$id
        ,conference      = rawfromJSON$scoreboard$games[[k]]$conference
        ,game_state      = rawfromJSON$scoreboard$games[[k]]$gameState
        ,start_ts        = as.POSIXct(as.numeric(rawfromJSON$scoreboard$games[[k]]$startTimeEpoch), origin = '1970-01-01')
        ,current_period  = rawfromJSON$scoreboard$games[[k]]$currentPeriod
        # ,final_message   = rawfromJSON$scoreboard$games[[k]]$finalMessage
        # ,game_status     = rawfromJSON$scoreboard$games[[k]]$gameStatus
        # ,period_status   = rawfromJSON$scoreboard$games[[k]]$periodStatus
        # ,down_to_go      = rawfromJSON$scoreboard$games[[k]]$downToGo
        ,timeclock       = rawfromJSON$scoreboard$games[[k]]$timeclock
        ,network_logo    = rawfromJSON$scoreboard$games[[k]]$network_logo
        ,location        = rawfromJSON$scoreboard$games[[k]]$location
        ,contest_name    = rawfromJSON$scoreboard$games[[k]]$contestName
        ,url             = rawfromJSON$scoreboard$games[[k]]$url
        ,highlights_url  = rawfromJSON$scoreboard$games[[k]]$highlightsUrl
        # ,live_audio_url  = rawfromJSON$scoreboard$games[[k]]$liveAudioUrl
        ,game_center_url = rawfromJSON$scoreboard$games[[k]]$gameCenterUrl
        # ,HomeAway
        # ,rawfromJSON$scoreboard$games[[k]]$alerts
        ,jsonLinks
        
        # ,champ_info      = rawfromJSON$scoreboard$games[[k]]$champInfo
    #     ,videos          = rawfromJSON$scoreboard$games[[k]]$videos
    #     ,score_breakdown = rawfromJSON$scoreboard$games[[k]]$scoreBreakdown
    #     ,home            = 
    #     ,away            = rawfromJSON$scoreboard$games[[k]]$away
    #     ,tabs            = rawfromJSON$scoreboard$games[[k]]$tabs
    #     ,tabs_array      = rawfromJSON$scoreboard$games[[k]]$tabsArray
    #     ,status          = rawfromJSON$scoreboard$games[[k]]$status
    #     ,alerts          = rawfromJSON$scoreboard$games[[k]]$alerts
    )
    
    
  }





}



