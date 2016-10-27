#' @title Scrape NCAA json files.
#' 
#' @import dplyr
#' 
#' @examples NCAApbp("http://data.ncaa.com/jsonp/game/football/fbs/2015/09/03/michigan-utah/pbp.json")
#' 
#' @export

NCAApbp <- function(url){
    
  rawfromJSON <- fromJSON(gsub("jsonp", "sites/default/files/data", url))

  Main <- foreach(i = 1:length(rawfromJSON$periods$possessions), .combine = 'rbind') %do% {
    
    plays <- do.call(rbind, rawfromJSON$periods$possessions[[i]]$plays)
    
    PlayCount <- sapply(rawfromJSON$periods$possessions[[i]]$plays, nrow)
  
    data.frame(
      teamId = rep(rawfromJSON$periods$possessions[[i]]$teamId, PlayCount)
      ,time = rep(rawfromJSON$periods$possessions[[i]]$time, PlayCount)
      ,plays
      ,quarter = i
    )
    
  }

  Opponent1 <- rawfromJSON$meta$teams[rawfromJSON$meta$teams$homeTeam == 'false',]$shortname
  Opponent2 <- rawfromJSON$meta$teams[rawfromJSON$meta$teams$homeTeam == 'true',]$shortname
  
  Main %>%
    mutate(
      teamId = as.character(teamId)
    ) %>%
    left_join(rawfromJSON$meta$teams, by = c('teamId' = 'id')) %>%
    mutate(
      Team = ifelse(homeTeam == 'true', Opponent2, Opponent1)
      ,Opponent = ifelse(homeTeam == 'false', Opponent2, Opponent1)
      ,visitingScore = cleanScores(visitingScore)
      ,homeScore = cleanScores(homeScore)
      ,homeTeam = homeTeam == 'true'
    )


}



