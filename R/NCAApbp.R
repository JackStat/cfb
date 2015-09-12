#' @title Scrape NCAA json files.
#' 
#' 
#' @import dplyr
#' 
#' @export

NCAApbp <- function(url){
    
  
raw<-suppressWarnings(readLines(url))

rawfromJSON <- fromJSON((gsub('callbackWrapper\\(','',gsub('\\}\\)\\;','\\}',raw))))


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


Main %>%
  mutate(
    teamId = as.character(teamId)
  ) %>%
  left_join(rawfromJSON$meta$teams, by = c('teamId' = 'id')) %>%
  mutate(
    visitingScore = cleanScores(visitingScore)
    ,homeScore = cleanScores(homeScore)
    ,homeTeam = homeTeam == 'true'
  )


}



