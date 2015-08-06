#' @title Scrape ESPN pbp tables.
#' 
#' 
#' @import XML RCurl dplyr
#' 
#' @export
# library(XML)
# library(doMC)
# library(data.table)
# library(tidyr)
# library(RCurl)
# library(dplyr)
# library(jsonlite)


ESPNpbp <- function(pbpLink) {
  setTxtProgressBar(pb, t)
  
  pbpPage<-htmlParse(paste0("http://scores.espn.go.com/",pbpLink , "&period=0"))
  
  gtl<-getNodeSet(pbpPage, "//div[@class='game-time-location']/p")
  
  timeLoc <- sapply(gtl, xmlValue)
  
  theurl = paste0("http://scores.espn.go.com/", pbpLink, "&period=0")

  tree <- htmlTreeParse(theurl, isURL=TRUE, useInternalNodes=TRUE)
  
  pbpDirty <- readHTMLTable(theurl)[[2]]
    
  PossesionString <- '([a-zA-Z]{1,50}) at ([0-9]{1,2}\\:[0-9]{2,})'
  PosessionsT <- grepl(PossesionString, pbpDirty$V1)
  Team <- gsub(PossesionString, '\\1', pbpDirty[PosessionsT,'V1'])
  time <- gsub(PossesionString, '\\2', pbpDirty[PosessionsT,'V1'])
  timeW <- cumsum(PosessionsT)
  timeW[timeW == 0] = 1
  
  QuartersT <- grepl('Quarter Play', pbpDirty$V1) & !duplicated(pbpDirty$V1)
  QuartersW <- cumsum(QuartersT)
  QuartersW[QuartersW == 0] = 1
  QuartersText <- c('1st', '2nd', '3rd', '4th')
  
  pbp <- data.frame(
    scoreText = ifelse(grepl('Â', pbpDirty$V1), NA, as.character(pbpDirty$V1))
    ,driveText = ifelse(grepl('Â', pbpDirty$V2), NA, as.character(pbpDirty$V2))
    ,visitingScore = cleanScores(pbpDirty$V3)
    ,homeScore = cleanScores(pbpDirty$V4)
    ,GameTime = time[timeW]
    ,TeamID = Team[timeW]
    ,Quarter = QuartersText[QuartersW]
  ) %>% 
    filter(
      !grepl('Quarter Play|DRIVE TOTALS', scoreText) & 
        !PosessionsT &
        ! grepl('wins the toss|End of|Timeout', driveText))
  
  
  Home = getNodeSet(pbpPage, '//*[contains(@class, "home")]//a')
  Away = getNodeSet(pbpPage, '//*[contains(@class, "away")]//a')
  
  home = sapply(Home, xmlValue)
  away = sapply(Away, xmlValue)
  
  gameId = gsub('(/ncf/playbyplay\\?gameId\\=)([0-9]{1,50})', '\\2', byYear$pbpLink[t])
  
  list(
    gameId = gameId
    , gameTime = makeTime(timeLoc[1])
    , gameLocation = timeLoc[2]
    , url = theurl
    , teams = c(home[[1]], away[[1]])
    , home = home
    , away = away
    , pbp = pbp)
  
}


