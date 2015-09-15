#' @title Scrape ESPN pbp json.
#' 
#' @import XML foreach
#' 
#' @export


ESPNpbp <- function(gameId) {
  
  url <- paste0("http://scores.espn.go.com/college-football/gamecast?gameId=", gameId)
  
  doc <- htmlParse(url)
  
  gg <- getNodeSet(doc, '//script')
  
  gcXML <- sapply(gg, xmlValue)
  
  rawGameCast <- ss[grepl('NCFGamecast', gcXML)]
  
  preGC <- gsub('var settings =|[ ]{2,20}|;', '', rawGameCast)
  GC <- iconv(preGC, "latin1", "ASCII", sub="")
  
  
  gamecastData <- jsonlite::fromJSON(Money3)
  
  
  gamecastData$data$gamecast$playbyplay$drives$plays[[1]]$players
  
  
  Plays <- gamecastData$data$gamecast$playbyplay$drives$plays
  
  
  player <- foreach(i = 1:length(Plays), .combine = 'rbind', .errorhandling = 'remove') %do% {
    
    data.frame(
      play_id = rep(Plays[[i]]$id, sapply(Plays[[i]]$players, nrow))
      ,do.call(rbind, Plays[[i]]$players)
    )
    
  }
  
  play <- foreach(i = 1:length(Plays), .combine = 'rbind', .errorhandling = 'remove') %do% {
    
    Plays[[i]][,sapply(Plays[[i]], class) != 'list']
  }
    
  
  list(play = play, player = player)









  
}


