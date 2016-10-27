#' @title Scrape NCAA scoringSummary
#' 
#' @import dplyr foreach jsonlite
#' 
#' @examples NCAAscoringSummary("http://data.ncaa.com/jsonp/game/football/fbs/2016/10/15/utah-oregon-st/scoringSummary.json")
#' 
#' @export

NCAAscoringSummary <- function(url){

  rawfromJSON <- fromJSON(gsub("jsonp", "sites/default/files/data", url))
  
  Rows <- sapply(rawfromJSON$periods$summary, nrow)
  
  data.frame(
    quarter = rep(rawfromJSON$periods$title, Rows)
    ,do.call("rbind", rawfromJSON$periods$summary)
  )
}

