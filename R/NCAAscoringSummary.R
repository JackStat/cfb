#' @title Scrape NCAA scoringSummary
#' 
#' @import dplyr foreach jsonlite
#' 
#' @examples NCAAscoringSummary("http://data.ncaa.com/jsonp/game/football/fbs/2016/10/15/utah-oregon-st/scoringSummary.json")
#' 
#' @export

NCAAscoringSummary <- function(url){

  rawfromJSON <- fromJSON(gsub("jsonp", "sites/default/files/data", url))
  
  meta <- rawfromJSON$meta$teams
  
  Rows <- sapply(rawfromJSON$periods$summary, nrow)
  
  if(length(Rows) == 0){
    NULL
  } else {
    data.frame(
      quarter = rep(rawfromJSON$periods$title, Rows)
      ,do.call("rbind", rawfromJSON$periods$summary)
      ,date = as.Date(gsub("(.*)\\/([0-9]{4,})\\/([0-9]{2,})\\/([0-9]{2,})(.*)", "\\2-\\3-\\4", url))
      ,vsText = gsub("(.*)\\/([0-9]{4,})\\/([0-9]{2,})\\/([0-9]{2,})\\/(.*)\\/scoringSummary.json", "\\5", url)
    ) %>%
      left_join(meta, by = c("teamId" = "id"))
  }
  
}

