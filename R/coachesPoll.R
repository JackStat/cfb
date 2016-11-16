#' @title Scrape ESPN Coaches Poll
#' 
#' @import XML
#' 
#' @export

coachesPoll <- function(year, week){
  
  url <- paste0('http://espn.go.com/college-football/rankings/_/poll/2/seasontype/2/year/',year,'/week/',week)
  
  doc <- htmlParse(url)
  
  rr <- readHTMLTable(doc)
  
  data.frame(
    RK = as.numeric(as.character(rr[[1]][,1]))
    ,Team = gsub('(.*)([a-z]|A&M)([A-Z]{3,5}|TA&M)(\\([0-9]{1,2}\\)|)', '\\1\\2', as.character(rr[[1]][,2]))
    ,Points = ifelse(as.numeric(as.character(rr[[1]][,4])) == 0, NA, as.numeric(as.character(rr[[1]][,4])))
    ,Year = year
    ,Week = week
    )
  
}
