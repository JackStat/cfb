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
    RK = as.numeric(as.character(rr[[1]][-1,1]))
    ,Team = gsub('([A-Za-z\\(\\)\\&]{2,25})([\n|\t]{1,55})[A-Z]{2,6}(([\n|\t]{1,55}|)\\([0-9]{0,3}\\)|)', '\\1', as.character(rr[[1]][-1,2]))
    ,Points = ifelse(as.numeric(as.character(rr[[1]][-1,4])) == 0, NA, as.numeric(as.character(rr[[1]][-1,4])))
    ,Year = year
    ,Week = week
    ,H1 = sapply(getNodeSet(doc, '//*[(@id = "main-container")]//h1'), xmlValue)
    )
  
}
