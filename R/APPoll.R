#' @title Scrape AP Poll
#' 
#' @import XML
#' 
#' @export

APPoll <- function(PollID){
  
  url <- paste0('http://www.databasefootball.com/College/polls/appoll.htm?PollID=', PollID)
  
  doc <- htmlParse(url)
  
  rr <- readHTMLTable(doc)
  
  data.frame(
    RK = as.numeric(gsub('\\.', '', as.character(rr[[2]][-1,1])))
    ,Change = as.character(rr[[2]][-1,2])
    ,Points = ifelse(as.numeric(as.character(rr[[2]][-1,5])) == 0, NA, as.numeric(as.character(rr[[2]][-1,5])))
    ,PollID
    ,H1 = gsub('[\n\t\r]','',sapply(getNodeSet(doc, '//h1'), xmlValue))
    )
  
}
