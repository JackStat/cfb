#' @title Pulls in all schedule pages for all teams
#' @description Using curl we pull all 254 teams schedules html at once. Use this function sparingly as is hits ESPN's servers in an intense way. Takes about 25 seconds
#' 
#' @import XML curl dplyr
#' 
#' @export


ESPNGET <- function(Year){
  url<-"http://espn.go.com/college-football/teams"
  
  doc <- htmlParse(url)
  
  tableNodesURL <- getNodeSet(doc, "//a[contains(@class,'bi')]/@href")
  
  teamMainURLs <- as.character(tableNodesURL)
  
  tableNodes <- getNodeSet(doc, "//a[contains(@class,'bi')]")
  
  team<-sapply(X = tableNodes, FUN = xmlValue)
  
  teamURLs <- paste0('http://www.espn.com/college-football/team/schedule/_/id/',gsub('(.*)\\/([0-9]{1,5})\\/(.*)','\\2',teamMainURLs),'/year/', Year)
  
  out <<- list()
  # This is function, function which will be run if data vendor call is successful
  complete = function(res){
    # cat("Request done! Status:", res$status, "\n")
    out <<- c(out, list(res))
  }
  
  for(i in 1:length(teamURLs)){
    curl_fetch_multi(
      teamURLs[i]
      , done = complete
      , fail = print
      , handle = new_handle(customrequest = "GET")
      )
  }
  
  multi_run()
  
  return(out)
}