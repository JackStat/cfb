#' @title Scrape ESPN Game Results.
#' @description Using curl we pull all 254 teams schedules at once. Use this function sparingly as is hits ESPN's servers in an intense way. Takes about 25 seconds
#' 
#' @import XML curl dplyr
#' @importFrom tidyr separate
#' 
#' @export


ESPNResults <- function(Year) {
  
  url<-"http://espn.go.com/college-football/teams"
  
  doc <- htmlParse(url)
  
  tableNodesURL <- getNodeSet(doc, "//a[contains(@class,'bi')]/@href")
  
  teamMainURLs <- as.character(tableNodesURL)
  
  tableNodes <- getNodeSet(doc, "//a[contains(@class,'bi')]")
  
  team<-sapply(X = tableNodes, FUN = xmlValue)
  
  teamURLs <- paste0('http://www.espn.com/college-football/team/schedule?id=',gsub('(.*)\\/([0-9]{1,5})\\/(.*)','\\2',teamMainURLs))
  
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
  
  CC <- lapply(out, function(x){
    
  doc <- xml2::read_html(x$content)
  data.frame(
    as.data.frame(rvest::html_table(doc))
    ,Info = rvest::html_nodes(doc, xpath='//table//tr//td[contains(@colspan, 4)]') %>% rvest::html_text()
    ,url = x$url
  )
  })
  
  dirtyGames <- do.call(rbind, CC)
  
  cleanGames <-
    dirtyGames %>% 
    mutate(
      Home = as.numeric(substr(X2, 1, 2) == 'vs')
      # - Need to remove anything after a space just in case there is OT
      ,X3 = gsub(' [A-z0-9 ]*', '' , X3)
      ,Score = substr(X3, 2, 20)
      # - 1st char in either W or L for the games that have already been played
      ,Result = as.factor(substr(X3, 1,1))
      # - Removing the vs or @
      ,Opponent = espnOpp(X2)
      ,Year = substr(Info, 1, 4)
      ,Team = gsub('([0-9]{,4}) (.*) (Schedule)','\\2',Info)
      ,Date = espnDate(paste(X1, Year))
      ,espnID = as.numeric(gsub('(.*)\\=([0-9]{1,5})','\\2', url))
    ) %>%
    filter(Result %in% c("W", "L")) %>%
    separate(Score, c("PF1", "PA1")) %>%
    dplyr::select(
      Date
      ,Home
      ,PF1
      ,PA1
      ,Opponent
      ,Result
      ,Team
      ,Year
      ,espnID
    )
  
  cleanGames$PF[cleanGames$Result=='L'] = cleanGames$PA1[cleanGames$Result=='L']
  cleanGames$PA[cleanGames$Result=='L'] = cleanGames$PF1[cleanGames$Result=='L']
  cleanGames$PF[cleanGames$Result=='W'] = cleanGames$PF1[cleanGames$Result=='W']
  cleanGames$PA[cleanGames$Result=='W'] = cleanGames$PA1[cleanGames$Result=='W']
  
  cleanGames <- select(cleanGames, -PF1, -PA1)
  cleanGames
  
}

