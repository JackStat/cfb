#' @title Scrape ESPN Schedules
#' @description Scrape and clean up schedules into a \code{data.frame}
#' 
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' 
#' @export


ESPNSchedule <- function(Year = 2016, ESPNSource = NULL) {
  
  if(is.null(ESPNSource)){
    ESPNSource <- ESPNGET(Year)
  }
  
  IdJSON <- function(x){
    # Basically give me everything between { and } but keep the {}
    g1 <- gsub("(.*) \\{(.*)\\} (.*)","\\{\\2\\}",x)
    # add in leading double quotes so we can convert form JSON
    g2 <- gsub("(\\{| )([a-z]{1,})", '\\1"\\2', g1)
    # add in trailing double quotes so we can convert form JSON
    g3 <- gsub("([a-z]{1,})(\\:)", '\\1"\\2', g2)
    as.data.frame(fromJSON(g3))
  }
  
  CC <- lapply(ESPNSource, function(x){
    
    doc <- xml2::read_html(x$content)
    
    IdInfo <- rvest::html_nodes(doc, xpath="/html/body[contains(@class, 'ncf')]") %>% xml2::xml_attrs()
    Team <- rvest::html_nodes(doc, xpath='//*[@id="sub-branding"]/h2/a/b') %>% rvest::html_text()
    Info <- rvest::html_nodes(doc, xpath='//table//tr//td[contains(@colspan, 4)]') %>% rvest::html_text()
    
    data.frame(
      as.data.frame(rvest::html_table(doc))
      ,Team = Team[1]
      ,Info = Info[1]
      ,IdJSON(IdInfo[[1]][[1]])
    )
  })
  
  dirtyGames <- do.call(rbind, CC)
    
  Schedule <-
    dirtyGames %>% 
    dplyr::mutate(
      Home = as.numeric(substr(X2, 1, 2) == 'vs')
      # - Need to remove anything after a space just in case there is OT
      ,X3 = gsub(' [A-z0-9 ]*', '' , X3)
      # - 1st char in either W or L for the games that have already been played
      ,Result = as.factor(substr(X3, 1, 1))
      # - Removing the vs or @
      ,Opponent = espnOpp(X2)
      ,Year = substr(Info, 1, 4)
      ,Date = espnDate(paste(X1, Year))
    ) %>%
    # - Keep games that haven't happened yet and have a Date
    dplyr::filter(!Result %in% c("W", "L") & !is.na(Date)) %>%
    dplyr::select(
      Date
      ,Home
      ,Opponent
      ,Team
      ,Year
    )
  
  Schedule <- Numerics(Schedule)
  
  return(Schedule)
}

