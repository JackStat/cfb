#' @title Scrape ESPN pbp tables.
#' 
#' 
#' @import XML RCurl dplyr stringr
#' 
#' @export


ESPNpbp <- function(gameId) {
  
  theurl <- paste0("http://scores.espn.go.com/college-football/playbyplay?gameId=",gameId , "&period=0")
  
  pbpPage<-htmlParse(theurl)
  
  tree <- htmlTreeParse(theurl, isURL=TRUE, useInternalNodes=TRUE)
  
  pbp <- getNodeSet(pbpPage, '//*[(@id = "gamepackage-drives-wrap")]')
  
  pbpSpacy <- str_split(sapply(pbp, xmlValue),'\t{1,1000}|\n{1,1000}')[[1]]
  
  
  pbpDirty <- pbpSpacy[pbpSpacy != ""]
  
  
  DownX <- '(1st|2nd|3rd|4th) and ([0-9]{1,3}) at ([A-Z]{2,6} |)([0-9]{1,3})'
  DownG <- (grepl(DownX,pbpDirty))
  
  DW <- cumsum(DownG)
  DownsW <- ifelse(DW==0, 1, DW)
  
  Down <- gsub(DownX,'\\1',pbpDirty[DownG])
  DownYds <- gsub(DownX,'\\2',pbpDirty[DownG])
  DownSide <- gsub(DownX,'\\3',pbpDirty[DownG])
  DownYrdLine <- gsub(DownX,'\\4',pbpDirty[DownG])
  
  
  DriveX <- '(.*?)([0-9]{1,3} )plays, ((-|)[0-9]{1,3} )(yards, |yard, )([0-9]{1,2}:[0-9]{1,2})([A-Z]{2,6})([0-9]{,3})([A-Z]{2,6})([0-9]{,3})'
  DriveG <- grepl(DriveX,pbpDirty)
  DriveW <- which(DriveG)

  
  QuarterX <- 'End of (1st|2nd|3rd|4th) Quarter'
  QuarterG <- grepl(QuarterX,pbpDirty)
  QsW <- cumsum(grepl(QuarterX,pbpDirty))+1
  QuarterW <- ifelse(QsW==5, 4, QsW)
  
  
  data.frame(
    gameId = gameId
    ,pbpText = pbpDirty
    ,Quarter = QuarterW
    ,Down = Down[DownsW]
    ,DownYds = DownYds[DownsW]
    ,DownSide = DownSide[DownsW]
    ,DownYrdLine = DownYrdLine[DownsW]
  ) %>% 
    .[!DownG & !DriveG & !QuarterG,] 
  
}


