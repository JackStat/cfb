#' @title Find and Parse Fumbles
#' 
#' 
#' @export


ParseFumble <- function(x){
  
  x$scoreText <- gsub('no gain', '0 yards', x$scoreText)
  
  x$Fumble <- grepl('FUMBLES', x$scoreText)
  
  x$fumbleText[x$Fumble] <- 
    gsub('(.*?)(FUMBLES)(\\.|)(.*)', '\\2\\4', x$scoreText[x$Fumble])
  
  x$scoreText <- gsub('(.*), (FUMBLES)(.*)', '\\1\\.', x$scoreText)
  
  
  x$FumbleReturner = NA
  x$FumbleReturnYards = NA
  
  regParse <- 
    paste0(
      'FUMBLES '
      ,'\\(([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20})\\)\\. '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) '
      ,'(to|runs|runs ob at) '
      ,'([A-Z]{2,6}) ([0-9]{1,3}) for '
      ,'([0-9]{1,3}) '
      ,'(yard|yards)\\.'
    )
  
  Cond <- grepl(regParse, x$fumbleText) 
  
  x$FumbleReturner[Cond] = gsub(regParse, '\\2', x$fumbleText[Cond])
  x$FumbleReturnYards[Cond] = gsub(regParse, '\\6', x$fumbleText[Cond])
  
  x$FumbleReturnYards = as.numeric(x$FumbleReturnYards)
  
  x
  
}
