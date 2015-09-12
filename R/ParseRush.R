#' @title Parse Rushing Attempts from NCAA scrape
#' 
#' 
#' 
#' @export



ParseRush <- function(x){
 
  # - Style 1
  regParse = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
      ,'(runs ob at|runs to|scrambles to|to|pushed ob|pushed ob at|scrambles, runs ob at|sacked at|scrambles, pushed ob at) '
      ,'([A-Z]{2,4}) ([0-9]{1,3}) '
      ,'(for) (-|)([0-9]{1,3}) '
      ,'(yards|yard)(\\.| \\([^*]+\\)\\.)'
    )
    #Fixing no gain...
  x[,"scoreText"] <- gsub('no gain', '0 yards', x[,"scoreText"])
  
  excludes = 'complete|kicks|Penalty|punts|FUMBLES|penalty'
  Cond <- grepl(regParse, x[,'scoreText']) & !grepl(excludes, x[,'scoreText'])
  x$Rusher[Cond] = gsub(regParse, '\\1', x[Cond,'scoreText'])
  x$RushAtt = FALSE
  x$RushAtt[Cond] = TRUE
  x$RushYards[Cond] = gsub(regParse, '\\6\\7', x[Cond,'scoreText'])
  
  # - Run for TD
  regParse3 = "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) (runs|scrambles, runs) ([0-9]{1,3}) (yard|yards) for a touchdown."
  Cond3 <- grepl(regParse3, x[,'scoreText']) & !grepl(excludes, x[,'scoreText']) & !Cond
  x$Rusher[Cond3] = gsub(regParse3, '\\1', x[Cond3,'scoreText'])
  x$RushAtt[Cond3] = TRUE
  x$RushYards[Cond3] = gsub(regParse3, '\\3', x[Cond3,'scoreText'])
  
  x$RushYards <- as.numeric(x$RushYards)
  x  
}
