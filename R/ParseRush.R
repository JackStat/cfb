#' @title Parse Rushing Attempts from NCAA scrape
#' 
#' 
#' 
#' @export



ParseRush <- function(x){
  
  textMod <- as.character(x$scoreText)
  
  # Players with spaces in last name
  textMod <- gsub("([0-9]{1,3}-[A-Z]{1,1}\\.[A-Z]{1,1}[a-z]{1,20})(\\ |\\-)([A-Z]{1,1}[a-z]{1,20})",'\\1-\\3', textMod)
  
  # remove declined penalties
  regParsePenalty <-
    paste0(
    '(.*) '
    ,'(Penalty on |Team penalty on )([A-Z]{2,4})\\, '
    ,'(.*)\\, declined\\.'
    )
  
  DeclinedCond <- grepl(regParsePenalty, textMod)
  
  textMod[DeclinedCond] = 
    gsub(regParsePenalty, '\\1', textMod[DeclinedCond])
  
  textMod <- gsub('no gain', '0 yards', textMod)
  
  x$Kneel = FALSE
  x$RushAtt = FALSE
  
  
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
  
  
  excludes = 'complete|kicks|Penalty|punts|FUMBLES|penalty'
  Cond <- grepl(regParse, textMod) & !grepl(excludes, textMod)
  x$Rusher[Cond] = gsub(regParse, '\\1', textMod[Cond])
  
  x$RushAtt[Cond] = TRUE
  x$RushYards[Cond] = gsub(regParse, '\\6\\7', textMod[Cond])
  
  # - Run for TD
  regParse3 = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
      ,"(runs|scrambles, runs) ([0-9]{1,3}) "
      ,"(yard|yards) for a touchdown."
    )
  Cond3 <- grepl(regParse3, textMod) & !grepl(excludes, textMod) & !Cond
  x$Rusher[Cond3] = gsub(regParse3, '\\1', textMod[Cond3])
  x$RushAtt[Cond3] = TRUE
  x$RushYards[Cond3] = gsub(regParse3, '\\3', textMod[Cond3])
  
  
  #- Kneels
  regParse4 = paste0(
    "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
    ,'kneels at '
    ,'([A-Z]{2,4}) ([0-9]{1,3}) for '
    ,'(-|)([0-9]{1,3}) (yards|yard)(\\.| \\([^*]+\\)\\.)' 
  )
  
  Cond4 <- grepl(regParse4, textMod) & !Cond & !Cond3
  x$Kneel[Cond4] = TRUE
  x$RushAtt[Cond4] = TRUE
  x$Rusher[Cond4] = gsub(regParse4, '\\1', textMod[Cond4])
  x$RushYards[Cond4] = gsub(regParse4, '\\4\\5', textMod[Cond4])
  
  
  x$RushYards <- as.numeric(x$RushYards)
  x  
}
