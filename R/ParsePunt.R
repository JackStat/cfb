#' @title Find and Parse Kicks
#' 
#' 
#' @export


ParsePunt <- function(x){
  
  x <- cleanPlayers(x)
  
  #Fixing no gain...
  x[,"scoreText"] <- gsub('no gain', '0 yards', x[,"scoreText"])
    
  # - Fair Catch
  regParse = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'punts ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6}) ([0-9]{1,3}) to '
      ,'([A-Z]{2,6}) ([0-9]{1,3}), '
      ,'fair catch by'
      ,"( [0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}|)\\."
    )
  Cond <- grepl(regParse, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"])
  
  x$Punt = FALSE
  x$PuntReturn = FALSE
  x$FairCatch = FALSE
  x$PuntReturnYards = NA
  x$Kicker = NA
  x$PuntYards = NA
  
  x$Punt[Cond] = TRUE
  x$FairCatch[Cond] = TRUE
  x$Kicker[Cond] = 
    gsub(regParse, '\\1', x[Cond,"scoreText"])
  x$PuntYards[Cond] = gsub(regParse, '\\2', x[Cond,"scoreText"])
  
  # - Out of Bounds
  regParse2 = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20} |)"
      ,'punts (-|)([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6}) ([0-9]{1,3}), '
      ,'out of bounds at the '
      ,'([A-Z]{2,6}) ([0-9]{1,3})\\.'
    )
  Cond2 <- grepl(regParse2, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond
    
  x$Punt[Cond2] = TRUE
  x$Kicker[Cond2] = trim(gsub(regParse2, '\\1', x[Cond2,"scoreText"]))
  x$PuntYards[Cond2] = gsub(regParse2, '\\2\\3', x[Cond2,"scoreText"])
  
  
  # - Returned
  regParse3 = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'punts ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6} [0-9]{1,3})\\. '
      ,"([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'(runs ob at|runs to|scrambles to|to|pushed ob|pushed ob at) '
      ,'([A-Z]{2,6} [0-9]{1,3}) for '      
      ,'(-|)([0-9]{1,3}) (yards|yard)'
      ,'(\\.| \\([^*]+\\)\\.)'
    )
  
  Cond3 <- grepl(regParse3, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond & !Cond2
  
  x$Punt[Cond3] = TRUE
  x$PuntReturn[Cond3] = TRUE
  x$Kicker[Cond3] = gsub(regParse3, '\\1', x[Cond3,"scoreText"])
  x$PuntYards[Cond3] = gsub(regParse3, '\\2', x[Cond3,"scoreText"])
  x$PuntReturnYards[Cond3] = gsub(regParse3, '\\7\\8', x[Cond3,"scoreText"])
  
  # - Returned TD
  regParse4 = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'punts ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6} [0-9]{1,3})\\. '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) runs '
      ,'([0-9]{1,3}) (yard|yards) for a touchdown.'
    )
  
  Cond4 <- grepl(regParse4, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond & !Cond2 & !Cond3
  
  x$Punt[Cond4] = TRUE
  x$PuntReturn[Cond4] = TRUE
  x$Kicker[Cond4] = gsub(regParse4, '\\1', x[Cond4,"scoreText"])
  x$PuntYards[Cond4] = gsub(regParse4, '\\2', x[Cond4,"scoreText"])
  x$PuntReturnYards[Cond4] = gsub(regParse4, '\\5', x[Cond4,"scoreText"])
  

  # - Touchback
  regParse5 = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'punts ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6}) ([0-9]{1,3}) to '
      ,'([A-Z]{2,6}) (End Zone|[0-9]{1,3})\\. '
      ,'touchback\\.'
    )
  Cond5 <- grepl(regParse5, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond & !Cond2 & !Cond3 & !Cond4
    
  x$Punt[Cond5] = TRUE
  x$Kicker[Cond5] = gsub(regParse5, '\\1', x[Cond5,"scoreText"])
  x$PuntYards[Cond5] = gsub(regParse5, '\\2', x[Cond5,"scoreText"])
  
  # - Downed
  regParse6 = 
    paste0(
      "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'punts ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6}) ([0-9]{1,3})'
      ,'( Downed at the | to the )'
      ,"([A-Z]{2,6}) ([0-9]{1,3})(\\.|\\, downed by [0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}\\.)"
    )
  Cond6 <- grepl(regParse6, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond & !Cond2 & !Cond3 & !Cond4
    
  x$Punt[Cond6] = TRUE
  x$Kicker[Cond6] = gsub(regParse6, '\\1', x[Cond6,"scoreText"])
  x$PuntYards[Cond6] = gsub(regParse6, '\\2', x[Cond6,"scoreText"])
  x$PuntReturn[Cond6] = FALSE
  x$PuntReturnYards[Cond6] = 0
  
  x$PuntYards = as.numeric(x$PuntYards)
  x$PuntReturnYards = as.numeric(x$PuntReturnYards)
  x
}
