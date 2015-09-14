#' @title Find and Parse Kicks
#' 
#' 
#' @export


ParseKick <- function(x){
  
  
  x <- cleanPlayers(x)
    
  # - Touchbacks
  regParse = 
    paste0(
      "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'kicks ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6}) ([0-9]{1,3}) to '
      ,'([A-Z]{2,6}) (End Zone|[0-9]{1,3})\\. '
      ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}, |)"
      ,'touchback\\.'
    )
  Cond <- grepl(regParse, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"])
  
  x$Kick = FALSE
  x$KickReturn = FALSE
  x$KickYards = NA
  
  x$Kick[Cond] = TRUE
  x$Kicker[Cond] = gsub(regParse, '\\1', x[Cond,"scoreText"])
  x$KickYards[Cond] = gsub(regParse, '\\2', x[Cond,"scoreText"])
  
  
  # - Returned
  regParse2 = 
    paste0(
      "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'kicks ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6} [0-9]{1,3})\\. '
      ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'(runs ob at|runs to|scrambles to|to|pushed ob|pushed ob at) '
      ,'([A-Z]{2,6} [0-9]{1,3}) for '      
      ,'(-|)([0-9]{1,3}) (yards|yard)'
      ,'(\\.| \\([^*]+\\)\\.)'
    )
  
  # solving the no gain situation...
  x[,"scoreText"] <- gsub('no gain', '0 yards', x[,"scoreText"])
  
  Cond2 <- grepl(regParse2, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond
  
  x$Kick[Cond2] = TRUE
  x$KickReturn[Cond2] = TRUE
  x$Kicker[Cond2] = gsub(regParse2, '\\1', x[Cond2,"scoreText"])
  x$KickYards[Cond2] = gsub(regParse2, '\\2', x[Cond2,"scoreText"])
  x$KickReturnYards[Cond2] = gsub(regParse2, '\\7\\8', x[Cond2,"scoreText"])
  
  # - Downed
  regParse3 = 
    paste0(
      "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'kicks ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6}) ([0-9]{1,3}) to( the|) '
      ,'([A-Z]{2,6}) ([0-9]{1,3}), '
      ,'(downed|fair catch) by '
      ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20})(.*?|)\\."
    )
  
  Cond3 <- grepl(regParse3, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond & !Cond2
  
  x$Kick[Cond3] = TRUE
  x$KickReturn[Cond3] = FALSE
  x$Kicker[Cond3] = gsub(regParse3, '\\1', x[Cond3,"scoreText"])
  x$KickYards[Cond3] = gsub(regParse3, '\\2', x[Cond3,"scoreText"])
  
  
  # - out-of-bounds
  regParse4 = 
    paste0(
      "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'kicks ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6}) ([0-9]{1,3}), out of bounds at the '
      ,'([A-Z]{2,6}) ([0-9]{1,3})\\.'
    )
  
  Cond4 <- 
    grepl(regParse4, x[,"scoreText"]) & 
      !grepl('Penalty', x[,"scoreText"]) &
      !Cond & !Cond2 & !Cond3
  
  x$Kick[Cond4] = TRUE
  x$KickReturn[Cond4] = FALSE
  x$Kicker[Cond4] = gsub(regParse4, '\\1', x[Cond4,"scoreText"])
  x$KickYards[Cond4] = gsub(regParse4, '\\2', x[Cond4,"scoreText"])
  
  
  # touchdown
  regParse5 = 
    paste0(
      "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'kicks ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6} [0-9]{1,3})\\. '
      ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) "
      ,'(runs) '
      ,'([0-9]{1,3}) yards for a touchdown\\.'
    )
  
  Cond5 <- grepl(regParse5, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond & !Cond2 & !Cond3 & !Cond4
  
  x$Kick[Cond5] = TRUE
  x$KickReturn[Cond5] = TRUE
  x$Kicker[Cond5] = gsub(regParse5, '\\1', x[Cond5,"scoreText"])
  x$KickYards[Cond5] = gsub(regParse5, '\\2', x[Cond5,"scoreText"])
  x$KickReturnYards[Cond5] = gsub(regParse5, '\\6', x[Cond5,"scoreText"])
  
  # No names....
  regParse6 = 
    paste0(
      "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20} |)"
      ,'kicks ([0-9]{1,3}) '
      ,'yards from '
      ,'([A-Z]{2,6} [0-9]{1,3})\\. '
      ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20} |)to "
      ,"([A-Z]{2,6} [0-9]{1,3}) for "
      ,'([0-9]{1,3}) yards\\.'
    )
  
  Cond6 <- 
    grepl(regParse6, x[,"scoreText"]) & 
    !grepl('Penalty', x[,"scoreText"]) & 
    !Cond & !Cond2 & !Cond3 & !Cond4 & !Cond5
  
  x$Kick[Cond6] = TRUE
  x$KickReturn[Cond6] = TRUE
  x$Kicker[Cond6] = trim(gsub(regParse6, '\\1', x[Cond6,"scoreText"]))
  x$KickYards[Cond6] = gsub(regParse6, '\\2', x[Cond6,"scoreText"])
  x$KickReturnYards[Cond6] = gsub(regParse6, '\\6', x[Cond6,"scoreText"])
  
  
  x$KickYards = as.numeric(x$KickYards)
  
  # - Handling games with no kick returns
  if(is.null(x$KickReturnYards)) {
    x$KickReturnYards = rep(NA, nrow(x))
  } else {
    x$KickReturnYards = as.numeric(x$KickReturnYards)
  }

  x
}
