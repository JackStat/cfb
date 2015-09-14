#' @title Parse passes from NCAA scrape
#' 
#' 
#' 
#' @export



ParsePass <- function(x){
  
  x <- cleanPlayers(x)
  
  textMod <- as.character(x$scoreText)
  x$Receiver = rep(NA, nrow(x))
  
  # remove declined penalties
  regParsePenalty <-
    paste0(
    '(.*) '
    ,'(Penalty on |Team penalty on )([A-Z]{2,6})\\, '
    ,'(.*)\\, declined\\.'
    )
  
  DeclinedCond <- grepl(regParsePenalty, textMod)
  
  textMod[DeclinedCond] = 
    gsub(regParsePenalty, '\\1', textMod[DeclinedCond])
  
  textMod <- gsub('no gain', '0 yards', textMod)
  
  # - Incompletes
  regParse = paste0(
    PlayerRegex(),' '
    ,'incomplete\\.(| Intended for )(|[0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20})|\\.'
    )
  excludes = 'kicks|Penalty|punts|sacked|FUMBLES|penalty|INTERCEPTED'
  
  Cond <- 
    grepl(regParse, textMod) & 
    !grepl(excludes, textMod) & 
    grepl('incomplete', textMod)
  
  x$Passer[Cond] = gsub(regParse, '\\1', textMod[Cond])
  x$Receiver[Cond] = gsub(regParse, '\\3', textMod[Cond])
  x$Receiver = ifelse(x$Receiver == '', NA, x$Receiver)
  x$Complete = FALSE
  
  x$PassAtt = FALSE
  x$PassAtt[Cond] = TRUE
  
  # - Completed passes
  regParse2 = paste0(
    "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
    ,'complete to '
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20})\\. "
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
    ,'(to|runs ob at|pushed ob at) '
    ,'([A-Z]{2,6}) ([0-9]{1,3}) for '
    ,'(-|)([0-9]{1,3}) (yards|yard)(\\.| \\([^*]+\\)\\.)'    
    )
  Cond2 = 
    grepl(regParse2, textMod) & 
    grepl('complete', textMod)  & 
    !grepl(excludes, textMod)
  
  x$Passer[Cond2] = gsub(regParse2, '\\1', textMod[Cond2])
  x$Receiver[Cond2] = gsub(regParse2, '\\2', textMod[Cond2])
  x$PassYards[Cond2] = gsub(regParse2, '\\7\\8', textMod[Cond2])
  x$Complete[Cond2] = TRUE
  
  x$PassAtt[Cond2] = TRUE
  
  
  # - Touchdown
  regParse3 = paste0(
    "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
    ,'complete to '
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20})\\. "
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
    ,'(runs) (-|)([0-9]{1,3}) (yards|yard) for a '
    ,'(touchdown)\\.'    
    )
  Cond3 = 
    grepl(regParse3, textMod) & 
    grepl('complete', textMod) & 
    !Cond & !Cond2 & 
    !grepl(excludes, textMod)
  
  x$Passer[Cond3] = gsub(regParse3, '\\1', textMod[Cond3])
  x$Receiver[Cond3] = gsub(regParse3, '\\2', textMod[Cond3])
  x$PassYards[Cond3] = gsub(regParse3, '\\5\\6', textMod[Cond3])
  x$Complete[Cond3] = TRUE
  
  x$PassAtt[Cond3] = TRUE
  
  # - no gain
  regParse4 = paste0(
    "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
    ,'complete to '
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20})\\. "
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) to "
    ,'([A-Z]{2,6}) ([0-9]{1,3}) for no gain\\.'    
    )
  Cond4 = 
    grepl(regParse4, textMod) & 
    grepl('complete', textMod) & 
    !Cond & !Cond2 & !Cond3  & 
    !grepl(excludes, textMod)
  
  x$Passer[Cond4] = gsub(regParse4, '\\1', textMod[Cond4])
  x$Receiver[Cond4] = gsub(regParse4, '\\2', textMod[Cond4])
  x$PassYards[Cond4] = 0
  x$Complete[Cond4] = TRUE
  
  x$PassAtt[Cond4] = TRUE
  
  # - caught out of bounds
  regParse5 = paste0(
    "([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}) "
    ,'complete to '
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20})\\. "
    ,"([0-9]{0,4}-[A-Z]\\.[A-Za-z\\'\\-]{1,20}), out of bounds at the "
    ,'([A-Z]{2,6}) ([0-9]{1,3})\\.'    
    )
  Cond5 = 
    grepl(regParse5, textMod) & 
    grepl('complete', textMod) & 
    !Cond & !Cond2 & !Cond3 & !Cond4 
    !grepl(excludes, textMod)
  
  x$Passer[Cond5] = gsub(regParse5, '\\1', textMod[Cond5])
  x$Receiver[Cond5] = gsub(regParse5, '\\2', textMod[Cond5])
  x$PassYards[Cond5] = NA
  x$Complete[Cond5] = TRUE
  
  x$PassAtt[Cond5] = TRUE
  
  x$PassYards <- as.numeric(x$PassYards)
  x  
}
