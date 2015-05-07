#' @title Parse passes from NCAA scrape
#' 
#' 
#' 
#' @export



ParsePass <- function(x){
  # - Incompletes
  regParse = paste0(
    '([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) '
    ,'incomplete\\.(| Intended for )(|[0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20})|\\.'
    )
  excludes = 'kicks|Penalty|punts|sacked|FUMBLES|penalty|INTERCEPTED'
  
  Cond <- grepl(regParse, x$scoreText) & !grepl(excludes, x$scoreText) & grepl('incomplete', x$scoreText)
  x$Passer[Cond] = gsub(regParse, '\\1', x[Cond,'scoreText'])
  x$Receiver[Cond] = gsub(regParse, '\\3', x[Cond,'scoreText'])
  x$Receiver[x$Receiver=='']=NA
  x$Complete = FALSE
  
  x$PassAtt = FALSE
  x$PassAtt[Cond] = TRUE
  
  # - Completed passes
  regParse2 = paste0(
    '([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) '
    ,'complete to '
    ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20})\\. '
    ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) '
    ,'(to|runs ob at|pushed ob at) '
    ,'([A-Z]{2,4}) ([0-9]{1,3}) for '
    ,'(-|)([0-9]{1,3}) (yards|yard)(\\.| \\([^*]+\\)\\.)'    
    )
  Cond2 = grepl(regParse2, x[,'scoreText']) & grepl('complete', x$scoreText)  & !grepl(excludes, x$scoreText)
  x$Passer[Cond2] = gsub(regParse2, '\\1', x[Cond2,'scoreText'])
  x$Receiver[Cond2] = gsub(regParse2, '\\2', x[Cond2,'scoreText'])
  x$PassYards[Cond2] = gsub(regParse2, '\\7\\8', x[Cond2,'scoreText'])
  x$Complete[Cond2] = TRUE
  
  x$PassAtt[Cond2] = TRUE
  
  
  # - Touchdown
  regParse3 = paste0(
    '([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) '
    ,'complete to '
    ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20})\\. '
    ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) '
    ,'(runs) (-|)([0-9]{1,3}) (yards|yard) for a '
    ,'(touchdown)\\.'    
    )
  Cond3 = grepl(regParse3, x[,'scoreText']) & grepl('complete', x$scoreText) & !Cond & !Cond2  & !grepl(excludes, x$scoreText)
  x$Passer[Cond3] = gsub(regParse3, '\\1', x[Cond3,'scoreText'])
  x$Receiver[Cond3] = gsub(regParse3, '\\2', x[Cond3,'scoreText'])
  x$PassYards[Cond3] = gsub(regParse3, '\\5\\6', x[Cond3,'scoreText'])
  x$Complete[Cond3] = TRUE
  
  x$PassAtt[Cond3] = TRUE
  
  # - no gain
  regParse4 = paste0(
    '([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) '
    ,'complete to '
    ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20})\\. '
    ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) to '
    ,'([A-Z]{2,4}) ([0-9]{1,3}) for no gain\\.'    
    )
  Cond4 = grepl(regParse4, x[,'scoreText']) & grepl('complete', x$scoreText) & !Cond & !Cond2 & !Cond3  & !grepl(excludes, x$scoreText)
  x$Passer[Cond4] = gsub(regParse4, '\\1', x[Cond4,'scoreText'])
  x$Receiver[Cond4] = gsub(regParse4, '\\2', x[Cond4,'scoreText'])
  x$PassYards[Cond4] = 0
  x$Complete[Cond4] = TRUE
  
  x$PassAtt[Cond4] = TRUE
  
  x$PassYards <- as.numeric(x$PassYards)
  x  
}
