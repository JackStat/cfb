#' @title Find and Parse Interceptions
#' 
#' 
#' @export


ParseInterception <- function(x){
  
  x <- cleanPlayers(x)
  
  x$scoreText <- gsub('no gain', '0 yards', x$scoreText)
  
  x$Interception <- grepl('INTERCEPTED', x$scoreText)
  
  x$interceptionText[x$Interception] <- 
    gsub('(.*?)(INTERCEPTED)(.*?)', '\\2\\3', x$scoreText[x$Interception])
  
  x$scoreText <- gsub('(.*), (INTERCEPTED)(.*)', '\\1\\.', x$scoreText)
  
  
  x$InterceptionYards = NA
  x$Interceptor = NA
  
  regParse <- 
    paste0(
      'INTERCEPTED by '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) at '
      ,'([A-Z]{2,6}) ([0-9]{1,3})\\. '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) (to|runs|runs ob at) '
      ,'([A-Z]{2,6}) ([0-9]{1,3}) for '
      ,'([0-9]{1,3}) '
      ,'(yard|yards).*\\.'
    )
  
  Cond <- grepl(regParse, x$interceptionText) 
  
  x$Interceptor[Cond] = gsub(regParse, '\\1', x$interceptionText[Cond])
  x$InterceptionYards[Cond] = gsub(regParse, '\\8', x$interceptionText[Cond])
  
  
  # - touchback
  regParse2 = 
    paste0(
      'INTERCEPTED by '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) at '
      ,'([A-Z]{2,6}) ([0-9]{1,3})\\. '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}), touchback\\.'
    )
  
  Cond2 <- grepl(regParse2, x$interceptionText) & !Cond 
  
  x$Interceptor[Cond2] = gsub(regParse2, '\\1',  x$interceptionText[Cond2])
  x$InterceptionYards[Cond2] = 0
  
  
  # - touchdown
  regParse3 = 
    paste0(
      'INTERCEPTED by '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) at '
      ,'([A-Z]{2,6}) ([0-9]{1,3})\\. '
      ,'([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}) runs '
      ,'([0-9]{1,3}) '
      ,'yards for a touchdown\\.'
    )
  
  Cond3 <- grepl(regParse3, x$interceptionText) & !Cond & !Cond2
  
  x$Interceptor[Cond3] = gsub(regParse3, '\\1',  x$interceptionText[Cond3])
  x$InterceptionYards[Cond3] = 
    gsub(regParse3, '\\5', x$interceptionText[Cond3])
  
  
  x$InterceptionYards = as.numeric(x$InterceptionYards)
  
  x
}
