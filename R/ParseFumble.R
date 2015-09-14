#' @title Identify No Plays
#' 
#' 
#' @export

ParseFumble <- function(x){
  
  x <- cleanPlayers(x)
  
  x$Fumble = FALSE
  x$FumbleYards = rep(NA, nrow(x))
  x$Turnover = FALSE
  
  # - Rushing Fumble
  regParse = paste0(
    "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-\\']{1,20}) to "
    ,'([A-Z]{2,6}) ([0-9]{1,3}), FUMBLES\\. '
    ,"([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-\\']{1,20}) to "
    ,'([A-Z]{2,6}) ([0-9]{1,3}) for '
    ,'([0-9]{1,3}) yards\\.'
  )
  
  Cond <- grepl(regParse, x[,'scoreText'])
  
  x$Fumble[Cond] = TRUE
  x$FumbleYards[Cond] <- as.numeric(
    gsub(regParse, '\\7', x[Cond,'scoreText'])
    )
  
   # - Sacked Fumble
  regParse2 = paste0(
    "([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-\\']{1,20}) "
    ,'sacked at '
    ,'([A-Z]{2,6} [0-9]{1,3}) '
    ,'for '
    ,'(-|)([0-9]{1,3}) (yards|yard), '
    ,'FUMBLES '
    ,"(\\([0-9]{1,4}-[A-Z]\\.[A-Za-z\\-\\']{1,20}\\)\\.|[0-9]{1,4}-[A-Z]\\.[A-Za-z\\-\\']{1,20}) to "
    ,'([A-Z]{2,6} [0-9]{1,3}) for '
    ,'([0-9]{1,3}) yards\\.'
  )
  
  Cond2 <- grepl(regParse2, x[,'scoreText']) & !Cond
  
  x$Fumble[Cond2] = TRUE
  x$FumbleYards[Cond2] <- as.numeric(gsub(regParse2, '\\8', x[Cond2,'scoreText']))
  # - If turnover the recovery player should be in parentheses
  x$Turnover[Cond2] = grepl('\\(', gsub(regParse2, '\\6', x[Cond2,'scoreText']))
  
  x
  
}
