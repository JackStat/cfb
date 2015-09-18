#' @title Identify No Plays
#' 
#' 
#' @export

ParsePenalty <- function(x){
  
  x <- cleanPlayers(x)
  
  PENALTIES <- '(Pass interference|Illegal substitution|False start|Personal Foul|Delay of game|Ineligible player downfield during passing down|Unsportsmanlike conduct|Illegal formation|Offside|Holding|Intentional grounding|Roughing the kicker|Running into kicker|Facemask, Incidental|Fair catch interference|Illegal block in the back|Unnecessary roughness|Roughing the passer|Illegal use of hands|Clipping|Chop block|Facemasking|Illegal Forward Pass|12 players|Illegal motion|12 men in the huddle|Illegal shift|Offside on Free Kick|Tripping|First onside kickoff out of bounds|Delay of game at start of either half|Illegal Procedure), '
  
  x$Penalty = FALSE
  x$NoPlay = FALSE
  x$PenaltyYards = rep(NA, nrow(x))
  x$PenaltyType = rep(NA, nrow(x))
  x$PenaltyPlayer = rep(NA, nrow(x))
  x$PenaltyDeclined = FALSE
  
  # - Penalty for incompletes
  regParse = paste0(
    '(.*?)'
    ,'(Penalty on |Team penalty on )([A-Z]{2,6})'
    ,'( [0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}|), '
    ,PENALTIES
    ,'([0-9]{1,3}) yards, enforced at ([A-Z]{1,6}) ([0-9]{1,2})\\.( No Play\\.|)'
  )
  
  Cond <- grepl(regParse, x[,'scoreText'])
  
  x$Penalty[Cond] = TRUE
  x$PenaltyYards[Cond] = gsub(regParse, '\\6', x[Cond, 'scoreText'])
  x$PenaltyType[Cond] = gsub(regParse, '\\5', x[Cond, 'scoreText'])
  x$NoPlay[Cond] = TRUE
  x$PenaltyPlayer[Cond] = trim(gsub(regParse, '\\4', x[Cond, 'scoreText']))
  
  
  # - Penalty
  regParse = paste0(
    'Penalty on ([A-Z]{2,6}) '
    ,'([0-9]{0,4}-[A-Z]\\.[A-Za-z\\-]{1,20}), '
    ,PENALTIES
    ,'([0-9]{1,3}) yards, enforced at ([A-Z]{1,4}) ([0-9]{1,2})\\. No Play\\.'
  )
  
  Cond2 <- grepl(regParse, x[,'scoreText']) & !Cond
  
  x$Penalty[Cond2] = TRUE
  x$PenaltyYards[Cond2] = gsub(regParse, '\\4', x[Cond2, 'scoreText'])
  x$PenaltyType[Cond2] = gsub(regParse, '\\3', x[Cond2, 'scoreText'])
  x$NoPlay[Cond2] = TRUE
  x$PenaltyPlayer[Cond2] = gsub(regParse, '\\2', x[Cond2, 'scoreText'])
  
  
  
  # - Penalty for declines
  regParse = paste0(
    '(.*?)'
    ,'(Penalty on |Team penalty on )([A-Z]{2,6})'
    ,'( [0-9]{1,4}-[A-Z]\\.[A-Za-z\\-]{1,20}|), '
    ,PENALTIES
    ,'declined\\.'
  )
  
  
  Cond3 <- grepl(regParse, x[,'scoreText']) & !Cond & !Cond2
  
  x$Penalty[Cond3] = TRUE
  x$PenaltyYards[Cond3] = 0
  x$PenaltyType[Cond3] = gsub(regParse, '\\5', x[Cond3, 'scoreText'])
  x$PenaltyDeclined[Cond3] = TRUE
  x$PenaltyPlayer[Cond3] = trim(gsub(regParse, '\\4', x[Cond3, 'scoreText']))
  
  x$PenaltyYards <- as.numeric(x$PenaltyYards)
  x
  
}
