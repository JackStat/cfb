#' @title Find and Parse Field Goal
#' 
#' 
#' @export


ParseFG <- function(x){
  # - Field Goals
  regParse = '([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) ([0-9]{1,3}) yards Field Goal is (Good|No Good)\\.'
  Cond <- grepl(regParse, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"])
  x$FieldGoal = FALSE
  x$FieldGoal[Cond] = TRUE
  
  x$Kicker[Cond] = gsub(regParse, '\\1', x[Cond,"scoreText"])
  x$FGYards[Cond] = gsub(regParse, '\\2', x[Cond,"scoreText"])
  x$FGGood[Cond] = gsub(regParse, '\\3', x[Cond,"scoreText"]) == "Good"

  # - Extra Points
  regParse2 = '([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) extra point is (Good|No Good|good|no good)\\.'
  Cond2 <- grepl(regParse2, x[,"scoreText"]) & !grepl('Penalty', x[,"scoreText"]) & !Cond
  x$FieldGoal[Cond2] = TRUE  
  x$ExtraPt = FALSE
  x$ExtraPt[Cond2] = TRUE
  
  x$Kicker[Cond2] = gsub(regParse2, '\\1', x[Cond2,"scoreText"])
  x$FGYards[Cond2] = 20
  x$FGGood[Cond2] = grepl("Good|good", gsub(regParse2, '\\2', x[Cond2,"scoreText"]))
  
  x$FGYards = as.numeric(x$FGYards)
  x
}
