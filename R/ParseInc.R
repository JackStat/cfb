#' @title Parse incomplete passes from NCAA scrape
#' 
#' 
#' 
#' @export



ParseInc <- function(x){
  # - Incompletes
  Cond <- grepl('incomplete. Intended', x$scoreText) & !grepl('Penalty', x$scoreText) & !grepl('INTERCEPTED', x$scoreText)
  x$Passer[Cond] = gsub('([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) (incomplete. Intended for) ([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20})\\.', '\\1', x[Cond,'scoreText'])
  x$Incomplete = FALSE
  x$Incomplete[Cond] = TRUE
  
  x$PassAtt = FALSE
  x$PassAtt[Cond] = TRUE
  
  x$IntendedFor[Cond] = gsub('([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20}) (incomplete. Intended for) ([0-9]{1,4}-[A-Z]\\.[A-Za-z]{1,20})\\.', '\\3', x[Cond,'scoreText'])
  x  
}
