#' @title Clean Player Names
#' 
#' 
#' @export



cleanPlayers <- function(x){
  
  # Players with spaces in last name
  x$scoreText <- gsub("([0-9]{0,3}-[A-Z]{1,1}\\.[A-Z]{1,1}[a-z]{1,20})(\\ |\\-)([A-Z]{1,1}[a-z]{1,20})",'\\1-\\3', x$scoreText)
  x$scoreText <- gsub("([0-9]{0,3}-[A-Z]{1,1}\\.) ([A-Z]{1,1}[a-z]{1,20})",'\\1\\2', x$scoreText)
  
  # Players with period in name...
  x$scoreText <- gsub("([0-9]{0,3}-[A-Z]{1,1}\\.[A-Z]{1,1}[a-z]{1,20})(\\ |\\-)([A-Z]{1,1}[a-z]{1,20})(\\.)( to| from)",'\\1-\\3\\5', x$scoreText)
  x$scoreText <- gsub("([0-9]{0,3}-[A-Z]{1,1}\\.[A-Z]{1,1}[a-z]{1,3})\\.([A-Z]{1,1}[a-z]{1,20})",'\\1-\\2', x$scoreText)
  
  # Players with single qoute in name...
  x$scoreText <- gsub("([0-9]{0,3}-[A-Z]{1,1}\\.[A-Z]{1,1})'([A-Z]{1,1}[a-z]{1,20})",'\\1-\\2', x$scoreText)

  x
  
}