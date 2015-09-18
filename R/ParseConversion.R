#' @title Conversions
#' 
#' 
#' 
#' @export



ParseConversion <- function(x){
  
  x$scoreText <- as.character(x$scoreText)
  
  regParse <- 
    paste0(
      '(.*?)'
      ," Conversion is good\\."
    )
  
  x$Conversion = grepl(regParse, x$scoreText)
  
  x$scoreText[x$Conversion] = gsub(regParse, '\\1', x$scoreText[x$Conversion])
  
  x
  
}