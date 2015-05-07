#' @title Parse Drive Details from NCAA scrape
#' 
#' @export



ParseDrive <- function(x){
  # - Drive Details
  regParse ='(1st|2nd|3rd|4th) and ([0-9]{1,2}) at ([A-Z]{2,4})([0-9]{1,2})'
  x$Down = as.numeric(gsub('[^0-9]','', gsub(regParse,'\\1', x$driveText)))
  x$YdsToGo = as.numeric(gsub(regParse,'\\2', x$driveText))
  x$FieldPos = gsub(regParse,'\\3', x$driveText)
  x$Yardline = as.numeric(gsub(regParse,'\\4', x$driveText))

  x
}