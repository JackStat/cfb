#' @title Create Timestamp from ESPN page
#' 
#' 
#' @export


makeTime <- function(time) {
  
  strptime(gsub(' AM| PM| ET,', '', time), "%H:%M %B %d, %Y", tz = 'EST')
  
}
