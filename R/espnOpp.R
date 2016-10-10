espnOpp<-function(x){
  
  # -Remove vs and @
  iter1<-gsub("vs|@", "", x)
  # -Remove ranking from name
  iter2<-gg2<-gsub("[#0-9]", "", iter1)
  # -Trim whitspace
  iter3<-gsub("^\\s+|\\s+$", "", iter2)
  # -Remove asterisks
  iter4<-gsub("\\*", "", iter3)
  return(iter4)
  
}



espnDate<-function(x){
    # - Removing weekday
    noWday<-substr(x,6,20)
    # -fixing September
    fixSept<-gsub("Sept", "Sep", noWday)
    # -converting to date
    as.Date(fixSept, "%b %d %Y")
}



Numerics <- function(data){
  findNums<-function(x) apply(x,2, FUN = function(x){!all(grepl('[^0-9]', x))})

  ff<-findNums(data)
  
  if(sum(ff) == 0){
    data
  }
  if(sum(ff) == 1){
    data[,ff] <- as.numeric(data[,ff])
    data
  }else{
    convert <- function(x) apply(x, 2, as.numeric)
  
    data[,ff] <- convert(data[,ff])
  
    data
  }

}