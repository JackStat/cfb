#' @title Numerics
#' 
#' @description Convert miss classed numerics vector from factor/character to numeric
#' 
#' @param data dataframe with potential numerics to convert.
#' 
#' 
#' @export


Numerics <- function(data){
    
  findNums <- function(x) apply(x,2, FUN = function(x){!all(grepl('[^0-9]', x))})
  
  ff <- findNums(data)
  
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

