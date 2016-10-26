#' @title Scrape NCAA boxscore json files.
#' 
#' @import foreach
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @importFrom tidyr separate_rows
#' 
#' @examples NCAAboxscore("http://data.ncaa.com/jsonp/game/football/fbs/2016/10/15/utah-oregon-st/boxscore.json")
#' 
#' @export

NCAAboxscore <- function(url){
  
  raw <- suppressWarnings(readLines(url))

  rawfromJSON <- fromJSON((gsub('callbackWrapper\\(','',gsub('\\}\\)\\;','\\}',raw))))
  
  meta <- rawfromJSON$meta$teams
  
  info <- data.frame(
    tableId = rawfromJSON$tables$id
    ,headerColor = rawfromJSON$tables$headerColor
    
    ,stringsAsFactors = FALSE
    ) %>%
    left_join(meta, by = c('headerColor' = 'color')) %>%
    mutate(
      home = homeTeam == 'true'
      ,homeTeam = shortname[home][1]
      ,awayTeam = shortname[!home][1]
      ,homeColor = headerColor[home][1]
      ,awayColor = headerColor[!home][1]
    ) %>%
    select(
      -shortname
      ,-seoName
      ,-sixCharAbbr
      ,-headerColor
    )
  
  tableheader <- lapply(rawfromJSON$tables$header, function(x){x$display})
  
  tableRows <- sapply(rawfromJSON$tables$data, function(x){nrow(x)})
  
  ff <- foreach(i = 1:length(rawfromJSON$tables$data), .combine = 'rbind') %do% {
    
    ins <- foreach(k = 1:length(rawfromJSON$tables$data[[i]]$row), .combine = 'rbind') %do% {
      
      dd <- rawfromJSON$tables$data[[i]]$row[[k]]
      if(is.null(dd)){
        NULL
      } else {
        playerL <- dd$class == 'playerName' & !is.na(dd$class)
  
        data.frame(
          player = trim(dd$display[playerL][1])
          ,type = tableheader[[i]][1]
          ,stat = tableheader[[i]][2:length(tableheader[[i]])]
          ,value = dd$display[!playerL]
          
          ,stringsAsFactors = FALSE
        )
      }
    }
    
    if(is.null(ins)){
      NULL
    } else {
      data.frame(
        info[i,]
        ,ins
        , row.names = 1:nrow(ins)
      )
    }
  }
  
  # cleaning up the combined stats
  combined <- ff %>% filter(grepl("\\-", stat))
  notCombined <- ff %>% filter(!grepl("\\-", stat))
  
  Main <- rbind(
    notCombined
    ,separate_rows(combined, stat, value, convert = TRUE)
  ) %>%
    filter(!(grepl("\\-", value) | value == ''))
  
  Numerics(Main)
}


