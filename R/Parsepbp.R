# 
# 
# 
# url = 'http://data.ncaa.com/jsonp/game/football/fbs/2014/09/11/louisiana-tech-north-texas/pbp.json'
# url = 'http://data.ncaa.com/jsonp/game/football/fbs/2014/12/20/nevada-la-lafayette/pbp.json'
# url = 'http://data.ncaa.com/jsonp/game/football/fbs/2014/12/20/utah-colorado-st/pbp.json'
# 
# 
# 
# nn <- NCAApbp(url)
# 
# # - Drive Details
#   nn$Down = as.numeric(gsub('[^0-9]','', gsub('(1st|2nd|3rd|4th) and ([0-9]|[0-9][0-9]) at ([A-Z][A-Z][A-Z])([0-9][0-9]|[0-9])','\\1', nn$driveText)))
#   nn$YdsToGo = as.numeric(gsub('(1st|2nd|3rd|4th) and ([0-9]|[0-9][0-9]) at ([A-Z][A-Z][A-Z])([0-9][0-9]|[0-9])','\\2', nn$driveText))
#   nn$FieldPos = gsub('(1st|2nd|3rd|4th) and ([0-9]|[0-9][0-9]) at ([A-Z][A-Z][A-Z])([0-9][0-9]|[0-9])','\\3', nn$driveText)
#   nn$Yardline = as.numeric(gsub('(1st|2nd|3rd|4th) and ([0-9]|[0-9][0-9]) at ([A-Z][A-Z][A-Z])([0-9][0-9]|[0-9])','\\4', nn$driveText))
# 
# # - Incompletes
#   Cond <- grepl('incomplete. Intended', nn[,1]) & !grepl('Penalty', nn[,1])
#   nn$Passer[Cond] = gsub('([^,]*) (incomplete. Intended for) ([^,]*)', '\\1', nn[Cond,1])
#   nn$Incomplete[Cond] = TRUE
#   nn$Incomplete[!Cond] = FALSE
#   nn$IntendedFor[Cond] = gsub('([^,]*) (incomplete. Intended for) ([^,]*)', '\\3', nn[Cond,1])
# 
# # - Passes
#   Cond <- grepl('([^,]*) to ([A-Z]{3,}) [0-9]{1,3} for [0-9]{1,3} yards \\(([^,]*)\\)', nn[,1])
# #   nn$Passer[Cond] = gsub('([^,]*) (incomplete. Intended for) ([^,]*)', '\\1', nn[Cond,1])
# #   nn$Incomplete[Cond] = TRUE
# #   nn$Incomplete[!Cond] = FALSE
# #   nn$IntendedFor[Cond] = gsub('([^,]*) (incomplete. Intended for) ([^,]*)', '\\3', nn[Cond,1])
# 
# 
# 
# # 15-E.McGuire to NEV 18 for 9 yards (26-K.Johnson).
# 
# 
# # - Field Goals
# ParseText <- '([0-9][0-9][0-9]-[A-Z\\.][^,]*|[0-9][0-9]-[A-Z\\.][^,]*|[0-9]-[A-Z\\.][^,]*) ([0-9][0-9]|[0-9]) yards Field Goal is (Good.|No Good.)'
#   Cond <- grepl('Field Goal', nn[,1]) & !grepl('Penalty', nn[,1])
#   nn$FieldGoal[Cond] = TRUE
#   nn$FieldGoal[!Cond] = FALSE
#   nn$Kicker[Cond] = gsub(ParseText, '\\1', nn[Cond,1])
#   nn$FGYards[Cond] = gsub(ParseText, '\\2', nn[Cond,1])
#   nn$FGGood[Cond] = gsub(ParseText, '\\3', nn[Cond,1]) == "Good."
#   
# # - Kickoffs
# TouchbackP <- '([0-9][0-9][0-9]-[A-Z\\.][^,]*|[0-9][0-9]-[A-Z\\.][^,]*|[0-9]-[A-Z\\.][^,]*) kicks ([0-9][0-9]|[0-9]) yards from ([A-Z]{3,}) ([0-9][0-9]|[0-9])(.|) to (|[A-Z]{3,}) ( |End Zone.) touchback.'
# KickoffP <- '([^,]*) kicks ([0-9][0-9]|[0-9]) yards from ([A-Z]{3,}) ([0-9][0-9]|[0-9])\\. ([^,]*) to ([A-Z]{3,}) ([0-9][0-9]|[0-9]) for (-?[0-9]{1,3}) yards.'
# MultiPlayer <- '([0-9][0-9][0-9]-[A-Z\\.][^,]*|[0-9][0-9]-[A-Z\\.][^,]*|[0-9]-[A-Z\\.][^,]*){2,}'
# 
#   Cond <- grepl('kicks', nn[,1])
#   nn$Kickoff[Cond] = TRUE
#   nn$Kickoff[!Cond] = FALSE
# 
#   nn$Kicker[grepl(TouchbackP, nn[,1])] = gsub(TouchbackP, '\\1', nn[grepl(TouchbackP, nn[,1]),1])
#   nn$Kicker[grepl(KickoffP, nn[,1])] = gsub(KickoffP, '\\1', nn[grepl(KickoffP, nn[,1]),1])
# 
#   nn$KickYards[grepl(TouchbackP, nn[,1])] = as.numeric(gsub(TouchbackP, '\\2', nn[grepl(TouchbackP, nn[,1]),1]))
#   nn$KickYards[grepl(KickoffP, nn[,1])] = as.numeric(gsub(KickoffP, '\\2', nn[grepl(KickoffP, nn[,1]),1]))
# 
#  
# 
