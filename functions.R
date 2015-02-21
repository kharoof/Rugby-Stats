library(lubridate)
####################################################################################################
## Functions that clean the results for a match
####################################################################################################

## Format dates, add team and opponent fields and pts diff win lose etc...
cleanResults <- function(data, team){
  data = cbind(data,data.frame(matrix(as.numeric(unlist(strsplit(data$Score, "-|v"))), ncol=2, byrow=T)))
  names(data) <- c("date", "time.local", "home.team", "away.team", "score", "venue","url", "home.score", "away.score")
  data$date = as.Date(data$date, "%d/%m/%Y")
  data$opponent <- ifelse(data$home.team==team, data$away.team, data$home.team)
  data$home.away <- ifelse(data$home.team==team, "Home", "Away")
  data$pts.diff <- ifelse(data$home.team==team, data$home.score - data$away.score,data$away.score - data$home.score)
  data$year <- as.ordered(year(data$date))
  data$month <- as.ordered(month(data$date))
  data$win.lose <- as.factor(ifelse(data$pts.diff>0 , "Win",ifelse(data$pts.diff<0 , "Lose","Draw" ) ))
  data$team <- team
  keep = c("date", "year", "month","time.local", "team", "opponent", "pts.diff", "home.away", "win.lose", "url")
  data <- data[,keep]
  return(data)
}




