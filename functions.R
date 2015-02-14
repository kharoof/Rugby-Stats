##Convert first letter of each word to upper case
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#Function to get results from a webpage
#Using Xpath and the XML library is one way but the htmltab package should eb more general
getResults <- function(url){
  #Get HTML table containing the results
  #On the rbs website the results are stored in a table with class list but there is a spanning column that contains
  #the word round which we exclude
  library(htmltab)
  results <- htmltab(doc=url, which="//td[@class='list']", body="//tr[not(@class = 'group')]")
  #Choose which data to keep
  keep = c("Date", "Time (Local)", "Home", "Away", "Score", "Venue")
  results <- results[-1,keep]
  row.names(results) <- NULL
  return(results)
}

#Clean the results
#Format dates, add team and opponent fields and pts diff win lose etc...
cleanResults <- function(data, team){
  data = cbind(data,data.frame(matrix(as.numeric(unlist(strsplit(data$Score, "-|v"))), ncol=2, byrow=T)))
  names(data) <- c("date", "time.local", "home.team", "away.team", "score", "venue","home.score", "away.score")
  data$date = as.Date(data$date, "%d/%m/%Y")
  data$opponent <- ifelse(data$home.team==team, data$away.team, data$home.team)
  data$home.away <- ifelse(data$home.team==team, "Home", "Away")
  data$pts.diff <- ifelse(data$home.team==team, data$home.score - data$away.score,data$away.score - data$home.score)
  library(lubridate)
  data$year <- as.ordered(year(data$date))
  data$month <- as.ordered(month(data$date))
  data$win.lose <- as.factor(ifelse(data$pts.diff>0 , "Win",ifelse(data$pts.diff<0 , "Lose","Draw" ) ))
  data$team <- team
  keep = c("date", "year", "month","time.local", "team", "opponent", "pts.diff", "home.away", "win.lose")
  data <- data[,keep]
  return(data)
}



#Test functions with real data
url = 'http://www.rbs6nations.com/en/ireland/ireland_matchcentre.php?includeref=436&season=2010-2011'
result <- getResults(url)

cleanResults(result, "Ireland")

