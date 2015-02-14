library(XML)
library(lubridate)

rm(list=ls())

results <- data.frame(stringsAsFactors=F)



getData <- function(country) {
country <- tolower(country)
#Retrieve data from website
if (country == "england") { code = 432}
if (country == "france") { code = 434}
if (country == "ireland") { code = 436}
if (country == "italy") { code = 438}
if (country == "scotland") { code = 440}
if (country == "wales") { code = 442}

for(j in 2000:2014) {
#theurl <- paste0('http://www.rbs6nations.com/en/wales/wales_matchcentre.php?includeref=442&season=2010-2011')
if (country=="wales") {
  theurl <- paste0('http://www.rbs6nations.com/en/',country,'/',country,'_match_centre.php?includeref=',code,'&season=',j,"-",j+1)
}
else {
  theurl <- paste0('http://www.rbs6nations.com/en/',country,'/',country,'_matchcentre.php?includeref=',code,'&season=',j,"-",j+1)
}
html3 <- htmlTreeParse(theurl, useInternalNodes = T)
date = xpathSApply(html3, "//td[@class='field_DateDmyLong']", xmlValue)
date = as.Date(date, "%d/%m/%Y")
time = xpathSApply(html3, "//td[@class='field_TimeLong']", xmlValue)
home_team = xpathSApply(html3, "//td[@class='field_HomeDisplay']", xmlValue)
score = xpathSApply(html3, "//td[@class='field_Score']", xmlValue)
score = data.frame(matrix(as.numeric(unlist(strsplit(score, "-|v"))), ncol=2, byrow=T))
names(score) <- c("home_score", "away_score")
away_team = xpathSApply(html3, "//td[@class='field_AwayDisplay']", xmlValue)
season = paste0(j,"-",j+1)
team <- capwords(country)
#Clean data and add useful fields
opponent <- ifelse(home_team==team, away_team, home_team)
location <- ifelse(home_team==team, "Home", "Away")
pts_diff <- ifelse(home_team==team, score$home_score - score$away_score,score$away_score - score$home_score)

year <- as.ordered(year(date))
result <- as.factor(ifelse(pts_diff>0 , "Win",ifelse(pts_diff<0 , "Lose","Draw" ) ))

results <- rbind(results, data.frame(cbind(season, year, time, team, opponent, pts_diff, location, result, score, stringsAsFactors=F)))
}



results <- results[complete.cases(results),]
}


france = getData("France")
england = getData("England")
ireland = getData("Ireland")
wales = getData("Wales")
scotland = getData("Scotland")
italy = getData("Italy")

results <- rbind(france, england, ireland, scotland, italy, wales)
