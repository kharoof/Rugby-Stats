####################################################################################################
## Get the Six Nations Results since 1990
####################################################################################################
## Copy the code from the Scrape data repository
library(XML)
library(htmltab)

##Function that uses the htmltab function to get results that are stored in tables
##This extracts the info of interest from each webpage
parseSixNationsResults <- function(url_html){
  ##On the rbs website the results are stored in a table with class list but there is a spanning column that contains
  ##the word round which we exclude
  results <- htmltab(doc=url_html, which="//td[@class='list']", body="//tr[not(@class = 'group')]")
  ##Choose which data to keep
  keep = c("Date", "Time (Local)", "Home", "Away", "Score", "Venue")
  results <- results[-1,keep]
  results$url <- docName(url_html)
  row.names(results) <- NULL
  return(results)
}

#########################
## Ireland specific results
#########################
## Use the XML package to return the html from each page and store this as a list which we will later parse (this will take a long time)
raw_html_data <- lapply(1990:2014, function(season) htmlTreeParse(paste0('http://www.rbs6nations.com/en/ireland/ireland_matchcentre.php?includeref=436&season=',season,"-",season+1),useInternalNodes=T))

## Extract from the raw html the data that we wish to keep, This will be cleaned later
ireland <- do.call(rbind,lapply(raw_html_data, parseSixNationsResults))

## Use a custom function to clean the data
## The general methodology of this funciton to specify a home team, calc pts difference etc is
## useful and can be reused
source("functions.R")
results <- cleanResults(ireland, "Ireland")

####################################################################################################
## Analysis
####################################################################################################
library(ggplot2)
library(caret)
## Start with some visual analysis

## The data appears to be skewed to losing as shown in the following histogram
ggplot(results, aes( pts.diff))+geom_histogram()

## Boxplot of the pts difference by year
ggplot(results, aes(factor(year), pts.diff)) + geom_boxplot() + scale_x_discrete(breaks=c(seq(1995,2015, 4)))

## Add a facet_wrap
ggplot(results, aes(factor(year), pts.diff)) + geom_point() + scale_x_discrete(breaks=c(seq(1995,2015, 4))) + facet_wrap(~opponent)
ggplot(results, aes(factor(year), pts.diff)) + geom_point(aes(shape=home.away,colour=win.lose)) + scale_x_discrete(breaks=c(seq(1995,2015, 4))) + facet_wrap(~opponent)
ggplot(results, aes(factor(year), pts.diff)) + geom_point(aes(colour=win.lose)) + scale_x_discrete(breaks=c(seq(1995,2015, 4))) + facet_wrap(~opponent)
ggplot(results, aes(factor(year), pts.diff)) + geom_point(aes(shape=home.away,colour=win.lose)) + scale_x_discrete(breaks=c(seq(1995,2015, 8))) + facet_wrap(~opponent)
ggplot(results[results$year>=2005,], aes(x=year,y=pts.diff)) + facet_wrap(~win.lose, ncol=1)+geom_boxplot()
ggplot(results[results$year>=2005,], aes(x=year,y=pts.diff, colour=win.lose)) + facet_wrap(~win.lose, ncol=1)+geom_point()+geom_jitter()

## Split the data into training and testing
results$year <- as.numeric(as.character(results$year))
train <- results[results$year %in% 2000:2014,]
testing <- results[results$year %in% 2009:2010,]
predict <- results[results$year %in% 2015,]

## Start by fitting a simple linear model
simple_lm <- lm(pts.diff~home.away, results)
plot(1:nrow(results),results$pts.diff)
lines(1:nrow(results),predict(simple_lm,results))
## Clearly a simple linear model is a terrible fit but lets look graphically at the results

## Add some extra factors to the model
simple_lm <- lm(pts.diff~home.away+opponent, results)
plot(1:nrow(results),results$pts.diff)
lines(1:nrow(results),predict(simple_lm,results))

## Add some interactions (this looks better)
simple_lm <- lm(pts.diff~home.away:opponent + year, results)
plot(1:nrow(results),results$pts.diff)
lines(1:nrow(results),predict(simple_lm,results))

## Adding some interactions makes the model look better, lets fit a training and testing model

rugby_model <- lm(pts.diff~home.away:opponent + year, train)

## Check the fitting of the model
table(testing$win.lose)
table(testing$win.lose,ifelse(predict(rugby_model,testing)>0, "Win", "Lose"))

## Fit the model for 2015
table(predict$opponent,ifelse(predict(rugby_model,predict)>0, "Win", "Lose"))

## Fitting a model to points differene does not seem to be producing robust results
## We could try ranking the teams each year using the IRB rankings for each year and fitting a
## linear model to the score for each team based on the opponent and the irish rankings

rankings <- read.csv("irb rankings.csv", stringsAsFactors=F)
rankings$year <- year(rankings$date)
rankings$opponent <- gsub(" ", "", rankings$opponent)

model_data <- results[results$year >= 2011,]
model_data <- merge(model_data, rankings, by=c("date", "opponent"))


## Add some interactions (this looks better)
ireland_lm <- step(lm(team.score ~ home.away + opponent + ireland_ranking + opponent_ranking, model_data))
opponent_lm <- step(lm(opponent.score ~ home.away + opponent + ireland_ranking + opponent_ranking, model_data))

layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))

plot(1:nrow(model_data),model_data$team.score, col=model_data$opponent)
lines(1:nrow(model_data),predict(ireland_lm,model_data))
legend('topright', levels(model_data$opponent), lty=1, col=1:5)

plot(1:nrow(model_data),model_data$opponent.score, col=model_data$opponent, add=T)
lines(1:nrow(model_data),predict(opponent_lm,model_data))
legend('topright', levels(model_data$opponent), lty=1, col=1:5)


pts.diff <- predict(ireland_lm,model_data)-predict(opponent_lm,model_data)
win.lose <- ifelse(pts.diff > 0, "Win", "Lose")

table(win.lose, model_data$win.lose)

plot(1:nrow(model_data),model_data$opponent.score)
lines(1:nrow(model_data),predict(simple_lm,model_data))





