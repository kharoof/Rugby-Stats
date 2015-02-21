library(ggplot2)

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

## Start with some visual analyis
p <- ggplot(results, aes(factor(year), pts.diff))
## Boxplot of the pts difference by year
p + geom_boxplot() + scale_x_discrete(breaks=c(seq(1995,2015, 4)))
## Add a facet_wrap
p + geom_point() + scale_x_discrete(breaks=c(seq(1995,2015, 4))) + facet_wrap(~opponent)
p + geom_point(aes(colour=win.lose)) + scale_x_discrete(breaks=c(seq(1995,2015, 4))) + facet_wrap(~opponent)
