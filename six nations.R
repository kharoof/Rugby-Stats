#Get the Six Nations Results since 1882

results <- data.frame(stringsAsFactors=F)
#Retrieve data from website
for(j in 2013:2014) {
  url = paste0('http://www.rbs6nations.com/en/matchcentre/index.php?includeref=428&season=',j,'-',j+1)
  results <- rbind(results,getResults(url))
}

results <- cleanResults(results, "Ireland")
