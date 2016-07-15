### William Rinehart | @willrinehart | July 2016 | william.eric.rinehart@gmail.com
### load libraries
library(jsonlite)
library(lubridate)

## API get | http://elections.huffingtonpost.com/pollster/api
baseurl <- "http://elections.huffingtonpost.com/pollster/api/polls.json?topic=2016-president&after=2015-6-1"
pages <- list()
for(i in 0:30){
  mydata <- fromJSON(paste0(baseurl, "&page=", i))
  message("Retrieving page ", i)
  pages[[i+1]] <- mydata
}

### combine all into one
polls <- rbind.pages(pages)


### standard deviation calculation | http://faculty.vassar.edu/lowry/polls/poll4.html
p <- .44
n <- 1000
sd <- sqrt((p*(1-p))/n)
sd 

### monte carlo simulation 
runs <- 200000
sim <- rnorm(runs, mean=p, sd=sd)
hist(sim)
sim <- sum(sim >= 3 & sims <= 6)/runs



#### NEW LOOP ####
## API get | http://elections.huffingtonpost.com/pollster/api
baseurl <- "http://elections.huffingtonpost.com/pollster/api/polls.json?topic=2016-president&after=2015-6-1"
pages <- list()
for(i in i){
  mydata <- fromJSON(paste0(baseurl, "&page=", i))
  message("Retrieving page ", i)
  pages[[i+1]] <- mydata
}

### combine all into one
polls2 <- rbind.pages(pages)


fromJSON(paste0(baseurl, "&page="