### William Rinehart | @willrinehart | July 2016 | william.eric.rinehart@gmail.com
### load libraries
library(jsonlite)
library(lubridate)

## API get | http://elections.huffingtonpost.com/pollster/api
url <- paste0("http://elections.huffingtonpost.com/pollster/api/polls.json?topic=2016-president&after=2015-6-1&page=", page)
page <- 1
huffpolls <- fromJSON(url)
df <- as.data.frame(huffpolls)
huffrows <- nrow(huffpolls) 

if (huffrows <= 10) {
  page = (page + 1)
  print(page)
}

url <- paste0("http://elections.huffingtonpost.com/pollster/api/polls.json?topic=2016-president&after=2015-6-1&page=", page)
huffpolls <- fromJSON(url)
df2 <- as.data.frame(huffpolls)
df <- rbind(df, df2)
huffrows <- nrow(huffpolls)  

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



