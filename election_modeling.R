### William Rinehart | @willrinehart | July 2016 | william.eric.rinehart@gmail.com
### load libraries
library(jsonlite)
library(lubridate)
library(XML)
library(pollstR)
library(reshape)
library(ggplot2)

### API get with pollstR | https://cran.r-project.org/web/packages/pollstR/vignettes/introduction.html
polldata <- pollstr_polls(max_pages = 10000, after = "2016-01-01")
questions <- subset(polldata$questions, state=="US"
                    & topic=="2016-president"  
                    & subpopulation %in% c("Adults", "Likely Voters", "Registered Voters"))

### merge data
prespolldata <- merge(questions, polldata$polls, by = "id")

### standard deviation calculation
prespolldata$sd <- 100 * (sqrt((prespolldata$value/100)*(1-(prespolldata$value/100))/prespolldata$observations))

### subset data for each candidate
clintonpres <- subset(prespolldata, choice=="Clinton")
trumppres <- subset(prespolldata, choice=="Trump")
other <- subset(prespolldata, choice=="Trump")


### load libraries
library(jsonlite)
library(lubridate)
library(XML)
library(pollstR)

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

questions <- xmlToList(polls$questions)


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



#### NEW CODE ####
library(XML)
library(reshape)
library(ggplot2)

### API get with pollstR | https://cran.r-project.org/web/packages/pollstR/vignettes/introduction.html
polldata <- pollstr_polls(max_pages = 10000, after = "2016-1-1")
questions <- subset(polldata$questions, state=="US" 
                    & topic=="2016-president" 
                    & subpopulation %in% c("Adults", "Likely Voters", "Registered Voters"))

prespolldata <- merge(polldata$polls, questions, by = "id")

### standard deviation calculation
questions$sd <- 100 * (sqrt((questions$value/100)*(1-(questions$value/100))/questions$observations))


dat <- pollstR(pages=20)
ggplot(dat,aes(end.date,Obama/(Obama+Romney)))+geom_point(alpha=.5)+geom_smooth(aes(weight=sqrt(N)))+geom_hline(aes(yintercept=0.5),lty=2,size=1)+
  labs(title="Proportion of Vote for Obama",x="Last Date of Poll",y=NULL)
