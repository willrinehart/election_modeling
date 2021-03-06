### William Rinehart | @willrinehart | July 2016
### load libraries
library(jsonlite)
library(lubridate)
library(XML)
library(pollstR)
library(reshape)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)

###read in electoral college data
electoral_votes <- read.csv("electoral_college_data.csv", header = TRUE)

### API get with pollstR | https://cran.r-project.org/web/packages/pollstR/vignettes/introduction.html
polldata <- pollster_polls(topic ='2016-president', max_pages = Inf)
questions <- subset(polldata$questions, state=="US" & topic=="2016-president" & code=="16-US-Pres-GE TrumpvClinton" & name %in% c("Adults", "Likely Voters", "Registered Voters"))

### merge data
prespolldata <- merge(questions, polldata$polls, by = "id")

### calculate standard deviation
prespolldata$sd <- 100 * (sqrt((prespolldata$value/100)*(1-(prespolldata$value/100))/prespolldata$observations))

### subset data for each candidate
clintonpres <- prespolldata[grep("Clinton", prespolldata$choice),]
trumppres <- prespolldata[grep("Trump", prespolldata$choice),]
johnsonpres <- prespolldata[grep("Johnson", prespolldata$choice),]

### rename columns
clintonpres$choice <- "Clinton" 
trumppres$choice <- "Trump"
johnsonpres$choice <- "Johnson"

### calculate weights 
clintonpres$timediff <- (Sys.Date() - clintonpres$end_date)
clintonpres$weights <- 1/(as.numeric(clintonpres$timediff))
trumppres$timediff <- (Sys.Date() - trumppres$end_date)
trumppres$weights <- 1/(as.numeric(trumppres$timediff))
johnsonpres$timediff <- (Sys.Date() - johnsonpres$end_date)
johnsonpres$weights <- 1/(as.numeric(johnsonpres$timediff))

### calculate weighted mean and standard deviation
clintonwtdmean <- wtd.mean(clintonpres$value, clintonpres$weights)
clintonwtdvar <- wtd.var(clintonpres$value, clintonpres$weights)
clintonwtdsd <- sqrt(clintonwtdvar)
trumpwtdmean <- wtd.mean(trumppres$value, trumppres$weights)
trumpwtdvar <- wtd.var(trumppres$value, trumppres$weights)
trumpwtdsd <- sqrt(trumpwtdvar)
johnsonwtdmean <- wtd.mean(johnsonpres$value, johnsonpres$weights)
johnsonwtdvar <- wtd.var(johnsonpres$value, johnsonpres$weights)
johnsonwtdsd <- sqrt(johnsonwtdvar)

### combine polls
finalpolls <- rbind(clintonpres, trumppres, johnsonpres)

### map with ggplot2 | Editing
ggplot(data = finalpolls, aes(x = end_date, y = value, group=choice, color = choice)) +
  geom_point(shape = 1) +
  geom_smooth(method = "loess", size = 1.5) +
  scale_color_manual(values = c("Clinton" = "blue", "Trump" = "red", "Johnson" = "yellow")) + 
  labs(title = "Average of All Polls", x = "Date of Poll", y = "Percent Supporting Candidate", color = "Candidate")

### state level creation 
state_data <- subset(polldata$questions, topic=="2016-president" & name %in% c("Adults", "Likely Voters", "Registered Voters"))
state_data <- state_data[!state_data$state=="US", ]
state_data <- na.omit(state_data)

state_data <- merge(state_data, polldata$polls, by = "id")
table(state_data$state)