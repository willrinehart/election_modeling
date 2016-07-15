### Wesley's code from http://www.r-bloggers.com/r-code-for-election-posterior-distribution-from-a-random-sample/

## This data was simulated during an earlier process for purposes of this example.
## The simulation is based on actual data collected from other surveys.
## "Probability-Based Estimation and the 2012 Presidential Election Exit Poll"

## Use gtools for rdirichlet()
library(gtools)

raw_txt = "Dem.pct EV size State Rep.pct
AK 40.50108 3 732 AK 55.49892
AL 38.62712 9 405 AL 60.37288
AR 40.56291 6 504 AR 59.43709
AZ 46.89617 11 859 AZ 53.10383
CA 61.49733 55 1005 CA 36.50267
CT 61.98948 7 900 CT 38.01052
DC 96.70255 3 501 DC 03.29745
DE 64.25041 3 588 DE 35.74959
FL 51.73604 29 738 FL 48.26396
GA 46.59993 16 596 GA 52.40007
HI 70.62515 4 761 HI 27.37485
IA 52.72411 6 1008 IA 46.27589
ID 39.34329 4 603 ID 60.65671
IL 63.53229 20 225 IL 36.46771
IN 49.95051 11 937 IN 50.04949
KS 37.85524 6 819 KS 62.14476
KY 39.42100 8 567 KY 60.57900
LA 40.41514 8 541 LA 57.58486
MA 64.06700 11 741 MA 35.93300
MD 68.46216 10 257 MD 31.53784
ME 60.95495 4 573 ME 39.04505
MI 59.32956 16 152 MI 40.67044
MN 56.39913 10 897 MN 43.60087
MO 46.92633 10 906 MO 53.07367
MS 48.60395 6 573 MS 51.39605
MT 42.38009 3 728 MT 55.61991
NC 48.08133 15 782 NC 49.91867
ND 39.72801 3 514 ND 60.27199
NE 32.55075 5 484 NE 67.44925
NH 56.95381 4 932 NH 43.04619
NJ 60.31767 14 388 NJ 39.68233
NM 56.23601 5 282 NM 43.76399
NV 50.96606 6 918 NV 49.03394
NY 66.55607 29 955 NY 33.44393
OH 51.46216 18 929 OH 47.53784
OK 34.13442 7 708 OK 65.86558
PA 52.38386 20 970 PA 45.61614
RI 66.38853 4 411 RI 33.61147
SC 49.62293 9 663 SC 50.37707
SD 39.74359 3 546 SD 60.25641
TN 46.22093 11 344 TN 53.77907
TX 41.57170 38 918 TX 57.42830
UT 43.70502 6 510 UT 56.29498
VA 58.95710 13 642 VA 41.04290
VT 73.74302 3 760 VT 26.25698
WI 55.49687 10 436 WI 44.50313
WV 36.40509 5 427 WV 62.59491
WY 27.88871 3 503 WY 68.11129
CO 52.02242 9 729 CO 46.97758
OR 54.95128 7 437 OR 42.04872
WA 56.7056 12 437 WA 42.2944";

raw_data = textConnection(raw_txt)
raw = read.table(raw_data, header=TRUE, comment.char="#", sep="")
close.connection(raw_data)

## Function to simulate each state outcome
p.win = function(state){
  #Dirichlet distribution because there can be multiple candidates
  p=rdirichlet(1000000,
               raw$size[state]*c(raw$Rep.pct[state],raw$Dem.pct[state],(100-raw$Rep.pct[state]-raw$Dem.pct[state]))/100+1)
  mean(p[,2]>p[,1])
}

run.simulation=function(){
  ## Binomial distribution because in all states except Nebraska and Maine it is winner takes all.
  ## In NE and ME they use the Congressional District Method. But often all votes go to the same candidate.
  ## During 2008 election was the first and last time NE split it's vote when Obama received 1 electoral vote.
  winner=rbinom(nrow(raw),1,probability.of.win)
  sum(raw$EV*winner)
}
## Iterate over the states
## This can be adjusted to further account for within state sample design rather than assuming an SRS within each state.
## Note that an exit poll style sample design is not an SRS but is a stratified, cluster design
p.win.state = sapply(1:nrow(raw),p.win)

## Renamed to perform other functions if desired
## Set Obama.win.probs for the sim.election() function
probability.of.win = p.win.state
#

## Replicate the simulation. A greater number of replicates will smooth out the distribution.
electoral.vote.simulation = replicate(500000,run.simulation())
( sim.median = median(electoral.vote.simulation) ) #Calculate the median from the simulation

## Graph it.
hist(electoral.vote.simulation, nclass=1000, main='Simulation of Electoral Vote', xlab='Electoral Votes')
credible.interval = quantile(electoral.vote.simulation, prob=c(.025, .975))
abline(v=credible.interval, col=2)
abline(v=sim.median, col=3, lwd=3)

## Also, an individual state can be examined
my.state = 'CA' ## Enter 2-letter state abbreviation
j.state = which(raw$State==my.state)

p.state=rdirichlet(100000,
                   raw$size[j.state]*
                     c(raw$Rep.pct[j.state],raw$Dem.pct[j.state],100-raw$Rep.pct[j.state]-raw$Dem.pct[j.state])/100+1)
hist(p.state[,2], nclass=1000, main=paste('Simulation for',my.state), xlab='Proportion of Vote')
median(p.state[,2])
abline(v=median(p.state[,2]), lwd=3, col=3)
abline(v=quantile(p.state[,2], prob=c(.025,.975)), col=2)