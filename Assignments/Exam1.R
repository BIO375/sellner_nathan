
# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()



library(readr)
chap13e5SagebrushCrickets <- read_csv("Analyses/sellner_nathan/datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")
View(chap13e5SagebrushCrickets)


chap13e5SagebrushCricketsanchez.csv<-mutate(chap13e5SagebrushCrickets,log_feedingStatus = log(timeToMating+1))

ggplot(chap13e5SagebrushCricketsanchez.csv) +
  geom_histogram(aes(timeToMating), binwidth = 10)+ facet_wrap(~feedingStatus)
ggplot(chap13e5SagebrushCricketsanchez.csv) +
  geom_histogram(aes(log(timeToMating+1)), binwidth = 0.5)+ facet_wrap(~feedingStatus)
