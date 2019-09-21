
# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()



library(readr)
chap13e5SagebrushCrickets <- read_csv("Analyses/sellner_nathan/datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")
# This is the correct cochap13e5SagebrushCrickets <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")
# NATHAN" your file path for read_csv needs to start with datasets
# Currently the code is broken for me, because you stepped outside the shared repo or have some weird working
# directory or something.
# I am guessing that you did not open your project by clicking on Template.Rproj.  You must do this in the 
# future or your code will continue to break.
View(chap13e5SagebrushCrickets)


chap13e5SagebrushCricketsanchez.csv<-mutate(chap13e5SagebrushCrickets,log_feedingStatus = log(timeToMating+1))

ggplot(chap13e5SagebrushCricketsanchez.csv) +
  geom_histogram(aes(timeToMating), binwidth = 10)+ facet_wrap(~feedingStatus)
ggplot(chap13e5SagebrushCricketsanchez.csv) +
  geom_histogram(aes(log(timeToMating+1)), binwidth = 0.5)+ facet_wrap(~feedingStatus)
