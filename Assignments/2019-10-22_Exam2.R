# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

install.packages("DescTools")
library("DescTools")



library(readr)
feathers <- read.csv("datasets/exams/feathers.csv")
view(feathers)

feathers <- mutate(feathers, diff = typical - odd)


ggplot(feathers) +
  geom_histogram(aes(diff), binwidth = 0.02)

ggplot(feathers) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(feathers)+
  geom_qq(aes(sample = diff))

summ_feathers <- feathers %>%
  summarise(mean_feather = mean(diff),
            median_feather = median(diff))

t.test(feathers$typical, feathers$odd, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)


library(readr)
baker <- read.csv("datasets/exams/baker.csv")
view(baker)

baker <- mutate(baker, diff = After - Before)


ggplot(baker) +
  geom_histogram(aes(diff), binwidth = 1)

ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(baker)+
  geom_qq(aes(sample = diff))

summ_baker <- baker %>%
  summarise(mean_baker = mean(diff),
            median_baker = median(diff))

SignTest(baker$diff, alternative = "greater", mu = 0, conf.level = 0.95)


library(readr)
alga <- read_csv("datasets/demos/alga.csv")

### CODE BREAKS HERE BC 4 ROWS WHERE TREATMENT = 1 ####

View(alga)

ggplot(alga) +
  geom_histogram(aes(growthrate), binwidth = 0.5)+
  facet_wrap(~Treatment)

ggplot(alga) +
  geom_boxplot(aes(x = Treatment, y = growthrate))

ggplot(alga)+
  geom_qq(aes(sample = growthrate, color = Treatment))

t.test(growthrate ~ Treatment, data = alga, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

### ONE CODE BREAK, 5/6 PTS ####
