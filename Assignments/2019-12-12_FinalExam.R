rm(list = ls())

library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

### Scenario 1 ####
library(readr)
insulation <- read_csv("datasets/final/insulation.csv")
View(insulation)

insulation01 <- lm(heat_loss ~ leanness, data = insulation)
autoplot(insulation01, smooth.colour = NA)

ggplot(data = insulation01)+
  geom_point(aes(x = leanness, y= resid(insulation01)))

summary(insulation01)

### Scenario 2 ####

### Scenario 3 ####

library(readr)
davis <- read_csv("datasets/final/davis.csv")
View(davis)


race <- tribble(
  ~race, ~obs_freq, ~exp_prop,
  #--|--|----
  "aab", 1096, 0.030,
  "aian", 231, 0.018,
  "api", 9967, 0.156,
  "hl", 6740, 0.319,
  "w", 7415, 0.423,
  "tmu", 457, 0.054
)

race
modelrace <-chisq.test(x = race$obs_freq, p = race$exp_prop)
modelrace
modelrace$expected
