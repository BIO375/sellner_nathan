# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("ggmosaic")
library("ggmosaic")

install.packages("epitools")
library("epitools")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


### Binomial Test #### 

# For the wilderness population
28+13
model02 <- binom.test(x= 28, n=41, p=0.5, alternative = "greater", conf.level = 0.95 )
model02

#For the Bergen op Zoom population
41+49
modelfemale <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
modelfemale



### Chi_Square Goodness of Fit ####

flower <- read_csv("~/Analyses/sellner_nathan/datasets/demos/flower.csv",
                   col_types = cols(color = col_factor()))

flower_summ <- flower %>%
  group_by(color)%>%
  summarise(color_n = n())

flower_summ <- add_column(flower_summ, expected= c(75,25)) %>%
  mutate(expected_p = expected/100)

modelflower <- chisq.test(x = flower_summ$color_n, p = flower_summ$expected_p)
modelflower

tab02 <- matrix(c(17, 30, 49, 41), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab02) <- list("Outcome" = c("Male", "No Female"),
                        "Treatment" = c("Belgium", "Holland"))

### Contingency Table Analysis ####

as.matrix(tab02)
model05 <- chisq.test(tab02, correct = FALSE)
model05
