

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
install.packages("nlme")
library("nlme")
# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
install.packages("broom")
library("broom")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


### Exam 3 #9 ####

library(readr)
bacteria <- read_csv("datasets/exams/bacteria.csv")
View(bacteria)

###   #10  ####



library(readr)
aphids <- read_csv("datasets/exams/aphids.csv")
View(aphids)


model02 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = aphids)


model02_varcomp <- VarCorr(model02)
model02_varcomp

varAmong  <- as.numeric( model02_varcomp[1,1] )


varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability


###   #11   ####
library(readr)
glucose <- read_csv("datasets/exams/glucose.csv")
View(glucose)

glucoseCor <- cor.test(~ blood_glucose + HbA1c, data = glucose,
                     method = "pearson")
glucoseCor


###   #12   ####

library(readr)
driver <- read_csv("datasets/exams/DriverVision.csv")
View(driver)


model01 <- lm(Distance ~ Age, data = driver)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model01, smooth.colour = NA)

driver_plus <- augment(model01)
ggplot(data = driver_plus)+
  geom_point(aes(x = Age, y= .resid))

driver <- driver %>%
  mutate(sqrt_distance = sqrt(Distance))
model03<-lm(sqrt_distance ~ Age, data = driver)
ggplot(data = driver)+
  geom_point(aes(x = Age, y= resid(model03)))

summary(model03)


#### Code runs perfectly 5/5 ####