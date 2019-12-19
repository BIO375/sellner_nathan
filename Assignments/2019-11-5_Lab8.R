#### Lab 8: 1-way ANOVA, continued #### 
# For this lab you will use the datasets described in Chapter 15 of your book but you will 
# answer the slightly modified questions that I provide below

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

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Problem 15-22 ####
# Complete parts a, b, c, d

## a ##
library(readr)
sticks <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", 
                   col_types = cols(specimen = col_factor(levels = c("1", 
                                                                     "2", "3", "4", "5", "6", "7", "8", 
                                                                     "9", "10", "11", "12", "13", "14", 
                                                                     "15", "16", "17", "18", "19", "20", 
                                                                     "21", "22", "23", "24", "25"))))
View(sticks)


modelsticks <- lme(fixed = headwidth ~ 1,
                   
               random = ~1|specimen, data = sticks)

modelsticks_varcomp <- VarCorr(modelsticks)
modelsticks_varcomp

## b ##

#variance within groups = 0.000166

## c ##

varAmong  <- as.numeric( modelsticks_varcomp[1,1] )

varWithin <- as.numeric( modelsticks_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability

# The repeatability of the head-width measurements is 59.7%

## d ##

# The repeatability of the femur length measurement experiement was 74% and the repeatability
# of the head width measurement experiment was only 60% so the femur length had higher repeatability
# and the head width experiment had a higher measurement error.


#### Problem 15-23 ####
# Complete parts a and c only

## a ##

# Planned comparison because the main interest of the study was determined before the data was collected 
# and so they were particularly focused on the two specific areas: island of pines with squirrels and island 
# of pines without squirrels.
# Unfortunately my machine is broken and is incapable of running the commands of multicomp and so I cannot do the test at this time: 2:13pm 11/5/19


library(readr)
pines <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv")
View(pines)

modelpines <- lm(conemass~habitat, data = pines)

modelpines <- aov(conemass ~ habitat, pines)
TukeyHSD(modelpines)

 

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

library(readr)
venom <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv")
View(venom)

summ_venom <- venom %>%
  group_by(treatmentGroup) %>%
  summarise(mean_venom = mean(logSporozoiteNumbers),
            median_venom = median(logSporozoiteNumbers),
            var_venom = var(logSporozoiteNumbers),
            sd_venom = sd(logSporozoiteNumbers))

ggplot(venom, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(venom) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 0.5)+
  facet_wrap(~treatmentGroup)
ggplot(venom)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

modelvenom <- lm(logSporozoiteNumbers~treatmentGroup, data = venom)

autoplot(modelvenom)

anova(modelvenom)

modelvenom <- aov(logSporozoiteNumbers ~ treatmentGroup, venom)
TukeyHSD(modelvenom)

# there is a significant difference between the means of the 3 groups of experimental mosquitos. Therefore we would reject the null.
# (one way anova, df = 2,40 F = 21.361, p = 4.88e-07)

# according to the Tukey HSD, The Scorpine group had a significantly higher sporozoite count than the control, the Wild type had a significantly
# lower sporozoite count than the Scorpine, and there was no significant difference between the wild type and the control group 
# (One-way ANOVA: F = 21.361, df = 2,40, p = 0.7538847)

#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

library(readr)
crab <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv")
crab <- slice(crab,-85)
View(crab)

summ_crab <- crab %>%
  group_by(crabType) %>%
  summarise(mean_crab = mean(bodyTemperature),
            median_crab = median(bodyTemperature),
            var_crab = var(bodyTemperature),
            sd_crab = sd(bodyTemperature))


ggplot(crab, aes(x = bodyTemperature, y = crabType))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(crab) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.1)+
  facet_wrap(~crabType)
ggplot(crab)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

modelcrab <- lm(bodyTemperature~crabType, data = crab)

autoplot(modelcrab)

anova(modelcrab)

modelcrab <- aov(bodyTemperature~crabType, crab)
TukeyHSD(modelcrab)

# there is a significant difference between the means of the 4 groups of experimental crabs. Therefore we would reject the null.
# (one way anova, df = 3,80 F = 20.312, p = 6.997e-10)

# according to the tukey HSD, the intact male group, major removed group, and minor removed group all had significantly lower rates of heat gain. 
# also, the male minor removed group had a significantly lower rate of heat gain than the male major removed group. The other two groups, the 
# major removed group and minor removed group when compared to the intact male group did not have any significant differences in rates of heat 
# gain (ONE-way ANOVA, df = 3,80, F = 20.312, p = 6.997e-10)

#### 26/26 code runs without breaking ####