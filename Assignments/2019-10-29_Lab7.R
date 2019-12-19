#### Lab 7: 1-way ANOVA #### 
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

# You can do it! :)

library(readr)
Jaffe <- read_csv("datasets/demos/Jaffe.csv")
View(Jaffe)

summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>%
  summarise(mean_A = mean(Aldrin),
            median_A = median(Aldrin),
            var_A = var(Aldrin),
            sd_A = sd(Aldrin))

Jaffe <- mutate(Jaffe,log10Aldrin = log10(Aldrin))

summ_HCB <- Jaffe %>%
  group_by(Depth) %>%
  summarise(mean_H = mean(HCB),
            median_H = median(HCB),
            var_H = var(HCB),
            sd_H = sd(HCB))

ggplot(Jaffe, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 0.5)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

ggplot(Jaffe, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))


modelHCB <- lm(HCB~Depth, data = Jaffe)

modelAldrin <- lm(Aldrin~Depth, data = Jaffe)

modellog10Aldrin <- lm(log10Aldrin~Depth, data = Jaffe)

autoplot(modelHCB)

autoplot(modelAldrin)

autoplot(modellog10Aldrin)


anova(modelHCB)

anova(modelAldrin)

anova(modellog10Aldrin)


### Multiple Comparisons if package "multicomp" fails ####

modellog10Aldrin_b <- aov(log10Aldrin ~ Depth, Jaffe)
TukeyHSD(modellog10Aldrin_b)

#### 10/10 code runs without breaking ####