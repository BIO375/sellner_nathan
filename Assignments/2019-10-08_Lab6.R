# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


### Problem 13-20 ####

library(readr)
chap13q20SalmonColor <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")
View(chap13q20SalmonColor)

summ_chap13 <- chap13q20SalmonColor %>%
  group_by(species) %>%
  summarise(mean_skin = mean(skinColor),
            median_skin = median(skinColor),
            var_skin = var(skinColor),
            sd_skin = sd(skinColor))

summ_chap13 <- chap13q20SalmonColor %>%
  group_by(species) %>%
  summarise(mean_logskin = mean(log(skinColor)),
            median_logskin = median(log(skinColor)),
            var_logskin = var(log(skinColor)),
            sd_logskin = sd(log(skinColor)))

logchap13q20SalmonColor <- transform(chap13q20SalmonColor, logskinColor = log(skinColor))

ggplot(chap13q20SalmonColor) +
  geom_histogram(aes(skinColor), binwidth = .1) +
  facet_wrap(~species)
ggplot(chap13q20SalmonColor) +
  geom_boxplot(aes(x = species, y = skinColor))
ggplot(chap13q20SalmonColor) +
  geom_qq(aes(sample = skinColor, color = species))

# a
# List two methods that would be appropriate to test whether there 
# was a difference in mean skin color between the two groups?
# 1. You could use a two sample t-test with the transformed data to find the mean skin color of 
#    the two groups and compare them to each other to find a difference
# 2. You could also use a Mann-Whiteny U test to compare the means of 
#    of both groups if the data breaks the assumptions of normality which 
#    the qq plot does

# b
# Use a transformation to test whether there is a difference in 
# mean between these two groups. Is there a difference in the mean 
# of kokanee and sockeye skin color?

t.test(skinColor ~ species, data = logchap13q20SalmonColor, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# there is a significant difference in the mean of kokanee and sockeye skin color?

### Problem 13-25 ####

#test whether there is a change in biomass of rainforest areas following a clear cutting

chap13q25Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")
view(chap13q25Clearcuts)

ggplot(chap13q25Clearcuts) +
  geom_histogram(aes(biomassChange), binwidth = .9) 
ggplot(chap13q25Clearcuts) +
  geom_boxplot(aes(y = biomassChange, x=""))
ggplot(chap13q25Clearcuts) +
  geom_qq(aes(sample = biomassChange))

chap13q25Clearcuts <- mutate(chap13q25Clearcuts, diff = biomassChange)
 
SignTest(chap13q25Clearcuts$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
#There was not a significant change in biomass of rainforest areas after clear cutting

### problem 13-26 ####

# Choose an appropriate mathod and test whether females preferred one type of male over the other type

chap13q26ZebraFinchBeaks <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")

null_mean <- 0
summ_finch <- chap13q26ZebraFinchBeaks %>%
  summarise(mean_time = mean(preference),
            median_time = median(preference),
            var_time = var(preference),
            sd_time = sd(preference))

t.test(chap13q26ZebraFinchBeaks$preference, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)
# there is a difference in female preference for redder beaks

### review problems 2-16 ####
chap03q22ZebraFishBoldness <- read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv")
view(chap03q22ZebraFishBoldness)
### a ###
summ_fish <- chap03q22ZebraFishBoldness %>%
  group_by(genotype) %>%
  summarise(mean_behavior = mean(secondsAggressiveActivity),
            median_behavior = median(secondsAggressiveActivity),
            sd_behavior = sd(secondsAggressiveActivity),
            var_behavior = var(secondsAggressiveActivity))

ratio <-(max(summ_fish$sd_behavior))/(min(summ_fish$sd_behavior))

ggplot(chap03q22ZebraFishBoldness) +
  geom_histogram(aes(secondsAggressiveActivity), binwidth = 18) +
  facet_wrap(~genotype)
ggplot(chap03q22ZebraFishBoldness) +
  geom_boxplot(aes(x = genotype, y = secondsAggressiveActivity))
ggplot(chap03q22ZebraFishBoldness) +
  geom_qq(aes(sample = secondsAggressiveActivity, color = genotype))
                 
t.test(secondsAggressiveActivity ~ genotype, data = chap03q22ZebraFishBoldness, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

### b ###

# HO:M(Spr)-M(wild)=0    HA:M(Spr)-M(wild) does not equal zero
# The evidence against the Ho is that the results of the statistical test resulted in p<0.05 meaning there is a significant difference between time of
# aggressive behavior between the Spr and wild type groups and so the difference is not equal to zero and therefore we reject the null
