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
library(readr)
caffeine <- read_csv("datasets/final/caffeine.csv", 
                     col_types = cols(group = col_factor() ))
View(caffeine)

summ_caffeine <- caffeine %>%
  group_by(group) %>%
  summarise(mean_h = mean(half_life),
            median_h = median(half_life),
            var_h = var(half_life),
            sd_h = sd(half_life))

ggplot(caffeine, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 2.1)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))

caffeine <- mutate(caffeine,log10half_life = log10(half_life))

summ_caffeine <- caffeine %>%
  group_by(group) %>%
  summarise(mean_h = mean(log10half_life),
            median_h = median(log10half_life),
            var_h = var(log10half_life),
            sd_h = sd(log10half_life))

ratio <-(max(summ_caffeine$sd_h))/(min(summ_caffeine$sd_h))


ggplot(caffeine, aes(x = group, y = log10half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(caffeine) +
  geom_histogram(aes(log10half_life), binwidth = 1.3)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = log10half_life, color = group))



modelcaffeine <- lm(log10half_life~group, data = caffeine)
modelcaffeine

autoplot(modelcaffeine)

anova(modelcaffeine)

summary(modelcaffeine)

planned <- glht(modelcaffeine, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                   "norm_prog - high_prog = 0")))
confint(planned)
summary(planned)
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
