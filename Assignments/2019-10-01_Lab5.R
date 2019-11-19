### Lab 5. t-tests and friends
# You can do it! :)

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
install.packages("DescTools")
library("DescTools")

# Read in data file, generic version
#<name-you-assign><-read_csv("path-to-file", col_names = TRUE)

library(readr)
furness <- read_csv("datasets/quinn/chpt3/furness.csv")
View(furness)

summ_furness <- furness %>%
  group_by(SEX) %>% 
  summarise(mean_METRATE = mean(METRATE),
            median_METRATE = median(METRATE),
            sd_METRATE = sd(METRATE),
            var_METRATE = var(METRATE),
            n_METRATE = n())

### One sample t-test #########################
# Option A
# The function pt() calculates the probability of t less than or equal to a sample value.  Note that this is 
# annoyingly the opposite of what a t-table does.  C'est la vie.

# First step, calculate t_sample.  You will need to define what the sample mean, null hypothesis mean, sample 
# standard deviation, and sample size are.  
null_mean <- 23.4722

### CODE BREAKS HERE

# If you are given the values for the sample mean, sd, and n, you can simply define each value as an object 
# in the environment
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1


# If you are given raw data, read in the data file and define each summary statistic with a simple equation
# Note: you can't use summarise here because it will create a table instead of named objects.

# Read in data
earth <- read_csv("datasets/demos/earth.csv", col_names = TRUE)

heart <- read_csv("datasets/demos/HeartAttack_short.csv")
view(heart)

### CODE BREAKS HERE
# Identify your response variable using the form dataset$variable_name
y<-earth$Obliquity

# Calculate summary statistics
null_mean <- 23.4722
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# Whether you are given the values for mean/sd/n or calculate them, your next step is calculating t_sample
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

# The value I call "negative tail" is the exact probability of obtaining t less than or equal to your t-sample
# If you are testing an alternate hypotheses of "sample mean is less than a certain number" then this is your
# p-value
negative_tail <- pt(t_sample, df)

# If you are testing an alternate hypothesis of "sample mean is greater than a certain number" then you have
# to calculate 1 - negative_tail.
positive_tail <- 1 - negative_tail

# For a two-sided test, the exact probability of obtaining t equal to t_sample or more extreme is calculated
# as:
two_tailed <- 2*(1-pt(abs(t_sample), df))

# Option B
# One-sample t-test can be calculate using t.test. 
# The mu argument gives the value stated in the null hypothesis.

# The code below ASSUMES that you have read in the data file
# Now you have to specify which dataset the values are coming from using the form dataset$variable_name.

# Two-sided
t.test(range_shift$elevationalRangeShift, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sided, HA that sample mean is greater than null mean
t.test(range_shift$elevationalRangeShift, 
       alternative = "greater", mu = 0, conf.level = 0.95)

# One-sided, HA that sample mean is less than null mean
t.test(range_shift$elevationalRangeShift, 
       alternative = "less", mu = 0, conf.level = 0.95)




### Paired t-test #########################
# Start with a dataset in untidy format (groups not defined by a categorical variable, two observations (or 
# more) in each row.  Later you will use this untidy dataset to perform the statistical test.
# These data come from Chapter 12 in your book.
untidy_blackbird <- read_csv("datasets/abd/chapter12/chap12e2BlackbirdTestosterone.csv")

library(readr)
elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")

# Begin by exploring the data with histograms, boxplots, and q-q plots
# Since the assumptions of normality apply to differences, use mutate() to add a column called diff.
# Note that here diff = After - Before

elgar <- mutate(elgar, diff = HORIZLIG - HORIZDIM)


untidy_blackbird <- mutate(untidy_blackbird, diff = afterImplant - beforeImplant)

ggplot(untidy_blackbird) +
  geom_histogram(aes(diff), binwidth = 10)

ggplot(untidy_blackbird) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(untidy_blackbird)+
  geom_qq(aes(sample = diff))

# Now perform the statistical test.  The boxplot wasn't horrible, and the sample size is 31, so you could
# justify not transforming.  Then again, the Q-Q plot is not so great, so you could also justify transforming.
# What is important to me, is that you justify your choice.

# There are (at least) two methods for paired t-tests.  The first is a one sample t-test on the differences, 
# using the function pt().
# The second uses the function t.test().  Unlike using t.test() for a one sample t-test, a two sample t-test
# specifies each group (i.e., before and after), does not take the argument mu = , and takes the argument 
# paired = TRUE.
# Note that the confidence intervals are for the mean difference.

# Two-sided
t.test(elgar$HORIZLIG, elgar$HORIZDIM, alternative = "two.sided", paired = TRUE, conf.level = 0.95)

t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# One-sided, HA that afterImplant is greater than beforeImplant
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)

# One-sided, HA that afterImplant is less than beforeImplant
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "less", paired =  TRUE, conf.level = 0.95)

# The most straight-forward way to show var.equal data is to connect each pair with a line.  To do this, you
# first have to make data tidy (each variable has its own column, one observation in each row).

# Generic code to transform untidy data to tidy data
# <new_name> <- <untidy_dataset_name> %>% 
# gather(<one_group>, <other_group>, key = "<heading_for_grouping_variable>", value = "<heading_for_response>")

tidy_blackbird <- untidy_blackbird %>%
  gather(beforeImplant, afterImplant, key="treatment", value = "antibody")

ggplot(tidy_blackbird, aes(x=treatment, y=antibody, group=blackbird)) +
  geom_point(aes(colour=treatment), size=4.5) +
  geom_line(size=1, alpha=0.5) +
  xlab('Testosterone Treatment') +
  ylab('Antibody Production (mOD/min)') +
  scale_colour_manual(values=c("#009E73", "#D55E00"), guide=FALSE) + 
  theme_bw()

### Non-parametric Sign Test #########################

# Although not necessary in either case, it is instructive to perform a sign test on the range_shift and 
# untidy_blackbird data.

# One-sample, Two-sided
SignTest(range_shift$elevationalRangeShift, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sample, One-sided, HA that sample mean is greater than null mean
SignTest(range_shift$elevationalRangeShift, 
       alternative = "greater", mu = 0, conf.level = 0.95)

# One-sample, One-sided, HA that sample mean is less than null mean
SignTest(range_shift$elevationalRangeShift, 
       alternative = "less", mu = 0, conf.level = 0.95)

# NOTE, for paired you need to specify the difference variable (in this case diff)
# Two-sided
SignTest(untidy_blackbird$diff, alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sided, HA that afterImplant is greater than beforeImplant
SignTest(untidy_blackbird$diff, alternative = "greater", mu = 0, conf.level = 0.95)

# One-sided, HA that afterImplant is less than beforeImplant
SignTest(untidy_blackbird$diff, alternative = "less", mu = 0, conf.level = 0.95)

### Two sample t-test #########################

# Pooled variances
# Read in the Ward & Quinn dataset looking at the egg production of predatory snails
heart <- read_csv("datasets/demos/HeartAttack_short.csv", col_names = TRUE,
                  col_types = cols(group = col_character()))
view(heart)



# Look at the summary statistics
summ_heart <- heart %>%
  group_by(group) %>% 
  summarise(mean_heart = mean(cholest),
            median_heart = median(cholest),
            sd_heart = sd(cholest),
            var_heart = var(cholest),
            n_heart = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_heart$sd_heart))/(min(summ_heart$sd_heart))

# Look at histograms, box plots, q-q plots
ggplot(heart) +
  geom_histogram(aes(cholest), binwidth = 10)+
  facet_wrap(~group)

ggplot(heart) +
  geom_boxplot(aes(x = group, y = cholest))

ggplot(heart)+
  geom_qq(aes(sample = cholest, color = group))

# A little right skew indicated in both histogram and but nothing terrible.

# For the two-sample t-test with pooled variance, there are additional arguments.  You need to give the 
# formula (response ~ predictor), identify the data, include var.equal = TRUE.

# Two-sided
t.test(cholest ~ group, data = heart, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# NOTE: Group 1 and Group 2 are ordered alphabetically unless you specify otherwise
# In the output of the t-test, the first mean under "sample estimates" is group 1, the second is group 2
# One-sided, HA that Littor is greater than Mussel
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

# One-sided, HA that Littor is less than Mussel
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "less", conf.level = 0.95)

## Welch's t-test #########################

### CODE BROKEN, DOES NOT READ IN CORRECT FILE, HAVE THE HEART FILE BELOW

# Read in the Levin et al dataset from Chapter 12 of your book.  
heart <- read_csv("datasets/demos/HeartAttack_short.csv")
view(heart)


# Suppose we are interested in potential differences in the proportion of surviving native chinook salmon
# in the presence and absence of invasive brook trout.
# Examine the ratio of the variances
summ_heart <- heart %>%
  group_by(group) %>% 
  summarise(mean_heart = mean(cholest),
            sd_heart = sd(cholest),
            n_heart = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_heart$sd_heart))/(min(summ_heart$sd_heart))

# Examine plots for evidence of non-normality.  

#Histogram is pretty worthless because n is so small.
ggplot(salmon) +
  geom_histogram(aes(proportionSurvived), binwidth = 0.05)+
  facet_wrap(~troutTreatment)

ggplot(salmon) +
  geom_boxplot(aes(x = troutTreatment, y = proportionSurvived))

ggplot(salmon)+
  geom_qq(aes(sample = proportionSurvived, color = troutTreatment))

# Go forward assuming that normality has been met but homogeneity of variances has not.
# To perform Welch's t-test, all you need to do is remove the argument var.equal = TRUE

# Two-sided
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "two.sided", conf.level = 0.95)


# One-sided, HA that absent is greater than present
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "greater", conf.level = 0.95)

# One-sided, HA that absent is less than present
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "less", conf.level = 0.95)

### Non-parametric Mann-Whitney U Test #########################

# For this we are going to return to the cannibal crickets from Exam 1 Extra Credit
furness <- read_csv("datasets/quinn/chpt3/furness.csv")
View(furness)

ggplot(furness) +
  geom_histogram(aes(METRATE), binwidth = 10)+
  facet_wrap(~SEX)

ggplot(furness) +
  geom_boxplot(aes(x = SEX, y = METRATE))

ggplot(furness)+
  geom_qq(aes(sample = METRATE, color = SEX))

# The fed group is pretty non-normal

# The Mann-Whitney U Test is equivalent to the Wilcoxon rank-sum test.  Similar to our 2-sample t-test 
# examples, we give a formula in the form y ~ x or response ~ predictor.

### CODE BROKEN HERE

# Two-sided
wilcox.test(METRATE ~ SEX, data = furness, alternative = "two.sided", conf.level = 0.95)

# One-sided, HA that fed greater than starved
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "greater", conf.level = 0.95)

# One-sided, HA that fed less than starved
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "less", conf.level = 0.95)


