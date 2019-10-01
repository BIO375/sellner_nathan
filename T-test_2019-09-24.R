# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

library(readr)
births <- read_csv("datasets/demos/births.csv")
View(births)

summ_births <- births %>%
  group_by(Country) %>%
  summarise(mean_Births = mean(Births),
            median_Births = median(Births),
            IQR_Births = IQR(Births),
            sd_Births = sd(Births),
            var_Births = var(Births))
view(summ_births)

ggplot(births) +
  geom_histogram(aes(Births), binwidth = 2)

ggplot(births) +
  geom_boxplot(aes(y = Births), notch = FALSE, varwidth = TRUE)

library(readr)
chap12e3HornedLizards <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
View(chap12e3HornedLizards)

chap12e3HornedLizards <- chap12e3HornedLizards %>% slice(-105)

summ_lizards <- chap12e3HornedLizards %>%
  group_by(Survival) %>%
  summarise(mean_horn = mean(squamosalHornLength),
            median_horn = median(squamosalHornLength),
            sd_horn = sd(squamosalHornLength),
            var_horn = var(squamosalHornLength))
view(summ_lizards)
 
ggplot(chap12e3HornedLizards) +
   geom_histogram(aes(squamosalHornLength), binwidth = 2)

ggplot(chap12e3HornedLizards) +
  geom_boxplot(aes(x = squamosalHornLength, y = Survival), binwidth = 10, notch = TRUE, varwidth = TRUE)
