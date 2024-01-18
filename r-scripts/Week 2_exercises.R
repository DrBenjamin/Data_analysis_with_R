### Exercises
library(tidyverse)
library(NHANES)
install.packages("epitools")
library(epitools)


## Exercises 2
# Make cross tab
c(18, 157, 5, 12) %>%
  matrix(., byrow = TRUE, nrow = 2) %>%
  epitab(., method = "riskratio", rev = "both")
# confidence more important than p-value (lower, upper)

# 2B
c(152, 248, 17, 103) %>%
  matrix(., byrow = TRUE, nrow = 2) %>%
  epitab(., method = "oddsratio", rev = "both")
