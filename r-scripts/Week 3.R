### Course "Data analysis with R"
### Week 3
## Libraries
pacman::p_load(tidyverse, finalfit, gtsummary, here, NHANES)


## Histogram
NHANES %>%
  ggplot(aes(x = Age)) +
  geom_histogram(aes(y = ..count.., fill = ..density..), bins = 20)

NHANES %>% t.test(x = .$Age, mu = 35, data = ., conf.int = TRUE)


## Non-parametric analog = Wilson signed rank test
NHANES %>% wilcox.test(x = .$Age, mu = 35, data = ., conf.int = TRUE)


## 2 sample t-test version for comparison
NHANES %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Weight)) +
  geom_histogram(aes(y = ..count.., fill = ..density..)) +
  facet_grid(cols = vars(Gender))
# Possibly normally distributed, although interesting "bumps" on the left hand side (children?)

NHANES %>%
  t.test(.$Weight ~ .$Gender, data = .)

NHANES %>%
  wilcox.test(.$Weight ~ .$Gender, data = ., conf.int = TRUE)
