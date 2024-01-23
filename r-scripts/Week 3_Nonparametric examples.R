#### Script for Non-parametric Methods examples

# Look at parametric analyses for continuous data from Week 2, and show the non-parametric tests
# that are appropriate for the same analyses. Compare the p-values - will usually be larger
# = less power in non-parametric analyses.


# Load the tidyverse and NHANES packages
library(tidyverse)
library(NHANES)


## 1 sample t-test version for comparison (missing Age values omitted automatically by t.test)
NHANES %>% 
  ggplot(aes(x=Age)) + 
  geom_histogram(aes(y=..count.., fill=..density..),bins=20)
# Age perhaps is not that Normally distributed (more Uniform?)

NHANES %>%  t.test(.$Age,mu=35,data=.)

## Non-parametric analog = Wilcoxon signed rank test
NHANES %>%  wilcox.test(.$Age,mu=35,data=.,conf.int=T)
# CI is estimate of median (almost - subtle technical discussion in help file for wilcox.test if interested)


## 2 sample t-test version for comparison
NHANES %>%
  drop_na(Weight) %>% # Some missing values that generate a warning message, so removing these here
  ggplot(aes(x=Weight)) + 
  geom_histogram(aes(y=..count..,fill = ..density..),bins=20)+
  facet_grid(cols = vars(Gender))
# Possibly Normal, although interesting "bumps" on left hand side (children?)

NHANES %>%  t.test(.$Weight~.$Gender,data=.)

## Non-parametric analog = 2 sample Wilcoxon Rank Sum or Mann-Whitney test
NHANES %>%  wilcox.test(.$Weight~.$Gender,data=.,conf.int=T)


## Alternative, base R structure. Simpler code (?), but Tidyverse code more flexible for 
##  situations where more pre-processing is needed to select variables/ subset the data/ etc
wilcox.test(NHANES$Age,mu=35,conf.int=T)
wilcox.test(Weight~Gender,data=NHANES,conf.int=T)

