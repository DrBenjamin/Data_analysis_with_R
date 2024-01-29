#### Script for Non-parametric Methods exercises

# Load the tidyverse and NHANES packages
library(tidyverse)
library(NHANES)

# Read in the NHANES subset data from Week 2
subNHANES<-read_csv(file = "subsetNHANES_exercise_week2.csv")

### Look at BMI questions in full NHANES data

# First, create a version of the data with either of the required HealthGen codes, then remove 
# rows with missing BMI:
Data1<-NHANES %>% 
  filter(HealthGen=="Good"|HealthGen=="Vgood") %>%
  drop_na(BMI)

# Graph to see what the set of resulting BMI look like:
Data1 %>%
  ggplot(aes(x=BMI)) + 
  geom_histogram(aes(y=..count.., fill=..density..),bins=20)

# Try both analyses to compare results:
Data1 %>% t.test(.$BMI,mu=21.5,data=.)
Data1 %>% wilcox.test(.$BMI,mu=21.5,data=.,conf.int = T)

# Large data set here, so t-test and Wilcoxon results are quite similar - p-values both very small
# and both tests suggest mean or median is closer to 28 than 21.5. 


# Second problem looks for differences in BMI between Very Good and Fair general health:
Data2<-NHANES %>% 
  filter(HealthGen=="Fair"|HealthGen=="Vgood") %>%
  drop_na(BMI)

# Graphing by HealthGen to see what the set of resulting BMI look like:
Data2 %>%
  ggplot(aes(x=BMI)) + 
  geom_histogram(aes(y=..count.., fill=..density..),bins=20) +
  facet_grid(rows = vars(HealthGen))

# Try both analyses to compare results:
Data2 %>% t.test(BMI~HealthGen,data=.)
Data2 %>% wilcox.test(BMI~HealthGen,data=.,conf.int = T)

# Again, quite similar results. Both tests show substantial evidence that the groups differ:
# about a 4 point difference in means, or a 3 point difference in median. Because of the positive 
# skewness here, the t-test may be slightly over-estimating the difference between the groups (?),
# but there is certainly no sense that the parametric result is misleading.
# The default t-test here corrects for imbalances in the group SDs, using Welch's Modification - 
# one can tell this has been applied because of the resulting fractional degrees of freedom (df).
# df are integers for the standard t-test.

### Look at BMI by Gender in NHANES subset
subNHANES %>% 
  ggplot(aes(x=BMI)) + 
  geom_histogram(aes(y=..count.., fill=..density..),bins=20) +
  facet_grid(rows = vars(Gender))

# Positive skewness here may primarily be an artefact of small number of outliers...

# Try both analyses to compare results:
subNHANES %>% t.test(BMI~Gender,data=.)
subNHANES %>% wilcox.test(BMI~Gender,data=.,conf.int = T)

# Very similar results again - no evidence of difference in BMI between men and women. Wilcoxon
# may have slightly better power here - the p-value it produces is smaller than for the t-test.

