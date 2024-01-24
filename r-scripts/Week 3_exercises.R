### Course `Data analysis with R`
### Week 3 Exercises
### © Benjamin Gross, S2616861
## Libraries
pacman::p_load(tidyverse, epitools, finalfit, gtsummary, here, broom, NHANES)



##############################
### Non-parametric Methods ###
##############################
## We are going to repeat some analyses from last week's material, in order to 
## compare the results obtained there via parametric analyses with corresponding 
## non-parametric methods that we have just encountered. Thus for each question, 
## either refer back to answers from last week, or re-run the relevant t-test 
## alongside the non-parametric test you choose.
## A. For the NHANES data:
## 1. Test whether the median BMI of individuals with either `Good` or 
## `Very Good` general health is different from `21.5`
data <- NHANES
NHANES %>%
  filter(HealthGen == "Good" | HealthGen == "Vgood") %>%
  select(HealthGen, BMI) %>%
  drop_na() %>%
  wilcox.test(.$BMI, mu = 21.5, data = ., conf.int = TRUE)


## 2. Are the BMI of individuals with `Very Good` general health drawn from a
## different distribution to those individuals with `Fair` general health?
NHANES %>%
  filter(HealthGen == "Fair" | HealthGen == "Vgood") %>%
  select(HealthGen, BMI) %>%
  drop_na() %>%
  wilcox.test(BMI ~ HealthGen, data = ., conf.int = TRUE)
# Compares the median, the median is different, CI is pseudo median

subsetNHANES<-NHANES %>% 
  drop_na(BMI, HealthGen) %>% 
  select(BMI, HealthGen) %>% 
  filter(HealthGen == "Vgood" | HealthGen == "Fair")

Height_stats <- subsetNHANES %>%
  summarise(mean = mean(BMI), median = median(BMI))

# Showing the median
subsetNHANES%>% 
  drop_na(BMI) %>% 
  group_by(HealthGen) %>% 
  summarise(median(BMI), sd(BMI))

subsetNHANES %>%
  ggplot(aes(x=BMI)) +
    geom_histogram(aes(y=..density.., fill=..count..),bins=30) +
    geom_vline(xintercept=mean(Height_stats$mean),color="blue",size=1) +
    geom_vline(xintercept=median(Height_stats$median),color="orange",size=1)+
    facet_grid(cols = vars(HealthGen)) + ylab("Density")


## B: For the subset NHANES data (subsetNHANES_exercise_week2.csv)
## 1. Are the BMI of men and women plausibly drawn from the same 
## underlying distribution?

## Post a brief summary of your results on the discussion board - do you get 
## similar numerical results as others? Do you agree with the interpretation 
## others have given? Share any other thoughts or code tips too.



########################
### Standardisation ###
#######################
## 1. Using the French prostate cancer data, calculate a standardised rate of 
## prostate cancer (`Prostate Cancer.csv`) in the 1994-96 period, again using the 1985-87 period as 
## the standard French population


## 2. The file `ICU Data.csv` contains age and sex-specific figures for mortality 
## in all intensive care units (ICU) in England for a one year period in the past. 
## It also contains number of deaths and patient population size for a specific 
## hospital of interest. Calculate the SMR for this hospital relative to the 
## overall ICU population for this year. (The data are adapted from 
## s4be.cochrane.org, but please note there appears to be an error in the
## calculation of the total expected number of deaths for the hospital of 
## interest on this page.)



###############################################
### Statistical Aspects of Diagnostic Tests ###
###############################################
## Try out these two examples. They have the same structure as the demonstration 
## video above - the first problem uses a binary test result, the second a 
## continuous measure that needs to have an optimal threshold calculated and 
## plotted in a ROC curve.
## The exercise `Practical Exercise.pdf`
## Data `Helicobacter.csv` and `AnginaMI.csv`


## Post a brief summary of your results on the discussion board - do you get 
## similar numerical results as others? Do you agree with the interpretation 
## others have given? Share any other thoughts or code tips too.


##########################################################
### Statistical Aspects of Diagnostic Tests (optional) ###
##########################################################
## If you have time and energy, you might like to explore this small set of 
## additional questions around interpreting diagnostic test statistics
## `Additional Examples.pdf`
## The first question asks you to do another calculation in R, using the 
## following data set `HIV.csv`
## Questions 2-4 are paper/ interpretation exercises.
