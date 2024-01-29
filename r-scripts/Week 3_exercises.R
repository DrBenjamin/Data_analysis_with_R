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
# Showing the course script(s)
browseURL(here("meta_data", "Non-parametric Methods - Introduction.pdf"))
browseURL(here("meta_data", "Non-parametric Methods - Caveats.pdf"))

# Loading data into variable
data <- NHANES

# Performing the Wilcox test
NHANES %>%
  drop_na(BMI, HealthGen) %>%
  filter(HealthGen == "Good" | HealthGen == "Vgood") %>%
  select(HealthGen, BMI) %>%
  wilcox.test(.$BMI, mu = 21.5, data = ., conf.int = TRUE)


## 2. Are the BMI of individuals with `Very Good` general health drawn from a
## different distribution to those individuals with `Fair` general health?
# Creating the subset of the data
subsetNHANES <- NHANES %>% 
  drop_na(BMI, HealthGen) %>% 
  select(BMI, HealthGen) %>% 
  filter(HealthGen == "Vgood" | HealthGen == "Fair")

# Calculating the mean, median and standard deviation of the BMI
subsetNHANES %>%
  group_by(HealthGen) %>% 
  summarise(mean = mean(BMI), median = median(BMI), sd(BMI))

# Showing histogram for both health conditions 
subsetNHANES %>%
  group_by(HealthGen) %>%
  mutate(med_BMI = round(median(as.numeric(BMI)), 2), mean_BMI = round(mean(as.numeric(BMI)), 2)) %>%
  ggplot(aes(x = BMI)) +
    geom_histogram(aes(y = ..density.., fill = ..count..), bins = 30) +
    ylab(label = "Density") +
    facet_grid(cols = vars(HealthGen)) +
      geom_vline(aes(xintercept = mean_BMI), color = "blue", linewidth = 1) +
      geom_vline(aes(xintercept = med_BMI), color = "orange", linewidth = 1) +
      geom_text(aes(x = 40, y = 0.07, label = paste0("Mean: ", mean_BMI)), color = "blue") +
      geom_text(aes(x = 40, y = 0.067, label = paste0("Median: ", med_BMI)), color = "orange")

# Performing the Wilcox test
subsetNHANES %>%
  wilcox.test(BMI ~ HealthGen, data = ., conf.int = TRUE)
# Compares the median, the median is different, CI is pseudo median


## B: For the subset NHANES data (subsetNHANES_exercise_week2.csv)
## 1. Are the BMI of men and women plausibly drawn from the same 
## underlying distribution?
subNHANES <- read_csv(here("raw_data", "subsetNHANES_exercise_week2.csv")) %>%
  drop_na(BMI, Gender) %>%
  select(BMI, Gender)

# Calculating mean and median for the BMI of both sexes
subNHANES %>%
  group_by(Gender) %>%
  summarise(mean = mean(BMI), median = median(BMI))

# Showing histogram for both sexes
subNHANES %>%
  group_by(Gender) %>%
  mutate(med_BMI = round(median(as.numeric(BMI)), 2), mean_BMI = round(mean(as.numeric(BMI)), 2)) %>%
  ggplot(aes(x = BMI)) +
    geom_histogram(aes(y = ..density.., fill = ..count..), bins = 30) +
    facet_grid(~Gender) +
      geom_vline(aes(xintercept = mean_BMI), color = "blue", linewidth = 1) +
      geom_vline(aes(xintercept = med_BMI), color = "orange", linewidth = 1)
      
# Performing Wilcox test
subNHANES %>%
  group_by(Gender) %>%
  wilcox.test(BMI ~ Gender, data = ., conf.int = TRUE)



########################
### Standardisation ###
#######################
## 1. Using the French prostate cancer data, calculate a standardised rate of 
## prostate cancer (`Prostate Cancer.csv`) in the 1994-96 period, again using 
## the 1985-87 period as the standard French population
# Showing the course script(s)
browseURL(here("meta_data", "Essential_Medical_Statistics_25_Standardization.pdf"))
prosCancer <- read_csv(here("raw_data", "Prostate Cancer.csv"))
with(prosCancer, ageadjust.direct(count = `PC94-96`, pop = `Pop94-96`, 
                                  stdpop = `Pop85-87`))
<<<<<<< HEAD
=======

# Output: 
# crude.rate adj.rate        lci        uci 
# 2.382100   2.386850   2.357075   2.416946
#
# Interpretation:
# Direct standardization method is used to calculate the standardized rate of 
# prostate cancer in the 1994-96 period. The age-specific rates of the 1994-96 
# period is applied to the 1985-87 population as the standard population. 
# The crude and standardized rates of prostate cancer is very similar, 2.38 per 
# 1000 person-years, suggesting the age structure of the two populations are very similar. 
>>>>>>> refs/remotes/origin/main


## 2. The file `ICU Data.csv` contains age and sex-specific figures for mortality 
## in all intensive care units (ICU) in England for a one year period in the past. 
## It also contains number of deaths and patient population size for a specific 
## hospital of interest. Calculate the SMR for this hospital relative to the 
## overall ICU population for this year. (The data are adapted from 
## s4be.cochrane.org, but please note there appears to be an error in the
## calculation of the total expected number of deaths for the hospital of 
## interest on this page.)
ICUdata <- read_csv(here("raw_data", "ICU Data.csv"))
with(ICUdata, ageadjust.indirect(count = HospitalDeaths, pop = HospitalPopulation, 
<<<<<<< HEAD
                                 stdcount = StandardDeaths, stdpop = StandardPopulation))
=======
                                     stdcount = StandardDeaths, stdpop = StandardPopulation))
# Output:
# $sir
# observed         exp         sir         lci         uci 
# 149.0000000 130.2102220   1.1443034   0.9745588   1.3436134 
# 
# $rate
# crude.rate   adj.rate        lci        uci 
# 0.2665474  0.2451625  0.2087954  0.2878639 
#
# Interpretation:
# Standardized mortality ration (SMR): 1,144 (95% CI: 0.97 - 1.34)
# The mortality rate in the specific hospital of interest is slightly higher 
# than the overall ICU population. Given that the confidence intervals include 1
# we can conclude that this is not a statistically significant difference.

>>>>>>> refs/remotes/origin/main


###############################################
### Statistical Aspects of Diagnostic Tests ###
###############################################
## Try out these two examples. They have the same structure as the demonstration 
## video above - the first problem uses a binary test result, the second a 
## continuous measure that needs to have an optimal threshold calculated and 
## plotted in a ROC curve.
## The exercise `Practical Exercise.pdf`
## Data `Helicobacter.csv` and `AnginaMI.csv`
browseURL(here("meta_data", "DT - Basic Principles.pdf"))



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
