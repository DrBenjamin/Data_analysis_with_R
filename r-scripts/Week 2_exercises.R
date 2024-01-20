### Course `Data analysis with R`
### Week 2 Exercises
### © Benjamin Gross, S2616861
## Libraries
pacman::p_load(tidyverse, epitools, finalfit, gtsummary, here, broom, NHANES)



##############################################################
### Cross-tabulations and Chi-square / Fisher's exact test ###
##############################################################
## Using the NHANES dataset:
## 1. Create a contingency table for the Education and Work variables, 
## for individuals aged between 40 and 49, with BMI between 18.5 and 24.9, 
## with good general health status.
# Remove missing data and filter data
cont_tab <- NHANES %>%
  drop_na(Work,Education,AgeDecade,HealthGen,BMI_WHO) %>%
  filter(str_replace_all(AgeDecade, " ", "") == "40-49", HealthGen == "Good", BMI_WHO == "18.5_to_24.9") %>%
  select(Work,Education)

# Show table
cont_tab %>%
  table()


## 2. Test the association between Education and Work using both 
## Fisher's exact test and a Chi-square test.



## 3. Test the association between the Education and Work variables 
## without filtering the dataset first, using the Chi-square test.




####################################
### Relative Risk and Odds Ratio ###
####################################
## A. In an outbreak of varicella (chickenpox), varicella was diagnosed in 
## 18 of 157 vaccinated children compared with 5 of 12 unvaccinated children.
## 1. Create a cross-tabulation for this data.
cont_tab2 <- c(18, 157, 5, 12) %>%
  matrix(., byrow = TRUE, nrow = 2)


## 2. Calculate the relative risk and its confidence interval.
cont_tab2 %>%
  epitab(., method = "riskratio", rev = "both")
# Confidence (lower, upper) is more important than p-value


## B. Consider the treatment of patients with endocarditis caused by 
## Staphylococcus aureus. In the population of interest, white males aged 30 to 
## 60, the mortality rate is 38% with the standard antibiotic treatment. A new 
## drug has been developed. A study showed that 152 men out of 400 died with the 
## standard treatment, and 17 men out of 120 died with the new drug.
## 1. What are the odds of dying with the new drug as opposed to the 
## standard antibiotic treatment?
cont_tab3 <- c(152, 248, 17, 103) %>%
  matrix(., byrow = TRUE, nrow = 2)
  
cont_tab3 %>%
  epitab(., method = "oddsratio", rev = "both")



############################################
### Confidence intervals for proportions ###
############################################
## Using the NHANES dataset:
## 1. Calculate the CI for the proportion of each combination of Diabetes 
## and HealthGen categories


## 2. Calculate the CI for the difference in proportions of individuals 
## with very good health between individuals with diabetes and individuals 
## without diabetes.



###############
### t-tests ###
###############
## Using the NHANES dataset:
## 1. Test whether the mean BMI of individuals with Good or Very good general 
## health is different from 21.5.


## 2. Test whether individuals with very good general health and individuals 
## with fair general health have the same mean BMI.



###################################################
### Assumption checking and data transformation ###
###################################################
## 1. Import the following csv file, a subset of the NHANES dataset:
## `subsetNHANES_exercise_week2.csv`
sub_data <- read_csv(here("raw_data", "subsetNHANES_exercise_week2.csv"))
# We will use that dataset for all subsequent questions.


## 2. Explore the distribution of the BMI variable visually and statistically.


## 3. If the distribution is not normal, find a transformation that makes 
## the distribution look more normal.


## 4. If we split this variable by Gender, are the two variances equal?


## 5. Is the relationship between Age and DirectChol linear?
