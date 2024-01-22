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
  drop_na(Work, Education, AgeDecade, HealthGen, BMI_WHO) %>%
  filter(str_replace_all(AgeDecade, " ", "") == "40-49", HealthGen == "Good", BMI_WHO == "18.5_to_24.9") %>%
  select(Work, Education)

# Show table
cont_tab %>%
  table()


## 2. Test the association between Education and Work using both 
## Fisher's exact test and a Chi-square test.
cont_tab %>%
  table() %>%
  fisher.test()

# Chi-square test
cont_tab %>%
  table() %>%
  chisq.test() #%>%
  .$expected


## 3. Test the association between the Education and Work variables 
## without filtering the dataset first, using the Chi-square test.
cont_tab_unfiltered <- NHANES %>%
  drop_na(Work, Education) %>%
  select(Work, Education)

# Chi-square test
cont_tab_unfiltered %>%
  table() %>%
  chisq.test()



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
  epitab(., method = "riskratio", rev = "both") # No (exposure) and No (outcome)
                                                # need to be first row & column
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
  epitab(., method = "oddsratio") # As it is a symmetric test, no need for `rev`



############################################
### Confidence intervals for proportions ###
############################################
## Using the NHANES dataset:
## 1. Calculate the CI for the proportion of each combination of Diabetes 
## and HealthGen categories
 NHANES %>%
  select(Diabetes, HealthGen) %>%
  drop_na() %>%
  table()

cont_tab4 <- NHANES %>%
  drop_na(Diabetes, HealthGen) %>%
  group_by(Diabetes) %>%
  count(HealthGen) %>%
  mutate(Total = sum(n)) %>%
  rowwise %>%
  mutate(prop = prop.test(n, Total, conf.level = 0.95)$estimate,
         lower_ci = prop.test(n, Total, conf.level = 0.95)$conf.int[1],
         upper_ci = prop.test(n, Total, conf.level = 0.95)$conf.int[2])

cont_tab4

## 2. Calculate the CI for the difference in proportions of individuals 
## with very good health between individuals with diabetes and individuals 
## without diabetes.
NHANES %>%
  drop_na(Diabetes, HealthGen) %>%  # Remove rows with missing Diabetes or HealthGen value
  group_by(Diabetes) %>%            # Group by Diabetes status (Yes or No), so the counts are calculated separately for each category
  count(HealthGen) %>%              # Count the number of rows for each combination of variable values (e.g. No diabetes and Excellent health, No diabetes and Vgood health, etc.)
  mutate(Total = sum(n)) %>%       # Create a new column named Total with the total number of observations for each Diabetes category
  filter(HealthGen == "Vgood") %>% # Only keep rows where the value of HealthGen is Vgood
  ungroup() %>%                    # Ungroup the tibble, so it "forgets" about the other values of HealthGen
  mutate(hasDiabetes = Diabetes == "Yes") %>% # Create a new logical column named hasDiabetes which is TRUE if Diabetes is "Yes" and FALSE otherwise
  summarise(prop_diabetes = n[hasDiabetes]/Total[hasDiabetes], # Create a new column called prop_diabetes by dividing the number of people with Diabetes and Vgood health status by the total number of people with Diabetes
            prop_noDiabetes = n[!hasDiabetes]/Total[!hasDiabetes], # Create a new column called prop_noDiabetes by dividing the number of people without Diabetes and with Vgood health status by the total number of people without Diabetes
            diff_prop = prop_diabetes - prop_noDiabetes, # Create a new column called diff_prop by subtracting the proportion of people with Vgood health status and without Diabetes from the proportion of people with Vgood health status and Diabetes
            # Create a new column called lower_ci by calling the prop.test function with arguments:
            # a vector of two numbers: the number of people with Diabetes and Vgood health status, and the number of people without Diabetes and with Vgood health status,
            # another vector of two numbers: the total number of people with Diabetes, and the total number of people without Diabetes,
            # and the confidence level set to 95%
            # The value stored in the column is the lower bound of the CI, extracted from the test result using $conf.int[1]
            lower_ci = prop.test(c(n[hasDiabetes], n[!hasDiabetes]), c(Total[hasDiabetes], Total[!hasDiabetes]), conf.level = 0.95)$conf.int[1],
            # Create a new column called upper_ci by calling the prop.test function with the same arguments as the line above
            # This time the value stored in the column is the upper bound of the CI, extracted from the test result using $conf.int[2]
            upper_ci = prop.test(c(n[hasDiabetes],n[!hasDiabetes]), c(Total[hasDiabetes],Total[!hasDiabetes]), conf.level = 0.95)$conf.int[2])



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
