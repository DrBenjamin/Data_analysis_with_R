# Load the packages
library(tidyverse)
library(NHANES)
library(knitr)
library(broom)
library(epitools)
library(finalfit)


#################################
#       Cross-tabulations       #
#################################

## BMI and physical activity ##

# Create a tibble subset of NHANES called subtib1
subtib1 <- NHANES %>%
  drop_na(BMI_WHO,PhysActive) %>%
  select(BMI_WHO, PhysActive)

# Create a simple cross-tabulation
subtib1 %>%
  table()

# Create a contingency table using the Tidyverse
subtib1 %>%
  count(BMI_WHO,PhysActive) %>% 
  spread(PhysActive,n)

# Create a cross-tabulation with proportions and row totals using 
# summary_factorlist() finalfit
subtib1 %>%
  summary_factorlist(dependent ="PhysActive",
                     explanatory = "BMI_WHO",
                     column = F,
                     total_col = T)

# Create a contingency table using tbl_cross() from gtsummary
subtib1 %>%
  tbl_cross(row = BMI_WHO,
            col = PhysActive,
            percent = "row") %>%
  bold_labels()

## Gender and physical activity ##

# Create a tibble subset of NHANES called subtib2
subtib2 <- NHANES %>% 
  drop_na(Gender,PhysActive) %>%
  filter(AgeDecade == " 30-39",
         Education == "College Grad",
         BMI_WHO == "18.5_to_24.9") %>%
  select(Gender,PhysActive)

# Create a contingency table of subtib2
subtib2 %>%
  table()

# Create a contingency table of subtib2 using the Tidyverse
subtib2 %>% 
  count(Gender,PhysActive) %>% 
  spread(PhysActive,n)

# Create a cross-tabulation with proportions and row totals 
subtib2 %>%
  summary_factorlist(dependent ="PhysActive",
                     explanatory = "Gender",
                     column = F,
                     total_col = T)

# Create a contingency table using tbl_cross() from gtsummary
subtib2 %>%
  tbl_cross(row = Gender,
            col = PhysActive,
            percent = "row") %>%
  bold_labels()

#################################
#        Chi-square test        #
#################################
# Perform the chi-square test on subtib1
subtib1 %>%
  table() %>%
  chisq.test()

# Extract the expected values from the chi-square test
subtib1 %>%
  table() %>%
  chisq.test() %>%
  .$expected

# Create a contingency table using the Tidyverse and perform the chi-square 
# test on it
subtib1 %>%
  count(BMI_WHO,PhysActive) %>% 
  spread(PhysActive,n) %>% 
  select(-BMI_WHO) %>% 
  chisq.test()

# Create a contingency table using the Tidyverse, perform the chi-square 
# test on it and display the output in a tibble
subtib1 %>%
  count(BMI_WHO,PhysActive) %>% 
  spread(PhysActive,n) %>%
  select(-BMI_WHO) %>% 
  chisq.test() %>%
  glance()

#################################
#      Fisher's exact test      #
#################################
# Perform Fisher's exact test on subtib2
subtib2 %>%
  table() %>%
  fisher.test()

# Perform Fisher's exact test on subtib2 and display in a tibble
subtib2 %>% 
  count(Gender,PhysActive) %>% 
  spread(PhysActive,n) %>%
  select(-Gender) %>% 
  fisher.test() %>%
  glance()
