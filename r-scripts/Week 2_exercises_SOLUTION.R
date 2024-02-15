# Load the packages
library(tidyverse)
library(NHANES)
library(knitr)
library(broom)
library(epitools)
library(finalfit)

#################################
#        Cross-tab topic        #
#################################
# 1. Create a contingency table for the Education and Work variables, for 
# individuals aged between 40 and 49, with BMI between 18.5 and 24.9,
# with good general health status.
crosstab <- NHANES %>% 
  drop_na(Education,Work) %>%
  filter(AgeDecade == " 40-49", BMI_WHO == "18.5_to_24.9", HealthGen == "Good") %>%
  select(Education,Work)

crosstab %>%
  summary_factorlist(dependent = "Work", explanatory = "Education", column = F,total_col = T)

# Tidyverse option
crosstab %>%
  count(Education, Work) %>%
  # The argument fill=0 in the spread function replaces NA caused by value
  # combinations without any observations by 0
  spread(Work, n, fill = 0) %>%
  select(-Education) %>%
  #fisher.test() or
  chisq.test() %>%
  .$expected


# 2. Test the association between Education and Work using both Fisher's exact test
# and a Chi-square test.
crosstab %>%
  table() %>%
  fisher.test() %>%
  glance()

crosstab %>%
  table() %>%
  chisq.test() %>%
  glance()

# 3. Test the association between the Education and Work variables
# without filtering the dataset first, using the chi-square test.
crosstab2 <- NHANES %>% 
  drop_na(Education,Work) %>%
  select(Education,Work)

crosstab2 %>%
  summary_factorlist(dependent ="Work",explanatory = "Education",column = F,total_col = T)

crosstab2 %>%
  table() %>%
  chisq.test() %>%
  glance()


#################################
#      Risk estimate topic      #
#################################
# A. In an outbreak of varicella (chickenpox), varicella was 
# diagnosed in 18 of 157 vaccinated children compared with 
# 5 of 12 unvaccinated children.
# 1. Create a cross-tabulation for this data
# 2. Calculate the relative risk and its confidence interval.

VaricellaData <- tibble(
  VaccineStatus = c(rep("Yes",157),rep("No",12)),
  Varicella = c(rep("Yes",18),rep("No",139),rep("Yes",5),rep("No",7))
  )
VaricellaData %>%
  table() %>%
  epitab(.,method="riskratio")

# The relative risk of getting varicella for vaccinated children compared
# with unvaccinated children is 0.275. The 95% CI [0.12-0.61] doesn't include 1 and
# the p-value for the Fisher's test is 0.0125, indicating a significant association
# between the vaccination and the varicella disease. In particular,
# vaccinated children have a risk that's 0.275 times the risk unvaccinated children,
# meaning that they are nearly 75% (between 39% and 88%) less likely to get varicella, when
# compared with unvaccinated children.

# B. Consider the treatment of patients with endocarditis caused
# by Staphylococcus aureus. In the population of interest, 
# white males aged 30 to 60, the mortality rate is 38% with 
# the standard antibiotic treatment. A new drug has been developed.
# A study showed that 152 men out of 400 died with the standard 
# treatment, and 17 men out of 120 died with the new drug.

# 1. What are the odds of dying with the new drug as opposed to 
# the standard antibiotic treatment? 

OR <- tibble(
  Survival = c("Died", "Survived"),
  StandardTreatment = c(152,248),
  NewDrug = c(17,103)
)

OR %>%
  select(-Survival) %>%
  as.matrix() %>%
  epitab(.,method="oddsratio")

OR %>%
  select(-Survival) %>%
  as.matrix() %>%
  fisher.test()

# The patients who received standard care died 3.71 times 
# more often than patients treated with the new drug. 
# Based on these results the researcher would recommend that 
# all males aged 30 to 60 diagnosed with bacterial endocarditis
# caused by SA be prescribed the new drug.

#################################
#       Proportions topic       #
#################################

# 1. Calculate the CI for the proportion of each combination of Diabetes and HealthGen categories

NHANES %>%
  drop_na(Diabetes,HealthGen) %>%
  group_by(Diabetes) %>%
  count(HealthGen) %>%
  mutate(Total = sum(n)) %>%
  rowwise %>%
  mutate(prop = prop.test(n, Total, conf.level=0.95)$estimate,
         lower_ci = prop.test(n, Total, conf.level=0.95)$conf.int[1],
         upper_ci = prop.test(n, Total, conf.level=0.95)$conf.int[2])

# 2. # Difference in proportions of individuals with very good health 
# between individuals with diabetes and individuals without diabetes.


NHANES %>%
  drop_na(Diabetes,HealthGen) %>%  # Remove rows with missing Diabetes or HealthGen value
  group_by(Diabetes) %>%           # Group by Diabetes status (Yes or No), so the counts are calculated separately for each category
  count(HealthGen) %>%             # Count the number of rows for each combination of variable values (e.g. No diabetes and Excellent health, No diabetes and Vgood health, etc.)
  mutate(Total = sum(n)) %>%       # Create a new column named Total with the total number of observations for each Diabetes category
  filter(HealthGen == "Vgood") %>% # Only keep rows where the value of HealthGen is Vgood
  ungroup() %>%                    # Ungroup the tibble, so it "forgets" about the other values of HealthGen
  mutate(hasDiabetes=Diabetes == "Yes") %>% # Create a new logical column named hasDiabetes which is TRUE if Diabetes is "Yes" and FALSE otherwise
  summarise(prop_diabetes = n[hasDiabetes]/Total[hasDiabetes], # Create a new column called prop_diabetes by dividing the number of people with Diabetes and Vgood health status by the total number of people with Diabetes
            prop_noDiabetes = n[!hasDiabetes]/Total[!hasDiabetes], # Create a new column called prop_noDiabetes by dividing the number of people without Diabetes and with Vgood health status by the total number of people without Diabetes
            diff_prop = prop_diabetes - prop_noDiabetes, # Create a new column called diff_prop by subtracting the proportion of people with Vgood health status and without Diabetes from the proportion of people with Vgood health status and Diabetes
  # Create a new column called lower_ci by calling the prop.test function with arguments:
    # a vector of two numbers: the number of people with Diabetes and Vgood health status, and the number of people without Diabetes and with Vgood health status,
    # another vector of two numbers: the total number of people with Diabetes, and the total number of people without Diabetes,
    # and the confidence level set to 95%
  # The value stored in the column is the lower bound of the CI, extracted from the test result using $conf.int[1]
            lower_ci = prop.test(c(n[hasDiabetes],n[!hasDiabetes]), c(Total[hasDiabetes],Total[!hasDiabetes]), conf.level=0.95)$conf.int[1],
  # Create a new column called upper_ci by calling the prop.test function with the same arguments as the line above
  # This time the value stored in the column is the upper bound of the CI, extracted from the test result using $conf.int[2]
  upper_ci = prop.test(c(n[hasDiabetes],n[!hasDiabetes]), c(Total[hasDiabetes],Total[!hasDiabetes]), conf.level=0.95)$conf.int[2])


#################################
#         t-tests topic         #
#################################
# 1. Test whether the mean BMI of individuals with Good or Very good 
# general health is different from 21.5.
NHANES %>%
  drop_na(BMI) %>%
  filter(HealthGen == "Good" | HealthGen == "Vgood") %>%
  t.test(.$BMI,mu=21.5,data=.)


# 2. Test whether individuals with very good general health and 
# individuals with fair general health have the same mean BMI
NHANES %>%
  drop_na(BMI) %>%
  filter(HealthGen == "Vgood" | HealthGen == "Fair") %>%
  summarise(t_stat=t.test(BMI~HealthGen)$statistic,
            pval=t.test(BMI~HealthGen)$p.value,
            lowerCI=t.test(BMI~HealthGen)$conf.int[1],
            upperCI=t.test(BMI~HealthGen)$conf.int[2])

#################################
#      Assumptions topic        #
#################################
# 1. Import the following csv file, a subset of the NHANES dataset.
# Explore the distribution of the BMI variable visually and 
# statistically.
subsetNHANES <- NHANES %>%
  drop_na(BMI) %>%
  sample_n(1000)
#write_csv(subsetNHANES, "subsetNHANES_exercise_week2.csv")
subsetNHANES <- read_csv("./raw_data/subsetNHANES_exercise_week2.csv")

subsetNHANES %>%
  ggplot(aes(sample=BMI)) +
  stat_qq() +
  stat_qq_line(color=2) +
  labs(title="Normal Q-Q Plot") +    ## add title
  theme_bw() +                       ## remove gray background
  theme(panel.grid=element_blank())  ## remove grid

subsetNHANES %>%
  ggplot(aes(x=BMI)) +
  geom_histogram(aes(y=..density.., fill=..count..),bins=30) +
  geom_density(aes(y=..density..))

subsetNHANES %>%
  pull(BMI) %>%
  shapiro.test()

# 2. If the distribution is not normal, find a transformation
# that makes the distribution look more normal.
subsetNHANES %>%
  mutate(BMI=BMI^(-1/3)) %>%
  ggplot(aes(x=BMI)) +
  geom_histogram(aes(y=..density.., fill=..count..),bins=30) +
  geom_density(aes(y=..density..))

subsetNHANES %>%
  mutate(BMI=BMI^(-1/3)) %>%
  pull(BMI) %>%
  shapiro.test()

# 3. If we split this variable by Gender, are the two variances equal?
subsetNHANES %>%
  drop_na(Gender) %>%
  mutate(BMI=BMI^(-1/3)) %>%
  var.test(BMI ~ Gender, ., alternative = "two.sided")

# 4. Is the relationship between Age and DirectChol linear?
subsetNHANES %>%
  drop_na(Age,DirectChol) %>%
  ggplot(aes(x = Age, y = DirectChol)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Age (years)") +
  ylab("DirectChol")
