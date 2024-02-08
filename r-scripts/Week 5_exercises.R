### Course "Data analysis with R"
### Week 5 - Exercises
## Libraries
pacman::p_load(tidyverse, survival, gtsummary, here, ggplot2)



##################################
## A. For the following data set #
##################################
## Fit a simple logistic regression model for the outcome (gosfav) in terms of 
## ApoE status.
## What happens when we add the grouped Glasgow Coma Scale (GCS) score as a 
## second explanatory variable to the above model?
## Is there any evidence that grouped GCS may be an effect modifier of the 
## ApoE / outcome relationship?
# Reading codebook
browseURL(here("meta_data", "Codebook for Apo E data.pdf"))

# Loading data
apoe <- read_csv(here("raw_data", "ApoE Study.csv"))

# Viewing data
View(apoe)
head(apoe)
glimpse(apoe)

# Setting up outcome variable as factor, with labelling from codebook
apoe <- apoe %>%
  mutate(gosfav = factor(gosfav, labels = c("Unfavourable (1-3)", "Favourable (4-7)"))) %>%
  mutate(gosfav_outcome = fct_relevel(gosfav, "Favourable (4-7)")) %>%
  mutate(apoe_e4 = factor(apoe_e4, labels = c("No", "Yes")))

# Showing levels of the `gosfav` variable
str(apoe)
levels(apoe$gosfav)
levels(apoe$gosfav_outcome)

# Exploring the relationship between ApoE status and the outcome
apoe %>%
  tbl_cross(row = apoe_e4, col = gosfav_outcome, percent = "row")

# Exploratory data analysis:
# apoe_e4 Favourable (4-7)	Unfavourable (1-3)  total
# No	    47 (75%)	        16 (25%)	          63 (100%)
# Yes	    13 (43%)	        17 (57%)	          30 (100%)
# Total	  60 (65%)          33 (35%)            93 (100%)
#
# Interpretation:
# Among the 30 / 93 patients who had the allele, 57% had unfavorable outcome after 
# injury. Among the 63/90 patients who did not have the allele, 25% had the 
# unfavorable outcome. Hence there appears to be a positive association between 
# the exposure variable and the outcome variable.

# Showing logistic regression model for the outcome (gosfav) in terms of ApoE status
apoe.lr <-
  glm(formula = gosfav_outcome ~ apoe_e4,
      data = apoe,
      family = binomial("logit"))

# Showing summary
summary(apoe.lr)

# Results:
# glm(formula = gosfav_outcome ~ apoe_e4, family = binomial("logit"), data = apoe)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.2933  -0.7655  -0.7655   1.0658   1.6556  

# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.0776     0.2894  -3.723 0.000197 ***
#  apoe_e4Yes    1.3458     0.4685   2.872 0.004073 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 120.97  on 92  degrees of freedom
# Residual deviance: 112.45  on 91  degrees of freedom
# AIC: 116.45
#
# Number of Fisher Scoring iterations: 4
#
# Interpretation:
# Parameter beta (log of odds ratio) is positive (1.3458) with a p-value <0.004073, 
# hence the odds ratio should be >1. This means if the allele is present, the 
# probability of experiencing unfavorable outcome after acute head injury increases. 
# This influence is statistically significant since the p-value is <0.05.

# Extracting the coefficients and back-transforming them to ORs
exp(cbind(OR = apoe.lr$gosfav_outcome, confint(apoe.lr)))

# Printing out the Analysis of Deviance table - both explanatory variables
# contribute strongly
anova(apoe.lr, test = "Chisq")

# Results:
# Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
# NULL                       92     120.97            
# apoe_e4  1   8.5205        91     112.45 0.003512 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Interpretation:
# The null hypothesis is that the "best fit" and the "perfect fit" models are 
# the same (parameter = 0). Since the the p-value is <0.05, the null hypothesis
# can be rejected.
#
## Conslusion:
# The presence of allele ApoE e4 allele are associated with greater probabilities 
# of experiencing unfavorable outcome after an acute head injury.



########################################
## B. The data file `LowBirthWt13.csv` #
########################################
## is an adapted version of the data set used in the demo video
## it has 1 case matched to 3 controls. Its codebook is here:   
browseURL(here("meta_data", "Codebook for LowBirthWt13 data.pdf"))
## Fit a conditional logistic regression model for low birthweight in terms 
## of smoking, history of hypertension and history of premature labour in these 
## data. Compare the results to those of the model fitted to the 1:1 matched 
## data in the demo.
# Loading data
low_birth_wt <- read_csv(here("raw_data", "LowBirthWt13.csv"))


## Post results from one of these 4 questions (A.1, A.2, A.3 or B) on the
## discussion board for your group.
## A model solution will become available at 9am (UK time) on Friday of this week.




