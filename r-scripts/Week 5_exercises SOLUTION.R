### Solution Script (Logistic Regression)
# Libraries
library(tidyverse)
library(survival)
library(here)



### Part A
## ApoE data
apoe <- read_csv(here("raw_data", "ApoE Study.csv"))

# Setting up factors in data set (not all needed, of course, but useful for further exploration, if you wish)
apoe <- apoe %>%
  mutate(
    sah = factor(sah, labels = c("low", "medium", "high")),
    ivh = factor(ivh, labels = c("no", "yes")),
    gcs_grp = factor(gcs_grp, labels = c("3-8", "9-12", "13-15")),
    ct_grp = factor(
      ct_grp,
      labels = c("diffuse", "diffuse, with swelling", "focal mass lesion")
    ),
    gosfav = factor(gosfav, labels = c("Unfavourable", "Favourable")),
    apoe_e4 = factor(apoe_e4, labels = c("no", "yes"))
  )

apoe # Should be set up for all analyses.


## Question 1
model1 <- glm(gosfav ~ apoe_e4, family = binomial("logit"), data = apoe)
summary(model1)
exp(cbind(OR = model1$coeff, confint(model1)))
# Model indicates that the odds of a Favourable outcome with the E4 allele present are about one quarter of the odds
# when it is absent - not very precise (CI is 0.10, 0.64), but could be quite a substantially negative risk factor.


## Question 2
model2 <-
  glm(gosfav ~ gcs_grp + apoe_e4,
      family = binomial("logit"),
      data = apoe)
summary(model2)
exp(cbind(OR = model2$coeff, confint(model2)))
anova(model2, test = "Chisq")
# As an alternative to the above command, use the following - this checks the contribution of each term in addition to
# everything else considered still to be in the model. It thus avoids the ordering problem in the anova() calculations.
drop1(model2, test = "Chisq")

# Interpretation:
# Once we adjust the model for GCS group, the effect of the E4 allele reduces quite substantially (perhaps entirely) -
# the OR for E4 increases to 0.40, with a CI that includes 1 (0.14, 1.15). Note the p-value in coeffs table = 0.088.
# Thus, GCS is confounding the relationship between E4 and outcome.


## Question 3
# Here we need to introduce an interaction term between the explanatory variables...
model3 <-
  glm(
    gosfav ~ gcs_grp + apoe_e4 + gcs_grp:apoe_e4,
    family = binomial("logit"),
    data = apoe
  )
summary(model3)
exp(cbind(OR = model3$coeff, confint(model3)))
anova(model3, test = "Chisq")
# drop1 not needed here, as sequential testing fine for interactions (normally)

# Interpretation:
# The Deviance table is particularly useful here, as there are 2 parameters needed for the GCS individual row in
# the coefficients table and the same for the interaction term - we can't directly read off the overall effect of
# these terms. The Deviance table doesn't have this problem, so can tell us that the p value for interaction
# is 0.371 - no evidence that it is needed.
#
# Note that the interaction terms are very imprecisely estimated - the CI for the relevant ORs are extremely wide,
# which usually results from having too few observations in one subcategory combination of the explanatory variables.



### Low Birthweight 1:3 matched study
LBW13 <- read_csv(here("raw_data", "LowBirthWt13.csv"))


# As usual, transform the categorical variables to factors. Outcome left as numeric, as this is needed for clogit()
LBW13 <- LBW13 %>%
  mutate(
    smoke = factor(smoke, labels = c("No", "Yes")),
    ptd = factor(ptd, labels = c("No", "Yes")),
    ht = factor(ht, labels = c("No", "Yes")),
    ui = factor(ui, labels = c("No", "Yes"))
  )
LBW13 # Tibble check

# For the model fit, remember that the variable that identifies the 1 case & 3 control strata is called "str" in these data
LBW13.clr <- clogit(low ~ smoke + ht + ptd + strata(str), data = LBW13)
summary(LBW13.clr)
anova(LBW13.clr, test = "Chisq")

# Interpretation:
# Remember that the p-values in the Deviance table are sequential tests, as opposed to the p-values produced by
# the summary command, so show you different effects. The summary results are perhaps more useful here.
#
# Less evidence that the explanatory factors influence low birthweight, but note the decrease in precision:
# the model is based on only 29 matched groups, as these are the only cases for which 3 suitable controls
# each can be found. We have likely lost a lot of power by vhanging from 1:3 to 1:1 matching in this case!
