# ========================================== #
# Logistic regression - demonstration script #
# ========================================== #

# Set up as normal - set working directory, create  a project or whatever method you prefer...
library(tidyverse)
library(survival) #  Makes the clogit function available for conditional logistic regression
library(here)


#### Course script files
browseURL(here("meta_data", "Intro Logistic Regression Lecture (slides).pdf"))
browseURL(here("meta_data", "Logistic Regression for Matched Designs Lecture (slides).pdf"))
browseURL(here("meta_data", "Extending Logistic Regression Lecture (slides).pdf"))


#### Example of standard logistic regression
browseURL(here("meta_data", "Codebook for Liver data.pdf"))
Liver <- read_csv(here("raw_data", "Liver.csv"))

# Set up outcome variable as factor, with labelling from codebook.
Liver <- Liver %>%
  mutate(outcome = factor(outcome, labels = c("Alive", "Dead")))

# For now, bypass the usual data analysis process steps of checking, exploration 
# etc - assume we have reached the model building stage. The method `glm` is 
# the generalised linear model function, which can be used for logistic regression.
Liver.lr <-
  glm(outcome ~ log_bil + log_icg,
      data = Liver,
      family = binomial("logit"))

# Showing model parameters etc, but NOT Odd Ratio (ORs) - 
# need to extract the coefficients and back-transform them
summary(Liver.lr) 

# Extracting the coefficients and back-transforming them to ORs
exp(cbind(OR = Liver.lr$coeff, confint(Liver.lr)))

# Printing out the Analysis of Deviance table - both explanatory variables
# contribute strongly
anova(Liver.lr, test = "Chisq")


#### Example of conditional logistic regression, with 1:1 matching
browseURL(here("meta_data", "Codebook for LowBirthWt data.pdf"))
LBW <- read_csv(here("raw_data", "LowBirthWt.csv"))

# Unlike other functions, we need to keep the outcome variable as numeric here (don't change it to a factor). clogit needs
# the numeric codes internally to work properly. Can & should transform the other variables to factors though...

LBW <- LBW %>%
  mutate(
    smoke = factor(smoke),
    ptd = factor(ptd),
    ht = factor(ht),
    ui = factor(ui),
    ethnic = factor(ethnic)
  )

# Checking the tibble to see that above has worked
LBW

# The clogit function fits a conditional logistic regression model
# (via a call to coxph) with relatively little setting up - the model
# is defined just as for a standard logistic model (response is
# just case/control status) but with a strata() term to include
# the matching variable.

# Seeing model output, parameter estimates, ORs etc
LBW.clr <- clogit(low ~ smoke + ht + ptd + strata(pair), data = LBW)
summary(LBW.clr)

# Generating an Analysis of Deviance table, to see the contribution 
# of the 3 explanatory variables in predicting outcome
anova(LBW.clr)

# summary command also prints out ORs (listed as exp(coef)) and 95% CI (lower .95 and upper .95), so little else needed for now.
