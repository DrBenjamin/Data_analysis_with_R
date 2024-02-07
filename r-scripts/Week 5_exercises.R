### Course "Data analysis with R"
### Week 5 - Exercises
## Libraries
pacman::p_load(tidyverse, NHANES, here, ggplot2, cowplot)

##
## A. For the following data set - 
## Fit a simple logistic regression model for the outcome (gosfav) in terms of ApoE status.
## What happens when we add the grouped GCS score as a second explanatory variable to the above model?
##Is there any evidence that grouped GCS may be an effect modifier of the ApoE/ outcome relationship?
# Reading Codebook
browseURL(here("meta_data", "Codebook for Apo E data.pdf"))

# Loading data
apoe <- read_csv(here("raw_data", "ApoE Study.csv"))


##
## B. The data file `LowBirthWt13.csv`
##is an adapted version of the data set used in the demo video
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




