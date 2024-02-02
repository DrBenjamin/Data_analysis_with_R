##### Hints and Tips section

# Assumes Lead data set is loaded and has been modified to Leadw, as per the worked example analysis.

# Use Lead data to explore getwd(), length() and dim()

getwd() # Answer depends on your directory structure.

length(Leadw$school)
dim(Leadw)
dim(Leadw)[1]
dim(Leadw)[2]


# Uses Lead data example to explore forcats etc

library(forcats)  # May not have been loaded automatically by a call to library(tidyverse)

table(Leadw$sex)
Leadw <- Leadw %>%
  mutate(sexnew = fct_relevel(sex, "Female"))
table(Leadw$sexnew)

table(Leadw$workmum)
Leadw <- Leadw %>%
  mutate(workmumnew = fct_recode(
    workmum,
    "No" = "No",
    "Yes" = "PT",
    "Yes" = "FT"
  ))
table(Leadw$workmumnew)



levels(Leadw$school)
table(Leadw$school)
Leadw <- Leadw %>%
  mutate(schoolnew = fct_collapse(
    school,
    Area1 = c("3", "4", "5", "6", "7", "8", "9", "10"),
    Area2 = c("11", "12", "13", "14", "15", "16"),
    Area3 = c("17", "18", "19", "20")
  ))
table(Leadw$schoolnew)
