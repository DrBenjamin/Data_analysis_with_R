# Standardisation example script
library(tidyverse)

# New package needed - epitools
install.packages("epitools")
library(epitools)

# Data from Kirkwood & Sterne (2010), Chapter 25


### Direct Standardisation
# Data from Table 25.2 are in the Prostate Cancer.csv file
# Represents numbers of cases of prostate cancer in France between 1979 and 1996
# in 3-year blocks (beginning, middle and end of this period), with associated 
# numbers of 1000s person years at risk.
# Use 1985-87 as the standard population.
# Calculate standardised rates of prostate cancer incidence for 1979-81...
prostate <- read_csv(file = "Prostate Cancer.csv", col_names = TRUE)
with(prostate, ageadjust.direct(count = `PC79-81`, pop = `Pop79-81`, stdpop = `Pop85-87`))
# The function uses the numbers of PC cases (count=) and the number of 1000 pyar (pop=)
# to calculate age-specific rates, then applies these to the standard population figures (stdpop=).
# Each age-stratum figure is added together to get the overall standardised rate, and a confidence
# interval is added. The crude (unstandardised) rate is also displayed.
# Note that the quote marks are needed for the variable names here because of the hyphen in each name:
# otherwise this would be read by R as something like "PC79 minus 81" as a piece of arithmetic...


### Indirect Standardisation
# Using the data from Table 25.5 in Kirkwood & Sterne (2010), which shows mortality in those with onchocerciasis.
# It is thought that those who have suffered blindness as a sequelae of this condition may be at greater risk of death.
# The data file (Oncho.csv) lists numbers of deaths and numbers of person-years at risk for the groups with and
# without blindness - the latter group is larger, with more deaths, so let's use this as the standard 
# population, and indirect standardise mortality in the smaller group with blindness to the larger group.
oncho <- read_csv(file = "Oncho.csv", col_names = TRUE)

with(oncho, ageadjust.indirect(count = DeathsBlind, pop = PYARBlind, stdcount = DeathsNon, stdpop = PYARNon))
# Here, the parameters represent deaths (count = ) and population sizes (pop = ), with an obvious nomenclature
# to distinguish between our study population of interest and the standard population from which we calculate
# age/ sex specific mortality rates.
#
# In the output, $sir represents the standardised rate information: the observed and expected numbers of 
# deaths in the Blind group (in this case - may sometimes be incidence, hence the SIR terminology), 
# then the SMR (listed as sir), then a confidence interval for SMR. The $rate line expresses the results 
# in terms of the crude and adjusted mortality rates within the Blind group (with a CI for the adjusted).