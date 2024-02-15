#### Standardisation Solution script

library(tidyverse)
library(epitools)

# Direct Standardisation

# Use 1985-87 as the standard population.
# Calculate standardised rates of prostate cancer incidence for 1994-96...

# Read in data if not already in R workspace
prostate <- read_csv(file = "Prostate Cancer.csv", col_names = T)

with(prostate,
     ageadjust.direct(
       count = `PC94-96`,
       pop = `Pop94-96`,
       stdpop = `Pop85-87`
     ))

# Adjusted rate of PC is 2.39 (95% CI 2.36, 2.42), rounding values to 2 decimal places.


# Indirect Standardisation

# All hospital ICUs used as standard, want SMR for hospital of interest.

icu <- read_csv(file = "ICU Data.csv", col_names = T)

with(
  icu,
  ageadjust.indirect(
    count = HospitalDeaths,
    pop = HospitalPopulation,
    stdcount = StandardDeaths,
    stdpop = StandardPopulation
  )
)

# 149 observed deaths, 130 expected. The SMR is 1.14 (95%CI 0.97, 1.34). So, the point estimate suggests a sightly greater
# mortality rate within the hospital of interest, but the data are compatible with equal mortality to the overall
# English population.
