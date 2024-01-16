### Course "Data analysis with R"
### Week 2
## Libraries
pacman::p_load(tidyverse, here, NHANES)


## Loading data
data <- read_csv(here("raw_data", "walking_clinic_January.csv"))
