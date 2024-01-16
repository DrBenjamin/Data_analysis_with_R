### Course "Data analysis with R"
### Week 2
## Libraries
pacman::p_load(tidyverse, finalfit, gtsummary, here, NHANES)


## Loading data
data_csv <- read_csv(here("raw_data", "walkin_clinic_January.csv"))
data <- NHANES


## Data wrangling for BMI_WHO and PhysActive
# Create tibble,
# dropping missing data rows and selecting columns
data_tibble <- data %>%
  drop_na(BMI_WHO, PhysActive) %>%
  select(BMI_WHO, PhysActive) %>%
  group_by(BMI_WHO, PhysActive)

# Option 1 (table)
data_tibble %>%
  table()

# Option 2 (dplyr count & spread methods)
data_tibble %>%
  count(BMI_WHO, PhysActive) %>%
  spread(PhysActive, n)

# Option 3 (summary_factorlist method)
data_tibble %>%
  summary_factorlist(dependent = "BMI_WHO",
                     explanatory = "PhysActive",
                     column = F,
                     total_col = T)

# Option 4 (tbl_cross method)
data_tibble %>%
  tbl_cross(row = BMI_WHO, col = PhysActive, percent = "row", margin = c("row", "col")) %>%
  bold_labels()


## Data wrangling for Gender, PhysActive and BMI
# Create tibble,
# 30-39 yrs, education = college grad, BMI = 18.5-24.9
data_tibble <- data %>%
  drop_na(Gender, PhysActive) %>%
  filter(AgeDecade == " 30-39", Education == "College Grad", BMI_WHO == "18.5_to_24.9") %>%
  select(Gender, PhysActive)

# Option 1 (dplyr count & spread methods - same as pivot_wider)
data_tibble %>%
  count(Gender, PhysActive) %>%
  #spread(PhysActive, n)
  pivot_wider(names_from = PhysActive, values_from = n)

# Option 2 (tbl_cross method)
data_tibble %>%
  tbl_cross(row = Gender, col = PhysActive, percent = "row") %>%
  bold_labels()
