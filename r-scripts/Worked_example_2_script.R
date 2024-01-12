# Load the packages
library(tidyverse)
library(NHANES)

# View the NHANES dataset
data <- NHANES
View(data)

# Look at the first 6 rows of the NHANES dataset
head(data)

# Get an overview of the NHANES dataset using the Tidyverse
data %>%
  glimpse()

# Get summary statistics for the Age, Height and Weight variables
data %>% 
  select(Age, Height, Weight) %>%
  summary()

# Get summary statistics for the Gender variable
data %>% 
  select(Gender) %>%
  summary()

# Get summary statistics for the state.abb variable
state.abb %>% 
  summary()

# Remove missing values and get the mean and standard deviation of the Height 
# variable using the Tidyverse
data %>% 
  drop_na(Height) %>%
  summarize(meanHeight = mean(Height), stdevHeight = sd(Height))

# Get the mean and standard deviation of Height using the Tidyverse
data %>% 
  summarize(meanHeight = mean(Height, na.rm = T), stdevHeight = sd(Height, na.rm = T))

# Remove missing values and get the mean and standard deviation of the Height 
# variable split by Gender value using the Tidyverse
data %>% 
  drop_na(Height) %>%
  group_by(Gender) %>%
  summarize(meanHeight = mean(Height), stdevHeight = sd(Height))

# Remove missing values and get the minimum, maximum, 25th percentile, median, 
# 75th percentile, range and interquartile range of the Height variable split 
# by Gender value using the Tidyverse
data %>% 
  drop_na(Height) %>%
  group_by(Gender) %>%
  summarize(minHeight = min(Height),
            maxHeight = max(Height),
            q25Height = quantile(Height, probs = .25),
            medianHeight = quantile(Height, probs = .5),
            q75Height = quantile(Height, probs = .75),
            rangeHeight = maxHeight - minHeight,
            IQRheight = q75Height - q25Height)

# Get the counts for each value of the AgeDecade variable
table(data$AgeDecade)

# Using the Tidyverse, remove missing values and get the counts and proportions 
# for each value of the AgeDecade variable
data %>% 
  drop_na(AgeDecade) %>%
  count(AgeDecade) %>%
  mutate(prop = prop.table(n))

# Create a contingency table with counts for each combination of values from 
# the Gender and AgeDecade variables
table(data$Gender, data$AgeDecade)

# Using the Tidyverse, create a table with counts and proportions for each 
# combination of values from the Gender and AgeDecade variables
data %>% 
  drop_na(Gender, AgeDecade) %>%
  count(Gender, AgeDecade) %>%
  group_by(Gender) %>%
  mutate(prop = prop.table(n))

# Get the potential outliers using the method of "more than three standard 
# deviations from the mean"
data %>% 
  drop_na(Weight) %>%
  select(ID,Weight) %>%
  filter(Weight < mean(Weight) - 3 * sd(Weight) | Weight > mean(Weight) + 3 *sd(Weight))

# Save the value of the interquartile range into the IQR variable
IQR <- data %>% 
  drop_na(Weight) %>%
  summarize(value = quantile(Weight, probs = 0.75) - quantile(Weight, probs = 0.25)) %>%
  pull(value)
is.vector(IQR)
IQR

# Get the potential outliers using the method of "more than 1.5 IQR below the 
# 25th percentile or more than 1.5 IQR above the 75th percentile"
outliers <- data %>% 
  drop_na(Weight) %>%
  select(ID, Weight) %>%
  filter(Weight < quantile(Weight, probs = 0.25) - 1.5 * IQR | 
           Weight > quantile(Weight, probs = 0.75) + 1.5 * IQR)
outliers

# Empty values
data %>%
  summarize(countNA = sum(is.na(Weight)))

# Empty values per column
data %>%
  select(Age, Weight, Height, BMI, PhysActive, DirectChol, BMI_WHO, AgeDecade, Education, Gender) %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  filter(isna == TRUE) %>%
  select(num.isna)

data %>%
  select(Age, Weight, Height, BMI, PhysActive, DirectChol, BMI_WHO, AgeDecade, Education, Gender) %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n(), .groups = 'drop') %>%
  complete(key, isna, fill = list(total = 10000, num.isna = 0)) %>%
  filter(isna == TRUE) %>%
  select(key, total, num.isna)  
