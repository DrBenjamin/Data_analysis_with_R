## Script for loading in and eyeballing data
# Load required libraries
library(tidyverse)

# Reading in the data
data <- read_csv("walkin_clinic_January.csv")

# Eyeballing data
View(data)
head(data, n = 10)
str(data)
glimpse(data)
