## Load the tidyverse and NHANES packages
library(tidyverse)
library(NHANES)


## Using the same dataset as in this week's worked examples,
## we will explore some different variables.
data <- NHANES



#########################################################
### 1. Create a plot to explore the distribution of   ###
###    the Weight variable for individuals aged 18 or ###
###    over, separating male and female individuals.  ###
#########################################################
## Option 1
data %>% 
  filter(Age >= 18 & Gender == "female") %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Weight)) + 
    geom_histogram(aes(y = after_stat(density), fill = after_stat(count)), bins = 30) +
    geom_density(aes(y = after_stat(density)))

data %>% 
  filter(Age >= 18 & Gender == "male") %>% 
  drop_na(Weight) %>%
  ggplot(aes(x = Weight)) + 
    geom_histogram(aes(y = after_stat(density), fill = after_stat(count)),bins = 30) +
    geom_density(aes(y = after_stat(density)))


## Option 2
data %>% 
  filter(Age >= 18) %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Weight)) + 
    geom_histogram(aes(y = after_stat(density), fill = after_stat(count)), bins = 30) +
    geom_density(aes(y = after_stat(density))) +
    facet_grid(rows = vars(Gender))


## Bonus option
install.packages("cowplot")
library(cowplot)

plotFemale <- data %>%
  filter(Age >= 18 & Gender == "female") %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Weight)) + 
    geom_histogram(aes(y = after_stat(density), fill = after_stat(count)), bins = 30) +
    geom_density(aes(y = after_stat(density))) +
    theme_bw()

plotMale <- data %>% 
  filter(Age >= 18 & Gender == "male") %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Weight)) + 
    geom_histogram(aes(y = after_stat(density), fill = after_stat(count)), bins = 30) +
    geom_density(aes(y = after_stat(density))) +
    theme_bw()

plot_grid(plotFemale, plotMale, labels = "AUTO")


## Option 3
data %>% filter(Age >= 18) %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Weight, colour = Gender)) + 
    geom_freqpoly(binwidth = 1.4)


## Option 4 (my option, look at `Week 1.R`)
data %>%
  filter(Age >= 18) %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Gender, y = Weight, color = Gender)) +
    geom_boxplot() +
    scale_color_brewer(palette = "Dark2") +
    labs(title = 'Weight distribution for adult male and female individuals',
         y = 'Weight (kg)',x = 'Gender')



#########################################################
### 2. Create a plot to explore the covariation       ###
###    between the  BMI and BPSys1 variables for      ###
###   female individuals aged 50 and over,            ###
###    separating by Diabetes status.                 ###
#########################################################
## Option 1
data %>%
  filter(Age >= 50 & Gender == "female") %>%
  drop_na(Diabetes, BMI, BPSys1) %>%
  ggplot(aes(x = BMI, y = BPSys1, color = Diabetes)) +
    geom_point() + 
    geom_smooth(method = lm) +
    scale_color_brewer(palette = "Set3") + 
    xlab("BMI") + 
    ylab("Systolic blood pressure (mm Hg)")


## Option 2
data %>%
  filter(Age >= 50 & Gender == "female") %>%
  drop_na(Diabetes, BMI, BPSys1) %>%
  ggplot(aes(x = BMI, y = BPSys1)) +
    geom_point() + 
    geom_smooth(method = lm) +
    facet_grid(rows = vars(Diabetes)) +
    xlab("BMI") + 
    ylab("Systolic blood pressure (mm Hg)")



#########################################################
### 3. Create a tibble for the BMI, DirectChol, and   ###
###    HHIncomeMid variables for female individuals   ###
###    aged 50 and over, separating by Education.     ###
###    We want the mean and median for each variable. ###
#########################################################
data %>% 
  filter(Gender == "female", Age >= 50) %>%
  select(BMI, DirectChol, HHIncomeMid, Education) %>%
  drop_na(BMI, DirectChol, HHIncomeMid, Education) %>%
  group_by(Education) %>%
  summarise(meanBMI = mean(BMI), medianBMI = median(BMI),
            meanChol = mean(DirectChol), medianChol = median(DirectChol),
            meanHHIncome = mean(HHIncomeMid), medianHHIncome = median(HHIncomeMid))
