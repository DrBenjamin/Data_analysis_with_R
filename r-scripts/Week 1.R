### Course "Data analysis with R"
### Week 1
## Libraries
pacman::p_load(tidyverse, NHANES)


## Loading data
data <- NHANES


## Data exploration
# Piped functions
esoph %>%
  filter(agegp == "65-74") %>%     # only keep rows where age group is 65 to 74
  filter(alcgp == "0-39g/day") %>% # only keep rows where alcohol consumption is 0 to 39 g per day
  select(-agegp) %>%               # remove the age group variable
  select(-alcgp)                   # remove the alcohol consumption variable

# Create a new dataset called iris_tibble containing the iris dataset
iris_tibble <- iris %>%
  # select the sepal and species variables
  select(Sepal.Length,         
         Sepal.Width,         
         Species) %>%   
  # keep only the observations for setosa and virginica  
  filter(Species == "setosa"| Species == "virginica")  

# Look at the resulting object
glimpse(iris_tibble)

# Make a scatterplot for the sepal width against the sepal length
iris_tibble %>%
  # set up the ggplot object axes
  ggplot(aes(x = Sepal.Length, y = Sepal.Width))+
  # add data points coloured and shaped by species
  geom_point(aes(color=Species, shape=Species)) +
  # add better x and y axes titles 
  labs(x = "Sepal Length", y = "Sepal Width") +
  # change to a different theme  
  theme_classic()

7 + 3

# assign the output of the sqrt() square root function with argument 4 to a vector called "sqrootfour"
sqrootfour <- sqrt(4)
# print the vector
sqrootfour
# calculate its length
length(sqrootfour)

# assign the output of the c() combine function with arguments 1 to 9 to a vector called "vecnine"
vecnine <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# you can use the : shortcut to get all integers between 1 and 9
vecnine <- 1:9
# print the vector
vecnine
# calculate its length
length(vecnine)

# create a 3x3 matrix called "matnine" storing the elements of the vecnine vector
# by using the matrix() function with arguments nrow and ncol to set the number
# of rows and the number of columns, respectively
matnine <- matrix(vecnine, nrow = 3, ncol = 3)
# calculate the size of the matrix
dim(matnine)
# print the matrix
 matnine

# assign the output of the c() function with arguments "one" to "nine" to a vector called "ninewords"
ninewords <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
# print the vector
ninewords

# create a base R data frame called "ninedf" with vecnine and ninewords as variables
# by using the data.frame() function
ninedf <- data.frame(vecnine, ninewords)
# calculate the dimension of the data frame
dim(ninedf)
# print the data frame
ninedf

# create a tidyverse (dplyr) tibble called ninetib with vecnine and ninewords as variables
ninetib <- tibble(vecnine = vecnine, ninewords = ninewords)
# calculate the dimension of the tibble
dim(ninetib)
# print the tibble
ninetib



################
## Exercises ##
################
## Exercise 1
# Exercise: "Use the code chunk below to calculate what 50 minus 8 is"
50 - 8


## Exercise 2
# Exercise: "Create 3 new variables,  a,  b,  c with values 6, 7, 8 using the 
# different assignment operators, and print the results"
a <- 6
b <- 7
c <- 8
a
b
c

## Exercise 3
# Exercise: "Carry out a calculation with saved variables:"
# - Create 2 new variables,  x ,  y , with values 3, 10.
# - Carry out a calculation using these variables (e.g.  x+y ).
# - Save the value to an object called  calc.
# - Print out the value of the variable to view the output.
x <- 3
y <- 10
x + y
calc <- x + y
calc


## Exercise 4: 
## "Exploratory data analysis (graphical)"
## Using the same dataset (NHANES) as in this week's worked examples, we will explore some different variables. 
## Practise using R using the suggestions below, and pick one of them to write into a short post in the 
## discussion board, including a short explanation, the R code and the output.
# 1. Create a plot to explore the distribution of the Weight variable for individuals aged 18 or over, 
#    separating male and female individuals.
# 2. Create a plot to explore the covariation between the BMI and BPSys1 variables for female individuals 
#    aged 50 and over, separating by Diabetes status.
# 3. For any of the code you've written this week, write down the code you ran and an error message you got, 
#    and how you fixed it (if you managed to).

# Exercise 4.1
# Creating a boxplot which shows the distribution of the Weight variable for individuals aged 18 or over,
# grouped by Gender
data %>%
  filter(Age >= 18) %>%
  ggplot(aes(x = Gender, y = Weight, color = Gender)) +
    geom_boxplot()

# Exercise 4.2
# Creating a scatterplot which shows the distribution of BPSys1 (blood pressure),
# by BMI (body mass index) grouped by Diabetes status (Yes or No))
data %>%
  filter(Age <= 50 & Gender == "female") %>%
  drop_na(BMI, BPSys1, Diabetes) %>%
  ggplot(aes(x = BMI, y = BPSys1, color = Diabetes)) +
    geom_point()

# Exercise 4.3
# Not clear to me what this exercise is asking for.


## Exercises 5:
## "Exploratory data analysis (numerical)"
## Using the same dataset as in this week's worked examples, we will explore some different variables. 
## Practise using R using the suggestions below, and pick one of them to write into a short post in the 
## discussion board, including a short explanation, the R code and the output.
# 1. Create a tibble for the BMI, DirectChol, and HHIncomeMid variables for female individuals aged 50 and over, 
#    separating by Education. We want the mean and median for each variable.
# 2. For any of the code you've written this week, write down the code you ran and an error message you got, 
#    and how you fixed it (if you managed to).

# Exercise 5.1
# Creating a tibble with the described parameters
data %>%
  filter(Gender == "female" & Age >= 50) %>%
  drop_na(BMI, DirectChol, HHIncomeMid, Education) %>%
  group_by(Education) %>%
  mutate(BMI_mean = mean(BMI),
         BMI_median = median(BMI),
         DirectChol_mean = mean(DirectChol),
         DirectChol_median = median(DirectChol),
         HHIncomeMid_mean = mean(HHIncomeMid),
         HHIncomeMid_median = median(HHIncomeMid)) %>%
  select(BMI_mean, BMI_median, DirectChol_mean, DirectChol_median, HHIncomeMid_mean, HHIncomeMid_median, Education) %>%
  group_by(Education, BMI_mean, BMI_median, DirectChol_mean, DirectChol_median, HHIncomeMid_mean, HHIncomeMid_median) %>%
  summarize()

# Exercise 5.2
# Not clear to me what this exercise is asking for.
