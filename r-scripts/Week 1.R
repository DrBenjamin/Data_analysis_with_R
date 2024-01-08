### Course "Data analysis with R"
### Week 1
## Libraries
pacman::p_load(tidyverse)

## Loading data
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
